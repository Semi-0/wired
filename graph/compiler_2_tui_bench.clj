(ns graph.compiler-2-tui-bench
  "Small benchmark for wired TUI trace rebuild/poll/render paths."
  (:require [clojure.string :as str]
            [graph.compiler-2-runtime-server :as server]
            [propagators.compiler-2.runtime :as runtime]
            [propagators.compiler-2.runtime.session.input :as runtime-input]
            [graph.compiler-2-tui :as tui]
            [propagators.cells.cell-protocol :as cell-protocol]
            [propagators.compiler-2.model.env :as cenv]
            [propagators.generic-procedure :as generic]))

(def simple-trace-commands
  [{:op :tui/register :client-id "A"}
   {:op :tui/append-block :client-id "A" :text "(def a)"}
   {:op :tui/append-block :client-id "A" :text "(<-> 2 a)"}
   {:op :tui/append-block :client-id "A" :text "(def out)"}
   {:op :tui/append-block :client-id "A" :text "(-> (+ 1 a) out)"}
   {:op :tui/append-block :client-id "A" :text "(def g)"}
   {:op :tui/append-block :client-id "A" :text "(trace out g)"}])

(defn- elapsed-ms
  [f]
  (let [t (System/nanoTime)]
    (f)
    (/ (double (- (System/nanoTime) t)) 1000000.0)))

(defn- samples
  [n f]
  (doall (repeatedly n #(elapsed-ms f))))

(defn- percentile
  [p xs]
  (nth (vec (sort xs))
       (dec (int (Math/ceil (* p (count xs)))))))

(defn- summary
  [xs]
  {:avg-ms (/ (reduce + xs) (count xs))
   :p95-ms (percentile 0.95 xs)})

(defn- request!
  [port command]
  (server/request server/default-host port command))

(defn- timed-request!
  [port command]
  (let [t (System/nanoTime)
        response (request! port command)]
    [(keyword (or (:text command) (name (:op command))))
     (/ (double (- (System/nanoTime) t)) 1000000.0)
     response]))

(defn- read-view-for-client
  [port client-id]
  (:result (request! port {:op :tui/read-view :client-id client-id})))

(defn- read-view
  [port]
  (read-view-for-client port "A"))

(defn- bench
  []
  (let [{:keys [port close]} (server/start-server 0)]
    (try
      (let [steps (mapv #(timed-request! port %) simple-trace-commands)]
      (let [view (read-view port)
              model (tui/init-model {:client-id "A" :port port})
            state (first (tui/update-fn model
                                        {:type :runtime/view
                                         :response {:ok true
                                                    :result view}}))]
          {:append-steps (mapv (fn [[label elapsed response]]
                                 {:step label
                                  :ms elapsed
                                  :ok (:ok response)})
                               steps)
           :view {:blocks (count (:blocks view))
                  :bytes (count (.getBytes (pr-str view) "UTF-8"))}
         :socket-read-view (summary (samples 20 #(read-view port)))
         :unchanged-update (summary
                            (samples 20
                                     #(tui/update-fn
                                       state
                                       {:type :runtime/view
                                        :response {:ok true
                                                   :result view}})))
         :raw-render (summary
                      (samples 5 #(tui/render-view view
                                                   {:width 120
                                                      :height 30})))}))
      (finally
        (close)))))

(defn- runtime-append!
  [session text]
  (runtime/append-tui-block! session {:client-id "A" :text text}))

(defn- runtime-append-source!
  [session text]
  (runtime/append-tui-block! session {:client-id "A"
                                      :text text
                                      :rebuild? false}))

(defn- graph-size
  [session]
  {:nodes (count (:nodes (:graph @session)))
   :edges (count (:edges (:graph @session)))})

(defn- cell-id
  [session label]
  (let [state @session]
    (cenv/resolve-binding-id (:program/net state)
                             (:program/env state)
                             (symbol label))))

(defn- parse-count
  [s default]
  (if s
    (Long/parseLong s)
    default))

(defn- client-id
  [i]
  (str "c" i))

(defn- value-symbol
  [client-index value-index]
  (str (client-id client-index) "x" value-index))

(defn- value-source
  [client-index value-index]
  (if (zero? value-index)
    (format "(def %s 0)" (value-symbol client-index 0))
    (format "(-> (+ %d %s) %s)"
            value-index
            (value-symbol client-index (dec value-index))
            (value-symbol client-index value-index))))

(defn- watcher-source
  [client-index watcher-index target-index]
  (format "(-> %s (be:block %d))"
          (value-symbol client-index watcher-index)
          target-index))

(defn- trace-cell-symbol
  [i]
  (str "x" i))

(defn- trace-additive-source
  [i]
  (format "(let-cell [u%d] (-> (+ u%d %d) %s))"
          i
          i
          i
          (trace-cell-symbol (mod i 5))))

(declare trace-completions wait-for-trace-completions! xr-effect-read-ms)

(def behavior-graph-channels
  ["a" "b" "c" "d" "e"])

(def behavior-graph-terms
  ["a" "b" "c" "d" "e" "a" "b" "c" "d" "e"])

(defn- behavior-graph-expr
  [op]
  (reduce (fn [acc term]
            (format "(%s %s %s)" op acc term))
          (first behavior-graph-terms)
          (rest behavior-graph-terms)))

(defn- behavior-graph-sources
  [op]
  [(format "(define-behaviors %s)"
           (str/join " " behavior-graph-channels))
   "(def out)"
   "(def g)"
   (format "(io:slider-panel %s)"
           (str/join " " behavior-graph-channels))
   (format "(-> %s out)" (behavior-graph-expr op))
   "(-> out (be:block 20))"
   "(trace out g)"
   "(io:xr g)"])

(defn- behavior-graph-value
  [latest]
  (reduce + (map #(get latest % 0) behavior-graph-terms)))

(defn- output-block-value
  [session]
  (get-in (runtime/read-tui-view @session {:client-id "A"})
          [:blocks 20 :value]))

(defn- behavior-graph-update!
  [session channel value]
  (runtime/commit-runtime-input!
   session
   {:runtime/input :xr/widget-event
    :widget-id "slider-panel-0"
    :channel channel
    :value value}))

(def slider-cache-profile-events
  (vec
   (map-indexed
    (fn [i channel]
      {:channel channel
       :value (+ 10 i)})
    (take 60 (cycle ["a" "b" "c"])))))

(defn- slider-cache-sources
  [full?]
  (cond-> ["(def-cells a b c d)"
           "(-> (- (+ a c) b) d)"
           "(io:slider-panels a b c)"]
    full?
    (into ["(def-cell g)"
           "(trace d g)"
           "(io:xr g)"
           "(-> d (block 7))"])))

(defn- runtime-widget-update!
  [session {:keys [channel value]}]
  (runtime/commit-runtime-input!
   session
   {:runtime/input :xr/widget-event
    :widget-id "slider-panel-0"
    :channel channel
    :value value}))

(defn- prefer-generic-protocol-session!
  [session]
  (swap! session update :program/net cell-protocol/prefer-generic-standard-protocols)
  nil)

(defn- with-slider-cache-session
  [{:keys [full? generic-protocol? retained-generic?]} f]
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (let [setup-ms (elapsed-ms
                    #(doseq [source (slider-cache-sources full?)]
                       (runtime-append! session source)))]
      (when generic-protocol?
        (prefer-generic-protocol-session! session))
      (if retained-generic?
        (generic/with-retained-apply-generic-values
          (f session setup-ms))
        (f session setup-ms)))))

(defn- summarize-ms
  [xs]
  (assoc (summary xs)
         :max-ms (apply max xs)
         :total-ms (reduce + xs)
         :count (count xs)))

(defn- slider-cache-profile-variant
  [{:keys [variant full? retained-generic?] :as options}]
  (with-slider-cache-session
    options
    (fn [session setup-ms]
      (let [rows (mapv (fn [event]
                         (assoc event
                                :ms (elapsed-ms
                                     #(runtime-widget-update! session event))))
                       slider-cache-profile-events)
            ms (mapv :ms rows)
            cell-d (runtime/read-cell @session {:label "d"})
            view (when full?
                   (runtime/read-tui-view @session {:client-id "A"}))]
        {:variant variant
         :setup-ms setup-ms
         :updates (summarize-ms ms)
         :first-update-ms (:ms (first rows))
         :last-update-ms (:ms (last rows))
         :cache-stats (:runtime/network-cache-stats @session)
         :retained-generic-stats (when retained-generic?
                                   (generic/retained-apply-stats))
         :graph (graph-size session)
         :final {:d (:strongest cell-d)
                 :block-7 (when full? (get-in view [:blocks 7 :value]))}}))))

(defn- slider-cache-profile-bench
  []
  {:variants [(slider-cache-profile-variant
               {:variant :direct-standard-plain-slider-arithmetic
                :full? false})
              (slider-cache-profile-variant
               {:variant :generic-rebuild-plain-slider-arithmetic
                :full? false
                :generic-protocol? true})
              (slider-cache-profile-variant
               {:variant :generic-retained-plain-slider-arithmetic
                :full? false
                :generic-protocol? true
                :retained-generic? true})
              (slider-cache-profile-variant
               {:variant :direct-standard-trace-xr-block-slider-arithmetic
                :full? true})]})

(defn- wait-for-behavior-graph-traces!
  [session target timeout-ms]
  (let [started (System/nanoTime)
        deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (>= (trace-completions session) target)
        {:ms (/ (double (- (System/nanoTime) started)) 1000000.0)
         :timed-out? false}

        (> (System/currentTimeMillis) deadline)
        {:ms (/ (double (- (System/nanoTime) started)) 1000000.0)
         :timed-out? true}

        :else
        (do
          (Thread/sleep 1)
          (recur))))))

(defn- behavior-graph-variant
  [{:keys [variant op updates trace-timeout-ms]}]
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (try
      (let [setup-ms (elapsed-ms
                      #(doseq [source (behavior-graph-sources op)]
                         (runtime/append-tui-block!
                          session
                          {:client-id "A"
                           :text source})))
            warmup-ms (elapsed-ms
                       #(doseq [channel behavior-graph-channels]
                          (behavior-graph-update! session channel 0)))
            fallback-before (long (or (:runtime/full-rebuild-fallbacks @session)
                                      0))
            latest (atom (zipmap behavior-graph-channels (repeat 0)))
            rows (mapv
                  (fn [i]
                    (let [channel (nth behavior-graph-channels
                                       (mod i (count behavior-graph-channels)))
                          value (inc i)
                          _ (swap! latest assoc channel value)
                          before (trace-completions session)
                          update-ms (elapsed-ms
                                     #(behavior-graph-update!
                                       session
                                       channel
                                       value))
                          scheduled (runtime/schedule-trace-refreshes! session)
                          trace-result (when (pos? scheduled)
                                         (wait-for-behavior-graph-traces!
                                          session
                                          (+ before scheduled)
                                          trace-timeout-ms))
                          tui-read-ms (elapsed-ms
                                       #(runtime/read-tui-view
                                         @session
                                         {:client-id "A"}))
                          xr-read-ms (xr-effect-read-ms session)]
                      {:index i
                       :channel channel
                       :value value
                       :update-ms update-ms
                       :scheduled-traces scheduled
                       :trace-refresh-ms (:ms trace-result)
                       :trace-refresh-timeout? (:timed-out? trace-result)
                       :tui-read-ms tui-read-ms
                       :xr-effect-read-ms xr-read-ms}))
                  (range updates))
            update-ms (mapv :update-ms rows)
            tui-read-ms (mapv :tui-read-ms rows)
            xr-read-ms (mapv :xr-effect-read-ms rows)
            trace-ms (vec (keep :trace-refresh-ms rows))
            expected (behavior-graph-value @latest)
            actual (output-block-value session)]
        {:variant variant
         :op op
         :update-count updates
         :setup-ms setup-ms
         :warmup-ms warmup-ms
         :first-update-ms (:update-ms (first rows))
         :updates (assoc (summary update-ms)
                         :max-ms (apply max update-ms)
                         :count (count update-ms))
         :tui-read-view (assoc (summary tui-read-ms) :count (count tui-read-ms))
         :xr-effect-read (assoc (summary xr-read-ms) :count (count xr-read-ms))
         :trace-refresh (when (seq trace-ms)
                          (assoc (summary trace-ms) :count (count trace-ms)))
         :trace {:subscriptions (count (:trace/subscriptions @session))
                 :published (long (or (:trace/published-results @session) 0))
                 :stale-dropped (long (or (:trace/stale-results @session) 0))
                 :unchanged-dropped (long (or (:trace/unchanged-results @session)
                                              0))
                 :refresh-timeouts (count (filter :trace-refresh-timeout?
                                                  rows))}
         :fallback-count (- (long (or (:runtime/full-rebuild-fallbacks @session)
                                      0))
                            fallback-before)
         :graph (graph-size session)
         :final {:expected expected
                 :actual actual
                 :correct? (= expected actual)}})
      (catch Throwable t
        {:variant variant
         :op op
         :error (.getMessage t)
         :data (ex-data t)}))))

(defn- trace-completions
  [session]
  (+ (long (or (:trace/published-results @session) 0))
     (long (or (:trace/stale-results @session) 0))
     (long (or (:trace/unchanged-results @session) 0))))

(defn- wait-for-trace-completions!
  [session target]
  (let [started (System/nanoTime)
        deadline (+ (System/currentTimeMillis) 5000)]
    (loop []
      (if (>= (trace-completions session) target)
        (/ (double (- (System/nanoTime) started)) 1000000.0)
        (if (> (System/currentTimeMillis) deadline)
          (throw (ex-info "timed out waiting for trace subscriptions"
                          {:target target
                           :completed (trace-completions session)}))
          (do
            (Thread/sleep 1)
            (recur)))))))

(defn- xr-effect-read-ms
  [session]
  (elapsed-ms #(runtime/read-xr-effects @session)))

(defn- trace-subscriptions-bench
  []
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (let [setup-ms
          (elapsed-ms
           #(do
              (doseq [i (range 5)]
                (runtime-append! session
                                 (format "(def %s)" (trace-cell-symbol i))))
              (doseq [i (range 5)]
                (let [before (trace-completions session)]
                  (runtime-append!
                   session
                   (format "(let-cell [g] (trace %s g) (io:xr g))"
                           (trace-cell-symbol i)))
                  (let [scheduled (runtime/schedule-trace-refreshes! session)]
                    (wait-for-trace-completions!
                     session
                     (+ before scheduled)))))))
          rows
          (mapv
           (fn [i]
             (let [before (trace-completions session)
                   append-ms (elapsed-ms
                              #(runtime-append!
                                session
                                (trace-additive-source i)))
                   scheduled (runtime/schedule-trace-refreshes! session)
                   trace-refresh-ms (wait-for-trace-completions!
                                     session
                                     (+ before scheduled))
                   read-ms (xr-effect-read-ms session)]
               {:index i
                :target (trace-cell-symbol (mod i 5))
                :append-ms append-ms
                :trace-refresh-ms trace-refresh-ms
                :xr-effect-read-ms read-ms}))
           (range 10))
          append-ms (mapv :append-ms rows)
          trace-ms (vec (keep :trace-refresh-ms rows))
          read-ms (mapv :xr-effect-read-ms rows)]
      {:variant :effectful-trace
       :setup-ms setup-ms
       :append (assoc (summary append-ms) :count (count append-ms))
       :trace-refresh (when (seq trace-ms)
                        (assoc (summary trace-ms) :count (count trace-ms)))
       :xr-effect-read (assoc (summary read-ms) :count (count read-ms))
       :rows rows
       :trace {:subscriptions (count (:trace/subscriptions @session))
               :published (long (or (:trace/published-results @session) 0))
               :stale-dropped (long (or (:trace/stale-results @session) 0))
               :unchanged-dropped (long (or (:trace/unchanged-results @session)
                                            0))}
       :effects (count (get-in @session [:xr :effects]))
       :graph (graph-size session)})))

(defn- request-ms!
  [port command]
  (let [[_ ms response] (timed-request! port command)]
    (when-not (:ok response)
      (throw (ex-info "benchmark request failed"
                      {:command command
                       :response response})))
    ms))

(defn- append-block-ms!
  [port client-id text]
  (request-ms! port
               (cond-> {:op :tui/append-block
                        :client-id client-id}
                 text (assoc :text text))))

(defn- multi-client-bench
  [{:keys [clients blocks watchers]
    :or {clients 4 blocks 10 watchers 10}}]
  (let [{:keys [port close]} (server/start-server 0)]
    (try
      (let [client-indexes (range clients)
            register-ms (mapv #(request-ms!
                                port
                                {:op :tui/register
                                 :client-id (client-id %)})
                              client-indexes)
            block-ms (mapv (fn [client-index]
                             (mapv #(append-block-ms!
                                     port
                                     (client-id client-index)
                                     (value-source client-index %))
                                   (range blocks)))
                           client-indexes)
            target-ms (mapv (fn [client-index]
                              (mapv (fn [_]
                                      (request-ms!
                                       port
                                       {:op :tui/append-block
                                        :client-id (client-id client-index)
                                        :rebuild? false}))
                                    (range watchers)))
                            client-indexes)
            watcher-ms (mapv (fn [client-index]
                               (mapv #(append-block-ms!
                                       port
                                       (client-id client-index)
                                       (watcher-source client-index
                                                       (mod % blocks)
                                                       (+ blocks %)))
                                     (range watchers)))
                             client-indexes)
            read-ms (mapv (fn [client-index]
                            (samples 3
                                     #(request!
                                       port
                                       {:op :tui/read-view
                                        :client-id (client-id client-index)})))
                          client-indexes)
            views (mapv #(read-view-for-client port (client-id %))
                        client-indexes)
            all-block-ms (vec (mapcat identity block-ms))
            all-target-ms (vec (mapcat identity target-ms))
            all-watcher-ms (vec (mapcat identity watcher-ms))
            all-read-ms (vec (mapcat identity read-ms))
            graph (:result (request! port {:op :semantic/graph}))]
        {:clients clients
         :blocks-per-client blocks
         :watchers-per-client watchers
         :requests {:register (summary register-ms)
                    :source-block-append (summary all-block-ms)
                    :target-block-append (summary all-target-ms)
                    :watcher-append (summary all-watcher-ms)
                    :read-view (summary all-read-ms)}
         :totals-ms {:register (reduce + register-ms)
                     :source-block-append (reduce + all-block-ms)
                     :target-block-append (reduce + all-target-ms)
                     :watcher-append (reduce + all-watcher-ms)
                     :read-view (reduce + all-read-ms)}
         :view {:blocks-per-client (mapv (comp count :blocks) views)
                :bytes-per-client (mapv #(count (.getBytes (pr-str %) "UTF-8"))
                                        views)}
         :graph {:nodes (count (:nodes graph))
                 :edges (count (:edges graph))}})
      (finally
        (close)))))

(defn- profiled-step!
  [session port rows phase client-index item command]
  (let [started (System/nanoTime)
        response (request! port command)
        ms (/ (double (- (System/nanoTime) started)) 1000000.0)
        profile (:runtime/last-incremental-profile @session)
        row (cond-> {:phase phase
                     :client (when client-index (client-id client-index))
                     :item item
                     :ms ms
                     :ok (:ok response)}
              (seq profile) (assoc :runtime-profile profile))]
    (swap! rows conj row)
    (when-not (:ok response)
      (throw (ex-info "benchmark request failed"
                      {:command command
                       :response response})))
    row))

(defn- slowest-rows
  [rows n]
  (->> rows
       (sort-by :ms >)
       (take n)
       vec))

(defn- phase-summary
  [rows phase]
  (let [xs (mapv :ms (filter #(= phase (:phase %)) rows))]
    (when (seq xs)
      (assoc (summary xs) :count (count xs)))))

(defn- multi-client-profile-bench
  [{:keys [clients blocks watchers slow-ms max-ms]
    :or {clients 4 blocks 10 watchers 10 slow-ms 5000 max-ms 60000}}]
  (let [{:keys [port close session]} (server/start-server 0)
        rows (atom [])
        started (System/nanoTime)]
    (try
      (letfn [(elapsed-total-ms []
                (/ (double (- (System/nanoTime) started)) 1000000.0))
              (stopped? [row]
                (or (> (:ms row) slow-ms)
                    (> (elapsed-total-ms) max-ms)))
              (finish [reason]
                (let [rows* @rows]
                  {:clients clients
                   :blocks-per-client blocks
                   :watchers-per-client watchers
                   :stopped reason
                   :elapsed-ms (elapsed-total-ms)
                   :phases {:register (phase-summary rows* :register)
                            :source-block-append (phase-summary rows* :source-block)
                            :target-block-append (phase-summary rows* :target-block)
                            :watcher-append (phase-summary rows* :watcher)
                            :read-view (phase-summary rows* :read-view)}
                   :slowest (slowest-rows rows* 10)
                   :completed-requests (count rows*)}))
              (step! [phase client-index item command]
                (let [row (profiled-step! session
                                          port
                                          rows
                                          phase
                                          client-index
                                          item
                                          command)]
                  (when (stopped? row)
                    (throw (ex-info "profile stopped"
                                    {:reason (if (> (:ms row) slow-ms)
                                               :slow-request
                                               :time-budget)})))
                  row))]
        (try
          (doseq [client-index (range clients)]
            (step! :register client-index nil
                   {:op :tui/register
                    :client-id (client-id client-index)}))
          (doseq [client-index (range clients)
                  block-index (range blocks)]
            (step! :source-block client-index block-index
                   {:op :tui/append-block
                    :client-id (client-id client-index)
                    :text (value-source client-index block-index)}))
          (doseq [client-index (range clients)
                  target-index (range watchers)]
            (step! :target-block client-index target-index
                   {:op :tui/append-block
                    :client-id (client-id client-index)
                    :rebuild? false}))
          (doseq [client-index (range clients)
                  watcher-index (range watchers)]
            (step! :watcher client-index watcher-index
                   {:op :tui/append-block
                    :client-id (client-id client-index)
                    :text (watcher-source client-index
                                          (mod watcher-index blocks)
                                          (+ blocks watcher-index))}))
          (doseq [client-index (range clients)]
            (step! :read-view client-index nil
                   {:op :tui/read-view
                    :client-id (client-id client-index)}))
          (finish nil)
          (catch clojure.lang.ExceptionInfo e
            (finish (or (:reason (ex-data e)) :error)))))
      (finally
        (close)))))

(defn- large-trace-bench
  []
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime-append-source! session "(def x0)")
    (runtime-append-source! session "(<-> 0 x0)")
    (doseq [i (range 1 101)]
      (runtime-append-source! session
                              (format "(-> (+ %d x%d) x%d)"
                                      i
                                      (dec i)
                                      i)))
    (let [setup-ms (elapsed-ms #(runtime-input/rebuild-program! session))
          trace-ms (elapsed-ms
                    #(runtime-append!
                      session
                      "(let-cell [g] (trace x100 g) (io:xr g))"))
          watcher-ms (elapsed-ms
                      #(runtime-append!
                        session
                        "(-> x50 (be:block 120))"))]
      {:setup-rebuild-ms setup-ms
       :trace-ms trace-ms
       :watcher-ms watcher-ms
       :blocks (count (get-in @session [:tuis "A" :blocks]))
       :graph (graph-size session)})))

(defn- incremental-trace-bench
  []
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime-append-source! session "(def x0)")
    (doseq [i (range 1 101)]
      (runtime-append-source! session
                              (format "(-> (+ %d x%d) x%d)"
                                      i
                                      (dec i)
                                      i)))
    (let [setup-ms (elapsed-ms #(runtime-input/rebuild-program! session))
          fallback-before (:runtime/full-rebuild-fallbacks @session)
          watcher-ms (elapsed-ms
                      #(runtime-append!
                        session
                        "(-> x50 (be:block 120))"))
          update-ms (elapsed-ms
                     #(runtime/commit-runtime-input!
                       session
                       {:runtime/input :cell-message
                        :cell-id (cell-id session "x0")
                        :update 1}))
          edit-ms (elapsed-ms
                   #(runtime/edit-tui-block!
                     session
                     {:client-id "A"
                      :index 101
                      :text "(-> x40 (be:block 120))"}))
          forward-session (runtime/new-session)
          _ (runtime/register-tui! forward-session {:client-id "A"})
          _ (runtime-append! forward-session
                             "(let-cell [out] (later 4 out) (block-at % 1 out) out)")
          _ (runtime/append-tui-block! forward-session {:client-id "A"})
          forward-ms (elapsed-ms
                      #(runtime-append!
                        forward-session
                        "(def-net later [x] [out] (<-> (+ x 1) out))"))]
      {:setup-rebuild-ms setup-ms
       :incremental-watcher-ms watcher-ms
       :post-install-update-ms update-ms
       :edit-watcher-ms edit-ms
       :forward-ref-repair-ms forward-ms
       :target-value (get-in (runtime/read-tui-view @session {:client-id "A"})
                             [:blocks 120 :value])
       :forward-ref-value (get-in (runtime/read-tui-view @forward-session
                                                         {:client-id "A"})
                                  [:blocks 1 :value])
       :fallback-count (- (long (:runtime/full-rebuild-fallbacks @session))
                          (long fallback-before))
       :forward-ref-fallback-count (:runtime/full-rebuild-fallbacks
                                    @forward-session)
       :incremental-installs (:runtime/incremental-installs @session)
       :blocks (count (get-in @session [:tuis "A" :blocks]))
       :graph (graph-size session)})))

(defn- behavior-graph-bench
  [{:keys [updates trace-timeout-ms]
    :or {updates 5 trace-timeout-ms 50}}]
  {:variants [(behavior-graph-variant
               {:variant :legacy-default-arithmetic
                :op "+"
                :updates updates
                :trace-timeout-ms trace-timeout-ms})
              (behavior-graph-variant
               {:variant :explicit-be-arithmetic
                :op "be:+"
                :updates updates
                :trace-timeout-ms trace-timeout-ms})]})

(defn -main
  [& args]
  (prn (case (first args)
         "large" (large-trace-bench)
         "incremental" (incremental-trace-bench)
         "trace-subscriptions" (trace-subscriptions-bench)
         "slider-cache-profile" (slider-cache-profile-bench)
         "behavior-graph" (behavior-graph-bench
                           {:updates (parse-count (second args) 5)
                            :trace-timeout-ms (parse-count (nth args 2 nil)
                                                           50)})
         "multi-client" (multi-client-bench
                         {:clients (parse-count (second args) 4)
                          :blocks (parse-count (nth args 2 nil) 10)
                          :watchers (parse-count (nth args 3 nil) 10)})
         "multi-client-profile" (multi-client-profile-bench
                                 {:clients (parse-count (second args) 4)
                                  :blocks (parse-count (nth args 2 nil) 10)
                                  :watchers (parse-count (nth args 3 nil) 10)
                                  :slow-ms (parse-count (nth args 4 nil) 5000)
                                  :max-ms (parse-count (nth args 5 nil) 60000)})
         (bench))))
