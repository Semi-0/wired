(ns graph.compiler-2-runtime
  "Shared compiler-2 runtime session for socket clients."
  (:require [clojure.edn :as edn]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [graph.vijual-compiler-2-demo :as demo]
            [propagators.cells.cell :as cell]
            [propagators.cells.value :as value]
            [propagators.compiler-2.env :as cenv]
            [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.compiler-2.main :as compiler]
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.semantic-trace :as semantic-trace]
            [propagators.stdlib.prop :as stdlib-prop])
  (:import [java.util.concurrent Executors TimeUnit]))

(defn new-session []
  (atom nil))

(defn- stable-node-id
  [& parts]
  (ids/->NodeId
   (java.util.UUID/nameUUIDFromBytes
    (.getBytes (pr-str (into [:compiler-2-runtime] parts)) "UTF-8"))))

(defn- daemon-executor
  [name]
  (Executors/newSingleThreadScheduledExecutor
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r]
       (doto (Thread. ^Runnable r name)
         (.setDaemon true))))))

(defn- compiled-state
  [source]
  (let [[_compiled-stage expanded-stage] (demo/compiled-progression source)
        compiled (:compiled expanded-stage)
        network (:network expanded-stage)
        graph (assoc (semantic-repl/compiled-semantic-graph compiled network)
                     :source source)]
    {:source source
     :compiled compiled
     :compiled-network network
     :network net/empty-net
     :program/net network
     :program/env (:env compiled)
     :program/graph graph
     :program/results {}
     :program/epoch 0
     :block-order []
     :next-order 0
     :tuis {}
     :graph graph}))

(defn- empty-state []
  {:network net/empty-net
   :program/net net/empty-net
   :program/env (compiler-helpers/default-env)
   :program/graph {:nodes {} :edges [] :values {}}
   :program/results {}
   :program/epoch 0
   :block-order []
   :next-order 0
   :traces {}
   :tuis {}})

(defn- ensure-session-state! [session]
  (when-not @session
    (reset! session (empty-state)))
  @session)

(defn compile-source!
  [session source]
  (doseq [{:keys [stop]} (vals (:traces @session))]
    (when stop (stop)))
  (let [state (assoc (compiled-state source) :traces {})]
    (reset! session state)
    (:graph state)))

(defn- require-state
  [state]
  (when-not state
    (throw (ex-info "no compiled source in runtime session" {})))
  state)

(defn- labels
  [state]
  (let [compiled (:compiled state)
        program-net (:program/net state)]
    (if (and compiled program-net)
      (demo/compiled-labels compiled program-net)
      {})))

(defn- cell-row
  [state [id entry]]
  (when (cell/cell? entry)
    {:cell-id (pr-str id)
     :label (get (labels state) id)
     :strongest (semantic-repl/display-cell-value (cell/cell-strongest entry))}))

(defn list-cells
  [state]
  (->> (net/net-env (:program/net (require-state state)))
       (keep (partial cell-row state))
       (sort-by (juxt #(or (:label %) "") :cell-id))
       vec))

(defn- resolve-cell-row
  [state {:keys [cell-id label]}]
  (let [cells (list-cells state)]
    (or (some #(when (= cell-id (:cell-id %)) %) cells)
        (some #(when (= label (:label %)) %) cells)
        (throw (ex-info "cell not found" {:cell-id cell-id :label label})))))

(defn- resolve-cell-id
  [state {:keys [cell-id label]}]
  (let [labels (labels state)
        by-label (some (fn [[id entry]]
                         (when (and (cell/cell? entry)
                                    (= label (get labels id)))
                           id))
                       (net/net-env (:program/net state)))
        by-cell-id (some (fn [[id entry]]
                           (when (and (cell/cell? entry)
                                      (= cell-id (pr-str id)))
                             id))
                         (net/net-env (:program/net state)))]
    (or by-cell-id
        by-label
        (throw (ex-info "cell not found" {:cell-id cell-id :label label})))))

(defn- runtime-cell-id
  [state spec]
  (try
    (resolve-cell-id state spec)
    (catch Exception _
      nil)))

(defn- parse-display-value
  [x]
  (if (string? x)
    (try
      (edn/read-string x)
      (catch Throwable _
        x))
    x))

(defn- compiled-cell-value
  [state {:keys [label]}]
  (let [compiled-network (:program/net state)
        labels (when (and compiled-network (:compiled state))
                 (demo/compiled-labels (:compiled state) compiled-network))]
    (when (and label compiled-network)
      (or (some (fn [[id entry]]
                  (when (and (cell/cell? entry)
                             (= label (get labels id)))
                    (net/network-cell-strongest compiled-network id)))
                (net/net-env compiled-network))
          (some-> (:graph state)
                  semantic-repl/value-labels
                  (get label)
                  parse-display-value)))))

(defn read-cell
  [state request]
  (resolve-cell-row (require-state state) request))

(declare append-tui-block! edit-tui-block! install-block-slots semantic-trace
         sync-tui-block!)

(defn- block-by-index
  [state client-id index]
  (some #(when (= index (:index %)) %)
        (get-in state [:tuis client-id :blocks])))

(defn- update-block
  [state client-id index f]
  (update-in state [:tuis client-id :blocks]
             (fn [blocks]
               (mapv #(if (= index (:index %)) (f %) %) blocks))))

(defn- write-block-value
  [state block _epoch payload]
  (let [current (net/network-cell-strongest (:network state) (:text-id block))]
    (if (= current payload)
      state
      (let [[tasks n1] (core/eval-cells [(message (:text-id block) payload)]
                                        (:network state))
            n2 (core/run-tasks tasks n1)]
        (assoc state :network n2)))))

(defn- next-global-order
  [state]
  (or (:next-order state) 0))

(defn- assign-source-order
  [state client-id index]
  (let [order (next-global-order state)]
    (-> state
        (update-block client-id index
                      #(assoc % :order order :generated? false))
        (update :block-order (fnil conj []) {:client-id client-id
                                             :index index})
        (assoc :next-order (inc order)))))

(defn- source-blocks
  [state]
  (->> (:block-order state)
       (keep (fn [{:keys [client-id index]}]
               (when-let [block (block-by-index state client-id index)]
                 (when (and (not (:generated? block))
                            (some? (:order block)))
                   (assoc block :client-id client-id)))))
       (sort-by :order)
       vec))

(defn- block-text
  [state block]
  (net/network-cell-strongest (:network state) (:text-id block)))

(defn- compiled-result-value
  [compiled network]
  (let [result-id (:cell compiled)
        result (net/network-cell-strongest network result-id)]
    (cond
      (value/unusable? result) value/nothing
      (net/network? result) (semantic-repl/compiled-semantic-graph compiled network)
      :else result)))

(defn- empty-graph []
  {:nodes {} :edges [] :values {}})

(defn- valid-client-id? [client-id]
  (and (string? client-id)
       (boolean (re-matches #"[A-Za-z_][A-Za-z0-9_-]*" client-id))))

(defn- block-list-symbol [client-id]
  (symbol (str client-id ".block")))

(defn- block-at-operator [head-id->blocks]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[head-id index-id maybe-out] (vec arg-ids)
            target-id (or maybe-out out-id)
            index (net/network-cell-strongest network index-id)
            block (some #(when (= index (:index %)) %)
                        (get head-id->blocks head-id))]
        (when-not (and (= 3 (count arg-ids)) block)
          (throw (ex-info "block-at expects a known block list and index"
                          {:arg-ids arg-ids :index index})))
        (let [[read-prop n1] ((stdlib-prop/id (:text-id block) target-id)
                              network)
              [write-prop n2] ((stdlib-prop/id target-id (:text-id block))
                               n1)]
          [n2 [read-prop write-prop] target-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 2 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[head-id index-id maybe-out] (vec arg-ids)
             target-id (or maybe-out out-id)
             index (net/network-cell-strongest current-net index-id)
             block (some #(when (= index (:index %)) %)
                         (get head-id->blocks head-id))
             source-value (when block
                            (net/network-cell-strongest current-net
                                                        (:text-id block)))
             target-value (net/network-cell-strongest current-net target-id)]
         (when-not (= 3 (count arg-ids))
           (throw (ex-info "block-at expects list, index, and output"
                           {:arg-ids arg-ids})))
         (cond-> []
           (and block (not (value/unusable? source-value)))
           (conj (message target-id source-value))

           (and block (not (value/unusable? target-value)))
           (conj (message (:text-id block) target-value)))))}))

(defn- trace-operator [graph-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[source-id direction-id maybe-out] (vec arg-ids)
            target-id (or maybe-out direction-id out-id)
            direction-id (when maybe-out direction-id)
            request-id (ids/new-node-id)
            network* (-> network
                         (nb/ensure-cell graph-id)
                         (nb/install-cell request-id))]
        (when-not (#{2 3} (count arg-ids))
          (throw (ex-info "trace expects source, optional direction, and output"
                          {:arg-ids arg-ids})))
        (let [[request-prop n1]
              ((if direction-id
                 (semantic-trace/p:cell-trace-request source-id
                                                       direction-id
                                                       request-id)
                 (semantic-trace/p:fixed-trace-request source-id
                                                       :upstream
                                                       request-id))
               network*)
              [trace-prop n2] ((semantic-trace/p:semantic-trace request-id
                                                                 graph-id
                                                                 target-id)
                               n1)]
          [n2 [request-prop trace-prop] target-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (last (vec arg-ids)) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[source-id direction-or-out maybe-out] (vec arg-ids)
             target-id (or maybe-out direction-or-out out-id)
             direction (if maybe-out
                         (net/network-cell-strongest current-net direction-or-out)
                         :upstream)
             graph (net/network-cell-strongest current-net graph-id)
             source-value (net/network-cell-strongest current-net source-id)
             request (cond-> {:node source-id :direction direction}
                       (not (value/unusable? source-value))
                       (assoc :value-label
                              (semantic-repl/display-cell-value source-value)))]
         (if (or (value/unusable? graph)
                 (value/unusable? direction))
           []
           [(message target-id
                     (semantic-trace/trace-graph
                      graph
                      request))])))}))

(defn- write-output
  [state block epoch value]
  (if-let [output-index (:output-index block)]
    (if-let [output-block (block-by-index state (:client-id block) output-index)]
      (write-block-value state output-block epoch value)
      state)
    state))

(defn- write-output-to-program-net
  [program-net block state value]
  (if-let [output-index (:output-index block)]
    (if-let [output-block (block-by-index state (:client-id block) output-index)]
      (nb/seed-cell (nb/ensure-cell program-net (:text-id output-block))
                    (:text-id output-block)
                    value)
      program-net)
    program-net))

(defn- read-program-cell
  [state request]
  (or (compiled-cell-value state request)
      (when-let [source-id (runtime-cell-id state request)]
        (net/network-cell-strongest (:program/net state) source-id))))

(defn- all-blocks [state]
  (mapcat (fn [[client-id tui]]
            (map #(assoc % :client-id client-id) (:blocks tui)))
          (:tuis state)))

(defn- clear-generated-block-values
  [state]
  (update state :network
          (fn [n]
            (reduce (fn [n block]
                      (if (:generated? block)
                        (nb/install-cell n (:text-id block))
                        n))
                    n
                    (all-blocks state)))))

(defn- seed-program-block-cell [program-net runtime-net id]
  (let [v (net/network-cell-strongest runtime-net id)]
    (if (value/unusable? v)
      (nb/ensure-cell program-net id)
      (nb/install-cell program-net id v v))))

(defn- seed-program-blocks [state program-net]
  (reduce
   (fn [n block]
     (let [n0 (reduce #(seed-program-block-cell %1 (:network state) %2)
                      n
                      [(:block-id block) (:index-id block)
                       (:next-id block)])
           n0 (if (:generated? block)
                (nb/ensure-cell n0 (:text-id block))
                (seed-program-block-cell n0 (:network state) (:text-id block)))]
       (install-block-slots n0 block)))
   program-net
   (all-blocks state)))

(defn- runtime-env [state base-env graph-id]
  (let [head-id->blocks (into {}
                              (keep (fn [[_ {:keys [head-id blocks]}]]
                                      (when head-id [head-id blocks])))
                              (:tuis state))]
    (as-> base-env env
      (cenv/bind-at env 'block-at (block-at-operator head-id->blocks) 0)
      (cenv/bind-at env 'trace (trace-operator graph-id) 0)
      (reduce-kv (fn [e client-id {:keys [head-id]}]
                   (if head-id
                     (cenv/bind-at e
                                   (block-list-symbol client-id)
                                   (cenv/cell-binding head-id)
                                   0)
                     e))
                 env
                 (:tuis state)))))

(defn- sync-program-block-writes [state program-net epoch]
  (reduce (fn [s block]
            (let [v (net/network-cell-strongest program-net (:text-id block))]
              (if (value/unusable? v)
                s
                (write-block-value s block epoch v))))
          state
          (all-blocks state)))

(defn- compile-program-form
  [state block epoch source]
  (try
    (let [graph-id (stable-node-id :runtime :graph (:order block) epoch)
          env (runtime-env state (:program/env state) graph-id)
          compiled (compiler/compile-source
                    source
                    env
                    {:net (:program/net state)
                     :seed [:runtime/block (:order block) (:epoch block)]})
          program-net0 (nb/run-propagators (:net compiled) (:props compiled))
          graph (assoc (semantic-repl/compiled-semantic-graph compiled program-net0)
                       :source source)
          [tasks program-net1] (core/eval-cells [(message graph-id graph)]
                                                (nb/ensure-cell program-net0
                                                                graph-id))
          program-net (nb/run-propagators (core/run-tasks tasks program-net1)
                                          (:props compiled))
          graph (assoc (semantic-repl/compiled-semantic-graph compiled program-net)
                       :source source)
          result (compiled-result-value compiled program-net)
          program-net (write-output-to-program-net program-net block state result)
          result-key [(:client-id block) (:index block)]
          state' (sync-program-block-writes state program-net epoch)]
      (-> state'
          (assoc :program/net program-net
                 :program/env (:env compiled)
                 :program/graph graph
                 :compiled compiled
                 :compiled-network program-net
                 :graph graph
                 :source source)
          (assoc-in [:program/results result-key]
                    {:result result :compiled compiled})
          (write-output block epoch result)))
    (catch Throwable t
      (write-output state block epoch
                    (str "error in block " (:index block) ": " (ex-message t))))))

(defn- rebuild-block
  [state epoch block]
  (let [source (block-text state block)]
    (if (or (value/unusable? source)
            (not (string? source)))
      state
      (compile-program-form state block epoch source))))

(defn- rebuild-program-state
  [state]
  (let [state (clear-generated-block-values state)
        epoch (inc (or (:program/epoch state) 0))
        base (assoc state
                    :program/net (seed-program-blocks state net/empty-net)
                    :program/env (compiler-helpers/default-env)
                    :program/graph (empty-graph)
                    :program/results {}
                    :program/epoch epoch
                    :compiled nil
                    :compiled-network net/empty-net
                    :graph (empty-graph)
                    :source nil)]
    (reduce (fn [s block]
              (rebuild-block s epoch block))
            base
            (source-blocks state))))

(defn- rebuild-program!
  [session]
  (swap! session rebuild-program-state)
  @session)

(defn- trace-via-propagator
  ([graph request]
   (trace-via-propagator graph request (:direction request)))
  ([graph source direction]
   (let [source-id (ids/new-node-id)
         direction-id (ids/new-node-id)
         request-id (ids/new-node-id)
         graph-id (ids/new-node-id)
         out-id (ids/new-node-id)
         n0 (-> net/empty-net
                (nb/install-cell source-id source source)
                (nb/install-cell direction-id direction direction)
                (nb/install-cell request-id)
                (nb/install-cell graph-id graph graph)
                (nb/install-cell out-id))
         [request-prop-id n1] ((semantic-trace/p:trace-request
                                source-id direction-id request-id)
                               n0)
         [trace-prop-id n2] ((semantic-trace/p:semantic-trace request-id graph-id out-id) n1)
         n3 (nb/run-propagators n2 [request-prop-id trace-prop-id])]
     (net/network-cell-strongest n3 out-id))))

(defn- resolved-trace-request
  [state request]
  (if (or (:label request) (:node request))
    request
    (assoc request :label (:label (resolve-cell-row state request)))))

(defn semantic-trace
  [state request]
  (let [state (require-state state)
        request* (resolved-trace-request state request)]
    (trace-via-propagator (:graph state) request*)))

(defn- installed-trace-graph
  [trace]
  (net/network-cell-strongest (:network trace) (:out-id trace)))

(defn- tick-trace!
  [session trace-id]
  (swap! session
         (fn [state]
           (if-let [trace (get-in state [:traces trace-id])]
             (let [next-epoch (inc (:epoch trace))
                   [tasks n1] (core/eval-cells
                               [(message (:epoch-id trace)
                                         (semantic-trace/epoch next-epoch))]
                               (:network trace))
                   n2 (core/run-tasks tasks n1)]
               (assoc-in state
                         [:traces trace-id]
                         (assoc trace
                                :epoch next-epoch
                                :network n2)))
             state))))

(defn install-semantic-trace!
  [session request]
  (let [state (require-state @session)
        request* (resolved-trace-request state request)
        interval-ms (long (or (:interval-ms request*) 5000))
        trace-id (str (random-uuid))
        request-id (ids/new-node-id)
        graph-id (ids/new-node-id)
        epoch-id (ids/new-node-id)
        out-id (ids/new-node-id)
        n0 (-> net/empty-net
               (nb/install-cell request-id request* request*)
               (nb/install-cell graph-id
                                (semantic-trace/graph-union (:graph state))
                                (semantic-trace/graph-union (:graph state)))
               (nb/install-cell epoch-id (semantic-trace/epoch 0) (semantic-trace/epoch 0))
               (nb/install-cell out-id))
        [prop-id n1] ((semantic-trace/p:semantic-trace request-id graph-id epoch-id out-id) n0)
        n2 (nb/run-propagators n1 [prop-id])
        executor (daemon-executor (str "semantic-trace-clock-" trace-id))
        stop #(do (.shutdownNow executor) nil)
        trace {:trace-id trace-id
               :request request*
               :interval-ms interval-ms
               :network n2
               :prop-id prop-id
               :request-id request-id
               :graph-id graph-id
               :epoch-id epoch-id
               :out-id out-id
               :epoch 0
               :stop stop}]
    (swap! session assoc-in [:traces trace-id] trace)
    (.scheduleAtFixedRate executor
                          #(tick-trace! session trace-id)
                          interval-ms
                          interval-ms
                          TimeUnit/MILLISECONDS)
    {:trace-id trace-id
     :interval-ms interval-ms
     :graph (installed-trace-graph trace)}))

(defn read-installed-trace
  [state {:keys [trace-id]}]
  (let [trace (get-in (require-state state) [:traces trace-id])]
    (when-not trace
      (throw (ex-info "trace not found" {:trace-id trace-id})))
    {:trace-id trace-id
     :epoch (:epoch trace)
     :graph (installed-trace-graph trace)}))

(defn stop-installed-trace!
  [session {:keys [trace-id]}]
  (let [trace (get-in @session [:traces trace-id])]
    (when-not trace
      (throw (ex-info "trace not found" {:trace-id trace-id})))
    ((:stop trace))
    (swap! session update :traces dissoc trace-id)
    {:trace-id trace-id
     :stopped true}))

(defn- install-block-slots
  [n {:keys [block-id index-id text-id next-id]}]
  (let [[index-prop n] ((obj/p:slot :block/index index-id block-id) n)
        [text-prop n] ((obj/p:slot :block/text text-id block-id) n)
        [next-prop n] ((obj/p:cdr next-id block-id) n)]
    (nb/run-propagators n [index-prop text-prop next-prop])))

(defn register-tui!
  [session {:keys [client-id]}]
  (when-not client-id
    (throw (ex-info "missing client-id" {})))
  (when-not (valid-client-id? client-id)
    (throw (ex-info "invalid client-id for compiler env binding"
                    {:client-id client-id})))
  (let [state (ensure-session-state! session)]
    (if-let [tui (get-in state [:tuis client-id])]
      {:client-id (:client-id tui)
       :view-id (pr-str (:view-id tui))
       :next-index (:next-index tui)}
      (let [view-id (stable-node-id :tui client-id :view)
            n (nb/ensure-cell (:network state) view-id)
            tui {:client-id client-id
                 :view-id view-id
                 :head-id nil
                 :tail-id nil
                 :next-index 0
                 :blocks []}]
        (swap! session #(-> %
                            (assoc :network n)
                            (assoc-in [:tuis client-id] tui)))
        {:client-id (:client-id tui)
         :view-id (pr-str (:view-id tui))
         :next-index (:next-index tui)}))))

(defn- tui!
  [session client-id]
  (register-tui! session {:client-id client-id})
  (get-in @session [:tuis client-id]))

(defn- append-tui-block-raw!
  [session {:keys [client-id text generated?] :as command}]
  (let [tui (tui! session client-id)
        index (:next-index tui)
        has-text? (contains? command :text)
        block {:block-id (ids/new-node-id)
               :index-id (ids/new-node-id)
               :text-id (ids/new-node-id)
               :next-id (ids/new-node-id)
               :index index
               :epoch 0
               :generated? (boolean generated?)}
        state @session
        n0 (-> (:network state)
               (nb/install-cell (:block-id block))
               (nb/install-cell (:index-id block) index index)
               ((if has-text?
                  #(nb/install-cell % (:text-id block) text text)
                  #(nb/install-cell % (:text-id block))))
               (nb/install-cell (:next-id block)))
        n1 (install-block-slots n0 block)
        messages (cond-> []
                   (nil? (:head-id tui)) (conj (message (:view-id tui)
                                                        (:block-id block)))
                   (:tail-id tui) (conj (message (:next-id (peek (:blocks tui)))
                                                 (:block-id block))))
        [tasks n2] (core/eval-cells messages n1)
        n3 (core/run-tasks tasks n2)
        tui' (-> tui
                 (assoc :head-id (or (:head-id tui) (:block-id block))
                        :tail-id (:block-id block))
                 (update :next-index inc)
                 (update :blocks conj block))]
    (swap! session #(-> %
                        (assoc :network n3)
                        (assoc-in [:tuis client-id] tui')))
    {:client-id client-id
     :index index
     :block-id (pr-str (:block-id block))
     :text-id (pr-str (:text-id block))
     :block block}))

(defn append-tui-block!
  [session {:keys [client-id generated?] :as command}]
  (let [source-result (append-tui-block-raw! session command)
        source-block (:block source-result)]
    (if generated?
      (dissoc source-result :block)
      (let [output-result (append-tui-block-raw! session
                                                {:client-id client-id
                                                 :generated? true})
            output-block (:block output-result)
            source-block' (assoc source-block
                                 :output-index (:index output-block)
                                 :output-text-id (:text-id output-block))]
        (swap! session
               #(-> %
                    (assign-source-order client-id (:index source-block))
                    (update-block client-id
                                  (:index source-block)
                                  (fn [block]
                                    (merge block
                                           (select-keys source-block'
                                                        [:output-index
                                                         :output-text-id]))))))
        (rebuild-program! session)
        (assoc (dissoc source-result :block)
               :output-index (:index output-block)
               :output-block-id (:block-id output-result))))))

(defn edit-tui-block!
  [session {:keys [client-id index text]}]
  (let [tui (tui! session client-id)
        block0 (some #(when (= index (:index %)) %) (:blocks tui))]
    (when-not block0
      (throw (ex-info "block not found" {:client-id client-id :index index})))
    (let [block (if (:generated? block0)
                  (let [output-result (append-tui-block-raw!
                                       session
                                       {:client-id client-id
                                        :generated? true})
                        output-block (:block output-result)
                        block' (assoc block0
                                      :generated? false
                                      :output-index (:index output-block)
                                      :output-text-id (:text-id output-block))]
                    (swap! session
                           #(-> %
                                (assign-source-order client-id index)
                                (update-block client-id index
                                              (fn [b]
                                                (merge b
                                                       (select-keys block'
                                                                    [:generated?
                                                                     :output-index
                                                                     :output-text-id]))))))
                    block')
                  block0)
          new-epoch (inc (or (:epoch block) 0))
          _ (swap! session update-block client-id index
                   #(assoc % :epoch new-epoch :generated? false))
          [tasks n1] (core/eval-cells [(message (:text-id block) text)]
                                      (:network @session))
          n2 (core/run-tasks tasks n1)]
      (swap! session assoc :network n2)
      (rebuild-program! session)
      {:client-id client-id
       :index index})))

(defn sync-tui-block!
  [session {:keys [client-id index from trace-id]}]
  (let [tui (tui! session client-id)
        block (some #(when (= index (:index %)) %) (:blocks tui))]
    (when-not block
      (throw (ex-info "block not found" {:client-id client-id :index index})))
    (if trace-id
      (do
        (swap! session assoc-in [:tuis client-id :blocks]
               (mapv #(if (= index (:index %))
                        (assoc % :trace-id trace-id)
                        %)
                     (:blocks tui)))
        {:client-id client-id :index index :trace-id trace-id})
      (let [v (read-program-cell @session from)]
        (when (or (nil? v) (value/unusable? v))
          (throw (ex-info "cell not found" from)))
        (swap! session write-block-value block (:program/epoch @session) v)
        {:client-id client-id
         :index index
         :from from}))))

(defn- block-value
  [state block]
  (if-let [trace-id (:trace-id block)]
    (get-in (read-installed-trace state {:trace-id trace-id}) [:graph])
    (net/network-cell-strongest (:network state) (:text-id block))))

(defn read-tui-view
  [state {:keys [client-id]}]
  (let [tui (get-in (require-state state) [:tuis client-id])]
    (when-not tui
      (throw (ex-info "tui client not found" {:client-id client-id})))
    {:client-id client-id
     :view-id (pr-str (:view-id tui))
     :blocks (mapv (fn [block]
                     {:index (:index block)
                      :block-id (pr-str (:block-id block))
                      :text-id (pr-str (:text-id block))
                      :value (block-value state block)})
                   (:blocks tui))}))

(defn unregister-tui!
  [session {:keys [client-id]}]
  (swap! session update :tuis dissoc client-id)
  {:client-id client-id
   :unregistered true})

(defn handle-command!
  [session {:keys [op source] :as command}]
  (try
    {:ok true
     :result
     (case op
       :tui/register (register-tui! session command)
       :tui/append-block (append-tui-block! session command)
       :tui/edit-block (edit-tui-block! session command)
       :tui/sync-block (sync-tui-block! session command)
       :tui/read-view (read-tui-view @session command)
       :tui/unregister (unregister-tui! session command)
       :compile/source (compile-source! session source)
       :cells/list (list-cells @session)
       :cell/read (read-cell @session command)
       :semantic/graph (:graph (require-state @session))
       :semantic/trace (semantic-trace @session command)
       :semantic/trace/install (install-semantic-trace! session command)
       :semantic/trace/read (read-installed-trace @session command)
       :semantic/trace/stop (stop-installed-trace! session command)
       (throw (ex-info "unknown runtime op" {:op op})))}
    (catch Throwable t
      {:ok false
       :error (ex-message t)
       :data (ex-data t)})))
