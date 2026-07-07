(ns graph.compiler-2-tui
  "Charm TUI client for linked-list runtime blocks."
  (:require [clojure.string :as str]
            [charm.components.viewport :as viewport]
            [charm.components.text-input :as text-input]
            [charm.message :as msg]
            [charm.program :as program]
            [graph.compiler-2-runtime-server :as server]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [graph.vijual :as v]))

(defn graph-value?
  [x]
  (and (map? x)
       (map? (:nodes x))
       (vector? (:edges x))))

(defn- graph-render-opts
  [{:keys [width height]}]
  (cond-> semantic-repl/stress-opts
    (pos? (long (or width 0)))
    ;; ponytail: approximate node+route width; replace with measured fit if Vijual grows sizing support.
    (assoc :stress-grid-width (max 1 (quot (long width) 14)))

    (pos? (long (or height 0)))
    (assoc :stress-grid-height (max 1 (quot (long height) 7)))))

(defn- alias-node-set
  [v]
  (cond
    (nil? v) #{}
    (set? v) v
    (sequential? v) (set v)
    :else #{v}))

(defn- edge-pair?
  [edge]
  (and (vector? edge) (= 2 (count edge))))

(defn- label-rank
  [label]
  (cond
    (or (nil? label) (= "cell" (str label))) 3
    (str/starts-with? (str label) "cell") 2
    (str/starts-with? (str label) "slot ") 2
    :else 0))

(defn- better-label
  [old new]
  (if (<= (label-rank new) (label-rank old))
    new
    old))

(defn- canonical-node
  [nodes values node-ids]
  (first
   (sort-by (fn [id]
              [(if (contains? values id) 0 1)
               (label-rank (get nodes id))
               (pr-str id)])
            node-ids)))

(defn- canonicalize-render-graph
  [{:keys [nodes node-aliases values edges] :as graph}]
  (let [groups (->> (vals node-aliases)
                    (map alias-node-set)
                    (filter #(<= 2 (count %))))
        replacements (reduce
                      (fn [replacements group]
                        (let [canonical (canonical-node nodes values group)]
                          (reduce #(assoc %1 %2 canonical)
                                  replacements
                                  group)))
                      {}
                      groups)
        canonical (fn [node-id] (get replacements node-id node-id))
        nodes* (reduce (fn [m [id label]]
                         (update m (canonical id) better-label label))
                       {}
                       nodes)
        edges* (vec (distinct
                     (keep (fn [[from to]]
                             (let [from* (canonical from)
                                   to* (canonical to)]
                               (when (not= from* to*)
                                 [from* to*])))
                           (filter edge-pair? edges))))]
    (assoc graph :nodes nodes* :edges edges*)))

(defn- normalize-render-graph
  [{:keys [nodes edges] :as graph}]
  (let [ids (->> (concat (keys nodes) (mapcat identity edges))
                 distinct
                 (sort-by pr-str))
        id-map (into {}
                     (map-indexed (fn [idx id]
                                    [id (keyword (str "n" idx))]))
                     ids)]
    (assoc graph
           :nodes (into {}
                        (map (fn [[id label]]
                               [(get id-map id) label]))
                        nodes)
           :edges (mapv (fn [[from to]]
                          [(get id-map from) (get id-map to)])
                        edges))))

(defn render-value
  ([value] (render-value value nil))
  ([value viewport-size]
   (if (graph-value? value)
     (try
       (let [{:keys [edges nodes]} (-> value
                                       canonicalize-render-graph
                                       normalize-render-graph)]
         (with-out-str
           (v/draw-stress-directed-graph edges
                                         nodes
                                         (graph-render-opts viewport-size))))
       (catch Throwable t
         (str "graph render error: " (ex-message t))))
     (str value))))

(defn- blank-block?
  [{:keys [value referenced?]}]
  (and (= :bool4/nothing value)
       (not referenced?)))

(defn- visible-blocks
  [blocks]
  (if (blank-block? (peek blocks))
    (pop (vec blocks))
    blocks))

(defn- error-source-label
  [source]
  (cond
    (map? source)
    (str/join " "
              (keep (fn [k]
                      (when-let [v (get source k)]
                        (str (name k) "=" v)))
                    [:op :phase]))

    (some? source) (str source)
    :else "runtime"))

(defn- render-runtime-error
  [{:keys [source message class data]}]
  (str "- " (error-source-label source)
       (when class (str " " class))
       (when message (str "\n  " message))
       (when (seq data) (str "\n  data: " (pr-str data)))))

(defn- render-error-panel
  [errors]
  (when (seq errors)
    (str "runtime errors\n"
         (str/join "\n"
                   (map render-runtime-error (take-last 5 errors))))))

(defn- render-blocks
  [blocks viewport-size]
  (str/join
   "\n\n"
   (map (fn [{:keys [index value annotation]}]
          (str "[" index "]\n"
               (render-value value viewport-size)
               (when annotation
                 (str "\n" annotation))))
        (visible-blocks blocks))))

(defn render-view
  ([view] (render-view view nil))
  ([{:keys [blocks errors]} viewport-size]
   (str/join "\n\n"
             (remove str/blank?
                     [(or (render-error-panel errors) "")
                      (render-blocks blocks viewport-size)]))))

(def ^:private viewport-keys
  {:line-up ["up"]
   :line-down ["down"]
   :half-page-up ["ctrl+u"]
   :half-page-down ["ctrl+d"]
   :page-up ["pgup"]
   :page-down ["pgdown"]
   :top ["home"]
   :bottom ["end"]})

(defn- new-viewport
  []
  (viewport/viewport "" :height 20 :keys viewport-keys))

(defn- viewport-height
  [{:keys [window-size error]}]
  (if-let [height (:height window-size)]
    (max 1 (- (long height)
              5
              (if error 2 0)))
    20))

(defn- viewport-width
  [{:keys [window-size]}]
  (or (:width window-size) 0))

(defn- viewport-size
  [state]
  {:width (viewport-width state)
   :height (viewport-height state)})

(defn- configure-viewport
  [state content]
  (let [vp0 (:viewport state)
        vp1 (if (= content (viewport/viewport-content vp0))
              vp0
              (viewport/viewport-scroll-to
               (viewport/viewport-set-content vp0 content)
               (:y-offset vp0 0)))
        vp2 (viewport/viewport-set-dimensions vp1
                                               (viewport-width state)
                                               (viewport-height state))]
    (assoc state :viewport vp2)))

(defn- resize-viewport
  [state]
  (configure-viewport state (viewport/viewport-content (:viewport state))))

(defn- render-view-needed?
  [state view-changed?]
  (or view-changed?
      (:error state)))

(def ^:private resize-render-delay-ms 120)

(defn- resize-render-cmd
  [window-size]
  (program/cmd
   (fn []
     (Thread/sleep resize-render-delay-ms)
     {:type :runtime/resize-render
      :window-size window-size})))

(defn- next-block-index
  [view]
  (or (some->> (:blocks view)
               (map :index)
               seq
               (apply max)
               inc)
      0))

(defn- input-block-index
  [view]
  (let [blocks (:blocks view)]
    (if (blank-block? (peek blocks))
      (:index (peek blocks))
      (next-block-index view))))

(defn- update-input-prompt
  [state]
  (assoc-in state [:input :prompt]
            (str "[" (input-block-index (:view state)) "]> ")))

(defn poll-view
  [host port client-id]
  (let [response (server/request host port {:op :tui/read-view
                                            :client-id client-id})]
    (if (and (not (:ok response))
             (#{"tui client not found"
                "no compiled source in runtime session"}
              (:error response)))
      (do
        (server/request host port {:op :tui/register :client-id client-id})
        (server/request host port {:op :tui/read-view :client-id client-id}))
      response)))

(defn init-model
  [{:keys [host port client-id poll-ms]
    :or {host server/default-host
         port server/default-port
         client-id "tui-1"
         poll-ms 1000}}]
  {:host host
   :port port
   :client-id client-id
   :poll-ms poll-ms
   :input (text-input/text-input :prompt "> ")
   :viewport (new-viewport)
   :window-size nil
   :view nil
   :error nil})

(defn refresh-cmd
  [{:keys [host port client-id poll-ms]}]
  (program/cmd
   (fn []
     (Thread/sleep (long (or poll-ms 0)))
     {:type :runtime/view
      :response (poll-view host port client-id)})))

(defn append-cmd
  [{:keys [host port client-id input view]}]
  (program/cmd
   (fn []
     {:type :runtime/view
      :response (server/request host port {:op :tui/submit-block
                                           :client-id client-id
                                           :text (text-input/value input)})})))

(defn update-fn
  [state message]
  (cond
    (or (msg/key-match? message "q")
        (msg/key-match? message "ctrl+c"))
    [state program/quit-cmd]

    (= :runtime/view (:type message))
    (let [response (:response message)]
      (if (:ok response)
        (let [view (:result response)
              view-changed? (not= view (:view state))
              state' (update-input-prompt
                      (assoc state :view view :error nil))
              state'' (if (or (:resize/target state')
                              (not (render-view-needed? state view-changed?)))
                        state'
                        (configure-viewport
                         state'
                         (render-view view (viewport-size state'))))]
          [state''
           (refresh-cmd state')])
        [(configure-viewport (assoc state :error (:error response))
                             (or (some-> (:view state)
                                         (render-view (viewport-size state)))
                                 ""))
         (refresh-cmd state)]))

    (msg/key-match? message :enter)
    (if (str/blank? (text-input/value (:input state)))
      [state nil]
      [(update state :input text-input/reset) (append-cmd state)])

    (msg/window-size? message)
    (let [window-size (select-keys message [:width :height])
          state' (assoc state
                        :window-size window-size
                        :resize/target window-size
                        :resize/suppress? true)]
      [state'
       (resize-render-cmd window-size)])

    (= :runtime/resize-render (:type message))
    (let [window-size (:window-size message)]
      (if (= window-size (:resize/target state))
        [(-> state
             (dissoc :resize/target)
             (dissoc :resize/suppress?)
             (configure-viewport
              (or (some-> (:view state)
                          (render-view (viewport-size state)))
                  "")))
         nil]
        [state nil]))

    :else
    (let [[vp vp-cmd] (viewport/viewport-update (:viewport state) message)]
      (if (not= vp (:viewport state))
        [(assoc state :viewport vp) vp-cmd]
        (let [[input cmd] (text-input/text-input-update (:input state) message)]
          [(assoc state :input input) cmd])))))

(defn view
  [{:keys [client-id input viewport error resize/suppress?]}]
  (if suppress?
    ""
    (str "compiler-2 " client-id "\n"
         "enter appends, arrows/page/home/end scroll, q quits\n\n"
         (when error (str "error: " error "\n\n"))
         (viewport/viewport-view viewport)
         "\n\n" (text-input/text-input-view input))))

(defn run-client
  [{:keys [host port client-id poll-ms]
    :or {host server/default-host
         port server/default-port
         client-id "tui-1"
         poll-ms 1000}}]
  (let [registered (server/request host port {:op :tui/register
                                              :client-id client-id})]
    (when-not (:ok registered)
      (throw (ex-info "failed to register TUI client"
                      {:client-id client-id
                       :response registered}))))
  (try
    (let [model (init-model {:host host
                             :port port
                             :client-id client-id
                             :poll-ms poll-ms})]
      (program/run {:init (fn [] [model (refresh-cmd (assoc model :poll-ms 0))])
                    :update update-fn
                    :view view
                    :alt-screen false}))
    (finally
      (server/request host port {:op :tui/unregister :client-id client-id}))))

(defn- parse-args
  [args]
  (loop [args args
         opts {}]
    (if-let [arg (first args)]
      (case arg
        ("-name" "--name" "--client-id")
        (recur (nnext args) (assoc opts :client-id (second args)))

        ("-port" "--port")
        (recur (nnext args) (assoc opts :port (Long/parseLong (second args))))

        (if (re-matches #"\d+" arg)
          (recur (next args) (assoc opts :port (Long/parseLong arg)))
          (recur (next args) (assoc opts :client-id arg))))
      opts)))

(defn -main
  [& args]
  (run-client (parse-args args)))
