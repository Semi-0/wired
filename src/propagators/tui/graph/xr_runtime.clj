(ns propagators.tui.graph.xr-runtime
  "XR projection helpers for the compiler-2 runtime.

  XR is a client surface only. It can extend the graph through compiler/runtime
  declarations or send ordinary messages into existing cells."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [propagators.runtime :as runtime]
            [propagators.tui.graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.model.env :as cenv]
            [propagators.infra.datastructures.behavior :as behavior]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.datastructures.tms :as tms]
            [propagators.runtime.experimental.visualization.interaction :as interaction]
            [propagators.infra.ids :as ids]
            [propagators.infra.network :as net]))

(def default-xr-client-id "xr")

(defn- node-id-string
  [x]
  (pr-str x))

(defn- parse-cell-id
  [cell-id]
  (cond
    (ids/node-id? cell-id) cell-id
    (string? cell-id) (edn/read-string cell-id)
    :else nil))

(defn- normalize-target
  [{:keys [cell-id label] :as target}]
  (cond-> {}
    cell-id (assoc :cell-id cell-id)
    label (assoc :label label)
    (string? target) (assoc :cell-id target)))

(defn- resolve-cell-id
  [state target]
  (let [target* (normalize-target target)
        cell-id (:cell-id target*)
        label (:label target*)]
    (or (parse-cell-id cell-id)
        (when label
          (cenv/resolve-binding-id (:program/net state)
                                   (:program/env state)
                                   (symbol label)))
        (when label
          (let [node-ids (into #{}
                               (keep (fn [[node-id node-label]]
                                       (when (= label node-label)
                                         node-id)))
                               (get-in state [:graph :nodes]))
                aliases (:node-aliases (:graph state))]
            (some (fn [[cell-id alias]]
                    (let [alias-set (cond
                                      (set? alias) alias
                                      (sequential? alias) (set alias)
                                      :else #{alias})]
                      (when (seq (set/intersection node-ids alias-set))
                        cell-id)))
                  aliases)))
        (some-> (runtime/read-cell state target*) :cell-id parse-cell-id)
        (throw (ex-info "cell not found" target*)))))

(defn- value-kind
  [v]
  (cond
    (value/nothing? v) :nothing
    (value/contradiction? v) :contradiction
    (and (contains? (obj/public-slot-keys v) behavior/base-layer)
         (contains? (obj/public-slot-keys v) behavior/summary-layer))
    :behavior-projection
    (tms/distributed-value? v) :tms
    (behavior/behavior-value? v) :behavior
    (net/network? v) :network
    :else :value))

(defn- behavior-summary
  [v]
  (let [current (behavior/strongest-value v)
        records (try
                  (count (behavior/history-records v))
                  (catch Throwable _ nil))]
    (cond-> {:kind "behavior"
             :current (semantic-repl/display-cell-value
                       (if (value/unusable? current)
                         current
                         (behavior/base-value current)))}
      records (assoc :retained-count records))))

(defn- behavior-projection-summary
  [v]
  (let [retained-count (try
                         (behavior/summary-retained-count v)
                         (catch Throwable _ nil))
        latest-time (try
                      (behavior/summary-latest-time v)
                      (catch Throwable _ nil))]
    (cond-> {:kind "behavior"
             :current (semantic-repl/display-cell-value (behavior/base-value v))}
      (some? retained-count) (assoc :retained-count retained-count)
      (some? latest-time) (assoc :latest-time
                                 (semantic-repl/display-cell-value
                                  latest-time)))))

(defn- tms-summary
  [v]
  (let [projected (tms/strongest-distributed-value v)
        base (when-not (value/unusable? projected)
               (tms/distributed-base-value projected))
        active-premises (try
                          (mapv pr-str (tms/distributed-supports projected))
                          (catch Throwable _ []))
        premise-slots (try
                        (count (tms/distributed-premise-slots v))
                        (catch Throwable _ 0))]
    (cond-> {:kind "tms"
             :current (semantic-repl/display-cell-value
                       (if (value/unusable? base) projected base))
             :premise-slot-count premise-slots}
      (seq active-premises) (assoc :active-premises active-premises))))

(defn value-summary
  [v]
  (let [kind (value-kind v)]
    (case kind
      :nothing {:kind "nothing"}
      :contradiction {:kind "contradiction"}
      :behavior-projection (behavior-projection-summary v)
      :tms (tms-summary v)
      :behavior (behavior-summary v)
      :network {:kind "network"}
      {:kind "value"
       :value (semantic-repl/display-cell-value v)})))

;; this is awful
(defn- node-kind
  [label ui]
  (let [label (str label)]
    (cond
      (= "widget" (:kind ui)) "widget"

      (or (str/starts-with? label "app:")
          (str/starts-with? label "call ")
          (str/starts-with? label "slot ")
          (contains? #{"<->" "->" "+" "-" "*" "/" "switch" "trace"
                       "xr-io" "io:xr"
                       "block" "block-at" "be:block" "be:block-at"
                       "instance" "translate" "list"
                       "slider-io" "slider-panel-io"
                       "io:slider" "io:slider-panel"
                       "io:slider-panel-name"}
                     label))
      "propagator"

      :else "cell")))

(defn- alias-node-set
  [v]
  (cond
    (nil? v) #{}
    (set? v) v
    (sequential? v) (set v)
    :else #{v}))

(defn- canonical-node
  [nodes values node-ids]
  (first
   (sort-by (fn [id]
              [(if (contains? values id) 0 1)
               (if (str/starts-with? (str (get nodes id)) "slot ") 1 0)
               (pr-str id)])
            node-ids)))

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

(defn- canonicalize-graph
  [{:keys [nodes node-kinds node-aliases values node-ui expansions edges] :as graph}]
  (let [groups (->> (vals node-aliases)
                    (map alias-node-set)
                    (filter #(<= 2 (count %))))
        replacements (reduce (fn [replacements group]
                               (let [canonical (canonical-node nodes values group)]
                                 (reduce (fn [m node-id]
                                           (assoc m node-id canonical))
                                         replacements
                                         group)))
                             {}
                             groups)
        canonical (fn [node-id] (get replacements node-id node-id))
        nodes* (reduce (fn [m [id label]]
                         (let [id* (canonical id)]
                           (update m id* better-label label)))
                       {}
                       nodes)
        values* (reduce (fn [m [id value]]
                          (assoc m (canonical id) value))
                        {}
                        values)
        node-ui* (reduce (fn [m [id ui]]
                           (assoc m (canonical id) ui))
                         {}
                         node-ui)
        expansions* (reduce (fn [m [id expansion]]
                              (assoc m (canonical id) expansion))
                            {}
                            expansions)
        edges* (vec (distinct
                     (keep (fn [[from to]]
                             (let [from* (canonical from)
                                   to* (canonical to)]
                               (when (not= from* to*)
                                 [from* to*])))
                           (filter edge-pair? edges))))
        node-aliases* (reduce (fn [m [cell-id alias-nodes]]
                                (update m (canonical-node nodes*
                                                          values*
                                                          (set (map canonical
                                                                    (alias-node-set alias-nodes))))
                                        (fnil conj [])
                                        cell-id))
                              {}
                              node-aliases)]
    (assoc graph
           :nodes nodes*
           :node-kinds (into {} (map (fn [[id kind]] [(canonical id) kind])) node-kinds)
           :node-aliases node-aliases*
           :values values*
           :node-ui node-ui*
           :expansions expansions*
           :edges edges*)))

(defn- graph-projection-options
  [state]
  {:changed-node-ids (:runtime/changed-node-ids state)
   :changed-cell-ids (:runtime/changed-cells state)})

(defn- id-string
  [id]
  (if (string? id)
    id
    (node-id-string id)))

(defn graph->json
  "Project a semantic trace graph into browser-friendly data.

  This is data for rendering, not runtime truth."
  ([graph]
   (graph->json graph {}))
  ([graph {:keys [changed-node-ids changed-cell-ids]}]
  (let [graph (canonicalize-graph graph)
        nodes (:nodes graph)
        values (:values graph)
        aliases (:node-aliases graph)
        node-ui (:node-ui graph)
        expansions (:expansions graph)]
    (cond-> {:graph true
             :widgets (->> node-ui
                           (keep (fn [[_id ui]]
                                   (when (= "widget" (:kind ui)) ui)))
                           vec)
             :nodes (mapv (fn [[id label]]
                            (let [ui (get node-ui id)]
                              (cond-> {:id (node-id-string id)
                                       :label (str label)
                                       :kind (if-let [kind (get (:node-kinds graph) id)]
                                               (name kind)
                                               (node-kind label ui))}
                                ui
                                (assoc :ui ui)

                                (contains? values id)
                                (assoc :value (value-summary (get values id)))

                                (contains? aliases id)
                                (assoc :aliases
                                       (mapv node-id-string (get aliases id)))

                                (contains? expansions id)
                                (assoc :expandable true
                                       :expansion-id (node-id-string id)))))
                          nodes)
             :edges (mapv (fn [[from to]]
                            {:from (node-id-string from)
                             :to (node-id-string to)})
                          (:edges graph))}
      (seq changed-node-ids)
      (assoc :changed-node-ids (mapv id-string changed-node-ids))

      (seq changed-cell-ids)
      (assoc :changed-cell-ids (mapv id-string changed-cell-ids))))))

(defn- history-sample->json
  [sample]
  (-> sample
      (update :sample/content value-summary)
      (update :sample/strongest value-summary)))

(defn view->json
  "Project a resolved visualizer declaration into browser-friendly data."
  [view]
  (let [base {:type (name (:view/type view))
              :id (id-string (:view/id view))}]
    (case (:view/type view)
      :graph
      (assoc base :graph (graph->json (:view/graph view)))

      :collection
      (let [collection (:view/collection view)]
        (assoc base
               :kind (name (:kind collection))
               :epoch (:view/epoch view) :revision (:view/revision view)
               :generation (:view/generation view)
               :selectable (some? (:view/selection-cell view))
               :items (mapv (fn [row]
                              {:id (pr-str (:identity row))
                               :label (:label row)
                               :kind (some-> (:node-kind row) name)
                               :value (value-summary (:payload row))}) (:items collection))
               :edges (mapv (fn [[a b]] {:from (pr-str a) :to (pr-str b)}) (:edges collection))
               :pending (count (filter #(= :pending (:membership %)) (:candidates collection)))))

      :cell-window
      (assoc base
             :content (value-summary (:view/content view))
             :strongest (value-summary (:view/strongest view)))

      :cell-history
      (assoc base
             :samples (mapv history-sample->json (:view/samples view)))

      :hierarchy
      (assoc base
             :roots (mapv node-id-string (:view/roots view))
             :graph (graph->json (:view/graph view)))

      :juxtapose
      (assoc base
             :layout {:axis (name (get-in view [:view/layout :axis]))}
             :children (mapv view->json (:view/resolved-children view)))

      (throw (ex-info "unknown resolved view type" {:view view})))))

(defn- trace-request
  [{:keys [label node direction] :as command}]
  (let [direction (cond
                    (keyword? direction) direction
                    (string? direction) (keyword direction)
                    :else :upstream)]
    (cond-> {}
    label (assoc :label label)
    node (assoc :node node)
    direction (assoc :direction direction)
    (:interval-ms command) (assoc :interval-ms (:interval-ms command)))))

(defn xr-trace-install!
  [session command]
  (let [trace-id (str (random-uuid))
        request (trace-request command)
        graph (runtime/semantic-trace @session request)]
    (swap! session #(-> %
                        (assoc-in [:xr/traces trace-id] request)
                        (assoc-in [:xr :traces trace-id] request)))
    {:trace-id trace-id
     :interval-ms (:interval-ms request)
     :request request
     :graph (graph->json graph (graph-projection-options @session))}))

(defn xr-trace-read
  [state-or-session {:keys [trace-id]}]
  (let [session? (instance? clojure.lang.IAtom state-or-session)
        state (if session? @state-or-session state-or-session)
        request (or (get-in state [:xr/traces trace-id])
                    (get-in state [:xr :traces trace-id]))]
    (when-not request
      (throw (ex-info "xr trace not found" {:trace-id trace-id})))
    {:trace-id trace-id
     :request request
     :graph (graph->json (runtime/semantic-trace state request)
                         (graph-projection-options state))}))

(defn xr-trace-expand
  [state command]
  {:graph (graph->json (runtime/semantic-expansion state command)
                       (graph-projection-options state))})

(defn xr-extend-graph!
  [session {:keys [source client-id] :or {client-id default-xr-client-id}}]
  (when (str/blank? source)
    (throw (ex-info "missing source for xr graph extension" {})))
  (let [extension (runtime/extend-source! session {:source source
                                                   :client-id client-id})]
    (swap! session update :xr
           (fn [xr-state]
             (let [xr-state (or xr-state {})
                   sources (conj (vec (:sources xr-state)) source)]
               (assoc xr-state
                      :client-id client-id
                      :sources sources
                      :program (str "(let-cell []\n"
                                    (str/join "\n" sources)
                                    "\n)")
                      :last-extension extension)))))
  {:client-id client-id
   :graph (graph->json (:graph @session)
                       (graph-projection-options @session))})

(defn- message-update
  [{:keys [kind value tick premise epoch active] :as msg}]
  (case kind
    "value" value
    :value value

    "behavior-event"
    (obj/compound-object {tick value})
    :behavior-event
    (obj/compound-object {tick value})

    "behavior-latest"
    (behavior/retained-value :xr tick value #{[:xr tick]})
    :behavior-latest
    (behavior/retained-value :xr tick value #{[:xr tick]})

    "tms-premise"
    (tms/distributed-premise-update premise epoch active)
    :tms-premise
    (tms/distributed-premise-update premise epoch active)

    (throw (ex-info "unsupported xr message kind" {:message msg}))))

(defn xr-send-message!
  [session {:keys [target message] :as command}]
  (let [state @session
        cell-id (resolve-cell-id state target)
        update (message-update message)]
    (runtime/commit-runtime-input! session
                                   {:runtime/input :xr/message
                                    :cell-id cell-id
                                    :update update
                                    :command command})
  {:target (runtime/read-cell @session {:cell-id (pr-str cell-id)})
     :graph (graph->json (:graph @session)
                         (graph-projection-options @session))
     :command command}))

(defn xr-widget-event!
  [session {:keys [widget-id channel value] :as command}]
  (runtime/commit-runtime-input! session
                                 {:runtime/input :xr/widget-event
                                  :widget-id widget-id
                                  :channel (or channel "value")
                                  :value value
                                  :command command})
  {:widgets (get-in @session [:xr :widgets])
   :command command})

(defn handle-command!
  [session {:keys [op] :as command}]
  (case op
    :xr/trace/install (xr-trace-install! session command)
    :xr/trace/read (xr-trace-read session command)
    :xr/trace/expand (xr-trace-expand @session command)
    :xr/extend-graph (xr-extend-graph! session command)
    :xr/send-message (xr-send-message! session command)
    :xr/widget-event (xr-widget-event! session command)
    :xr/view-select
    (locking session
      (runtime/commit-runtime-input! session (interaction/selection-input @session command))
      {:status :selected})
    ;; Delegate existing runtime commands for convenience.
    (:result (runtime/handle-command! session command))))

(defn request-runtime
  [host port command]
  ((requiring-resolve 'propagators.tui.graph.compiler-2-runtime-server/request)
   host
   port
   command))
