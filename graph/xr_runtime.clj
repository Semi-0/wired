(ns graph.xr-runtime
  "XR projection helpers for the compiler-2 runtime.

  XR is a client surface only. It can extend the graph through compiler/runtime
  declarations or send ordinary messages into existing cells."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.cells.value :as value]
            [propagators.compiler-2.env :as cenv]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.tms :as tms]
            [propagators.ids :as ids]
            [propagators.network :as net]))

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
          (when-let [binding (cenv/lookup (:program/env state)
                                          (symbol label))]
            (cenv/binding-id binding)))
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
      :tms (tms-summary v)
      :behavior (behavior-summary v)
      :network {:kind "network"}
      {:kind "value"
       :value (semantic-repl/display-cell-value v)})))

(defn- node-kind
  [label]
  (let [label (str label)]
    (if (or (str/starts-with? label "app:")
            (str/starts-with? label "call ")
            (str/starts-with? label "slot ")
            (contains? #{"<->" "->" "+" "-" "*" "/" "switch" "trace" "xr-io"
                         "block-at" "instance" "translate"}
                       label))
      "propagator"
      "cell")))

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

(defn- canonicalize-graph
  [{:keys [nodes node-aliases values expansions edges] :as graph}]
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
                           (if (contains? m id*)
                             m
                             (assoc m id* label))))
                       {}
                       nodes)
        values* (reduce (fn [m [id value]]
                          (assoc m (canonical id) value))
                        {}
                        values)
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
                           edges)))
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
           :node-aliases node-aliases*
           :values values*
           :expansions expansions*
           :edges edges*)))

(defn graph->json
  "Project a semantic trace graph into browser-friendly data.

  This is data for rendering, not runtime truth."
  [graph]
  (let [graph (canonicalize-graph graph)
        nodes (:nodes graph)
        values (:values graph)
        aliases (:node-aliases graph)
        expansions (:expansions graph)]
    {:graph true
     :nodes (mapv (fn [[id label]]
                    (cond-> {:id (node-id-string id)
                             :label (str label)
                             :kind (node-kind label)}
                      (contains? values id)
                      (assoc :value (value-summary (get values id)))

                      (contains? aliases id)
                      (assoc :aliases
                             (mapv node-id-string (get aliases id)))

                      (contains? expansions id)
                      (assoc :expandable true
                             :expansion-id (node-id-string id))))
                  nodes)
     :edges (mapv (fn [[from to]]
                    {:from (node-id-string from)
                     :to (node-id-string to)})
                  (:edges graph))}))

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
    (swap! session assoc-in [:xr :traces trace-id] request)
    {:trace-id trace-id
     :interval-ms (:interval-ms request)
     :request request
     :graph (graph->json graph)}))

(defn xr-trace-read
  [state {:keys [trace-id]}]
  (let [request (get-in state [:xr :traces trace-id])]
    (when-not request
      (throw (ex-info "xr trace not found" {:trace-id trace-id})))
    {:trace-id trace-id
     :request request
     :graph (graph->json (runtime/semantic-trace state request))}))

(defn xr-trace-expand
  [state command]
  {:graph (graph->json (runtime/semantic-expansion state command))})

(defn xr-extend-graph!
  [session {:keys [source client-id] :or {client-id default-xr-client-id}}]
  (when (str/blank? source)
    (throw (ex-info "missing source for xr graph extension" {})))
  (let [old-xr (:xr @session)
        sources (conj (vec (:sources old-xr)) source)
        program (str "(let-cell []\n"
                     (str/join "\n" sources)
                     "\n)")]
    (runtime/compile-source! session program)
    (swap! session assoc :xr (assoc old-xr
                                    :client-id client-id
                                    :sources sources
                                    :program program)))
  {:client-id client-id
   :graph (graph->json (:graph @session))})

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
     :graph (graph->json (:graph @session))
     :command command}))

(defn handle-command!
  [session {:keys [op] :as command}]
  (case op
    :xr/trace/install (xr-trace-install! session command)
    :xr/trace/read (xr-trace-read @session command)
    :xr/trace/expand (xr-trace-expand @session command)
    :xr/extend-graph (xr-extend-graph! session command)
    :xr/send-message (xr-send-message! session command)
    ;; Delegate existing runtime commands for convenience.
    (:result (runtime/handle-command! session command))))

(defn request-runtime
  [host port command]
  ((requiring-resolve 'graph.compiler-2-runtime-server/request)
   host
   port
   command))
