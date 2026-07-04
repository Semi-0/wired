(ns graph.compiler-2-runtime
  "Shared compiler-2 runtime session for socket clients."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [graph.compiler-2-runtime.block-model :as block-model]
            [graph.compiler-2-runtime.boundary :as boundary]
            [graph.compiler-2-runtime.ids :as runtime-ids]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [graph.compiler-2-runtime.widget :as runtime-widget]
            [graph.vijual-compiler-2-demo :as demo]
            [propagators.cells.cell :as cell]
            [propagators.cells.cell-protocol :as cell-protocol]
            [propagators.cells.value :as value]
            [propagators.compile :as compile1]
            [propagators.compiler-2.application :as compiler-app]
            [propagators.compiler-2.env :as cenv]
            [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.compiler-2.main :as compiler]
            [propagators.compiler-2.parser :as compiler-parser]
            [propagators.core :as core]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.tms :as tms]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]
            [propagators.semantic-trace :as semantic-trace]
            [propagators.stdlib.prop :as stdlib-prop])
  (:import [java.io PushbackReader StringReader]
           [java.util.concurrent Executors TimeUnit]))

(defn new-session []
  (atom nil))

(def default-xr-client-id runtime-ids/default-xr-client-id)

(def ^:private external-source-client-id runtime-ids/external-source-client-id)

(defn- stable-node-id [& parts]
  (apply runtime-ids/stable-node-id parts))

(defn- runtime-graph-id []
  (runtime-ids/runtime-graph-id))

(defn- boundary-outbox-id []
  (runtime-ids/boundary-outbox-id))

(defn- receipt-slot-key [effect-id]
  (runtime-ids/receipt-slot-key effect-id))

(defn- effect-slot-key [effect-id]
  (runtime-ids/effect-slot-key effect-id))

(defn- daemon-executor
  [name]
  (Executors/newSingleThreadScheduledExecutor
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r]
       (doto (Thread. ^Runnable r name)
         (.setDaemon true))))))

(defn- install-runtime-protocols
  [n]
  (-> n
      (compile1/install-and-run (cell-protocol/install-cell-protocol))
      (compile1/install-and-run (cell-protocol/install-behavior-protocol))
      (compile1/install-and-run (cell-protocol/install-tms-distributed-protocol))))

(defn- runtime-base-net []
  (install-runtime-protocols net/empty-net))

(defn- runtime-compiler-env []
  ((requiring-resolve
    'propagators.compiler-2.behavior/bind-behavior-operators)
   (compiler-helpers/default-env)))

(declare empty-graph
         empty-state
         perform-boundary-effects
         runtime-env
         settle-application-props)

(defn- compiled-state
  [source]
  (let [graph-id (runtime-graph-id)
        base-state (empty-state)
        base-net (-> (:program/net base-state)
                     (nb/ensure-cell (boundary-outbox-id))
                     (nb/install-cell graph-id
                                      (semantic-trace/graph-union (empty-graph))
                                      (semantic-trace/graph-union (empty-graph))))
        env (runtime-env base-state
                         (:program/env base-state)
                         graph-id
                         default-xr-client-id)
        compiled (compiler/compile-source source
                                          env
                                          {:net base-net
                                           :seed [:runtime/xr source]})
        network0 (nb/run-propagators (:net compiled) (:props compiled))
        [tasks network1] (core/eval-cells
                          [(message graph-id
                                    (semantic-repl/compiled-semantic-graph
                                     compiled
                                     network0))]
                          network0)
        network2 (core/run-tasks tasks network1)
        network (settle-application-props network2 (:props compiled))
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
     :xr {:launched {}}
     :graph graph}))

(defn- empty-state []
  {:network (install-runtime-protocols net/empty-net)
   :program/net (runtime-base-net)
   :program/env (runtime-compiler-env)
   :program/graph {:nodes {} :edges [] :values {} :expansions {}}
   :program/results {}
   :program/epoch 0
   :runtime/commit-tick 0
   :block-order []
   :next-order 0
   :traces {}
   :xr {:launched {}}
   :tuis {}})

(defn- ensure-session-state! [session]
  (when-not @session
    (reset! session (empty-state)))
  @session)

(defn compile-source!
  [session source]
  (doseq [{:keys [stop]} (vals (:traces @session))]
    (when stop (stop)))
  (let [state (perform-boundary-effects
               (assoc (compiled-state source) :traces {}))]
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

(defn- matching-label?
  [requested actual]
  (and requested actual (= (str requested) (str actual))))

(defn- resolve-cell-row
  [state {:keys [cell-id label]}]
  (let [cells (list-cells state)]
    (or (some #(when (= cell-id (:cell-id %)) %) cells)
        (some #(when (matching-label? label (:label %)) %) cells)
        (throw (ex-info "cell not found" {:cell-id cell-id :label label})))))

(defn- trace-cell-id-for-label
  [state label]
  (or (some-> (cenv/lookup (:program/env state) (symbol (str label)))
              cenv/binding-id)
      (some (fn [[id entry]]
              (when (and (cell/cell? entry)
                         (matching-label? label (get (labels state) id)))
                id))
            (net/net-env (:program/net state)))))

(defn read-cell
  [state request]
  (resolve-cell-row (require-state state) request))

(declare all-blocks
         append-tui-block!
         edit-tui-block!
         install-block-slots
         read-tui-view
         semantic-trace)

(defn- block-by-index
  [state client-id index]
  (block-model/block-by-index state client-id index))

(defn- update-block
  [state client-id index f]
  (block-model/update-block state client-id index f))

(defn- write-block-value
  [state block _epoch payload]
  (let [current (net/network-cell-strongest (:network state) (:text-id block))]
    (if (= current payload)
      state
      (let [[tasks n1] (core/eval-cells [(message (:text-id block) payload)]
                                        (:network state))
            n2 (core/run-tasks tasks n1)]
        (assoc state :network n2)))))

(defn- block-by-text-id
  [state text-id]
  (block-model/block-by-text-id state text-id))

(defn- block-by-display-id
  [state display-id]
  (block-model/block-by-display-id state display-id))

(defn- next-global-order
  [state]
  (block-model/next-global-order state))

(defn- assign-source-order
  [state client-id index]
  (block-model/assign-source-order state client-id index))

(defn- source-blocks
  [state]
  (block-model/source-blocks state))

(defn- block-text
  [state block]
  (block-model/block-text state block))

(defn- compiled-result-value
  [compiled network]
  (let [result-id (:cell compiled)
        result (net/network-cell-strongest network result-id)]
    (cond
      (value/unusable? result) value/nothing
      (net/network? result) value/nothing
      :else result)))

(defn- empty-graph []
  {:nodes {} :edges [] :values {} :expansions {}})

(defn- namespace-graph
  [graph prefix]
  (letfn [(rename [node-id]
            (if (keyword? node-id)
              (keyword (namespace node-id)
                       (str prefix "/" (name node-id)))
              [prefix node-id]))]
    (let [rename-alias (fn [v]
                         (cond
                           (nil? v) #{}
                           (set? v) (set (map rename v))
                           (sequential? v) (set (map rename v))
                           :else #{(rename v)}))]
    (-> graph
        (update :nodes update-keys rename)
        (update :values update-keys rename)
        (update :edges (fn [edges]
                         (mapv (fn [[from to]]
                                 [(rename from) (rename to)])
                               edges)))
        (update :expansions (fn [expansions]
                              (into {}
                                    (map (fn [[node-id expansion]]
                                           [(rename node-id)
                                            (namespace-graph expansion prefix)]))
                                    expansions)))
        (update :node-aliases (fn [aliases]
                                (into {}
                                      (map (fn [[k v]] [k (rename-alias v)]))
                                      aliases)))))))

(defn- valid-client-id? [client-id]
  (block-model/valid-client-id? client-id))

(defn- declared-slot-parent-id
  [network block-id slot-key]
  (some->> (get (obj/accessor-declarations-for network block-id) slot-key)
           keys
           (sort-by pr-str)
           first))

(defn- instance-block-head-id
  [network instance-id]
  (let [instance-value (net/network-cell-strongest network instance-id)
        instance-id (if (ids/node-id? instance-value)
                      instance-value
                      instance-id)]
    (when-let [blocks-id (declared-slot-parent-id network
                                                  instance-id
                                                  :instance/blocks)]
      (net/network-cell-strongest network blocks-id))))

(defn- block-at-slot-id
  [network instance-id index-id slot-key]
  (let [wanted-index (net/network-cell-strongest network index-id)
        first-block-id (instance-block-head-id network instance-id)]
    (loop [block-id first-block-id
           seen #{}]
      (when (and (ids/node-id? block-id)
                 (not (contains? seen block-id)))
        (let [index-cell-id (declared-slot-parent-id network
                                                     block-id
                                                     :block/index)
              slot-cell-id (declared-slot-parent-id network block-id slot-key)
              next-cell-id (declared-slot-parent-id network block-id :cdr)
              block-index (when index-cell-id
                            (net/network-cell-strongest network index-cell-id))]
          (if (= wanted-index block-index)
            slot-cell-id
            (recur (when next-cell-id
                     (net/network-cell-strongest network next-cell-id))
                   (conj seen block-id))))))))

(defn- block-at-text-id
  [network instance-id index-id]
  (block-at-slot-id network instance-id index-id :block/text))

(defn- block-at-display-id
  [network instance-id index-id]
  (block-at-slot-id network instance-id index-id :block/display))

(declare p:tui-write-request tui-write-effect-request)

(defn- block-target-operator
  [outbox-id instance-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[index-id maybe-out] (vec arg-ids)
            target-id (or maybe-out out-id)
            text-id (and instance-id
                         index-id
                         (block-at-text-id network instance-id index-id))]
        (when-not (and (#{1 2} (count arg-ids)) text-id)
          (throw (ex-info "block expects a known block index"
                          {:arg-ids arg-ids
                           :index (when index-id
                                    (net/network-cell-strongest network
                                                              index-id))})))
        (let [network (nb/ensure-cell network outbox-id)
              [read-prop n1] ((stdlib-prop/id text-id target-id) network)
              [write-prop n2] ((p:tui-write-request target-id
                                                     text-id
                                                     outbox-id)
                               n1)]
          [n2 [read-prop write-prop] target-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 1 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[index-id maybe-out] (vec arg-ids)
             target-id (or maybe-out out-id)
             text-id (and instance-id
                          index-id
                          (block-at-text-id current-net instance-id index-id))
             source-value (when text-id
                            (net/network-cell-strongest current-net text-id))
             target-value (net/network-cell-strongest current-net target-id)
             write-message (when (and text-id
                                      (not (value/nothing? target-value)))
                             (let [epoch (or (:program/epoch
                                              (net/net-dict-or-empty current-net))
                                             0)
                                   effect-id [:tui/write-block
                                              text-id
                                              epoch
                                              (hash target-value)]]
                               (message outbox-id
                                        (obj/compound-object
                                         {(effect-slot-key effect-id)
                                          (tui-write-effect-request effect-id
                                                                    text-id
                                                                    target-value
                                                                    epoch)}))))]
         (when-not (#{1 2} (count arg-ids))
           (throw (ex-info "block expects index and optional output"
                           {:arg-ids arg-ids})))
         (cond-> []
           text-id
           (conj (message target-id source-value))

           write-message
           (conj write-message))))}))

(defn- tui-write-effect-request
  [effect-id text-id payload epoch]
  (boundary/tui-write-effect-request effect-id text-id payload epoch))

(defn- tui-display-effect-request
  [effect-id display-id payload tick]
  (boundary/tui-display-effect-request effect-id display-id payload tick))

(defn- p:tui-write-request
  [source-id text-id outbox-id]
  (prop/construct-propagator
   (fn [_inputs _outputs network]
     (let [payload (net/network-cell-strongest network source-id)
           epoch (or (:program/epoch (net/net-dict-or-empty network)) 0)
           effect-id [:tui/write-block text-id epoch (hash payload)]]
       (if (value/nothing? payload)
         []
         [(message outbox-id
                   (obj/compound-object
                    {(effect-slot-key effect-id)
                     (tui-write-effect-request effect-id
                                               text-id
                                               payload
                                               epoch)}))])))
   [source-id]
   [outbox-id]))

(defn- p:tui-display-request
  [source-id display-id outbox-id]
  (prop/construct-propagator
   (fn [_inputs _outputs network]
     (let [payload (net/network-cell-strongest network source-id)
           dict (net/net-dict-or-empty network)
           tick (or (:runtime/commit-tick dict)
                    (:program/epoch dict)
                    0)
           effect-id [:tui/write-display display-id tick (hash payload)]]
       (if (value/nothing? payload)
         []
         [(message outbox-id
                   (obj/compound-object
                    {(effect-slot-key effect-id)
                     (tui-display-effect-request effect-id
                                                 display-id
                                                 payload
                                                 tick)}))])))
   [source-id]
   [outbox-id]))

(defn- trace-target-value
  [label source-id]
  {:trace/target true
   :trace/symbol label
   :node source-id
   :label label})

(defn- trace-target-operator []
  (with-meta
    (fn [network arg-ids out-id]
      (let [[label-id source-id maybe-out] (vec arg-ids)
            target-id (or maybe-out out-id)]
        (when-not (and label-id source-id (#{2 3} (count arg-ids)))
          (throw (ex-info "trace-target expects label, source, and optional output"
                          {:arg-ids arg-ids})))
        (let [[prop-id n]
              ((prop/construct-propagator
                (fn [_inputs _outputs current-net]
                  (let [label (net/network-cell-strongest current-net label-id)]
                    (if (value/unusable? label)
                      []
                      [(message target-id
                                (trace-target-value label source-id))])))
                [label-id]
                [target-id])
               network)]
          [n [prop-id] target-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 2 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[label-id source-id maybe-out] (vec arg-ids)
             target-id (or maybe-out out-id)
             label (net/network-cell-strongest current-net label-id)]
         (when-not (and label-id source-id (#{2 3} (count arg-ids)))
           (throw (ex-info "trace-target expects label, source, and optional output"
                           {:arg-ids arg-ids})))
         (if (value/unusable? label)
           []
           [(message target-id (trace-target-value label source-id))])))}))

(defn- block-at-operator [outbox-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[instance-id index-id maybe-out] (vec arg-ids)
            target-id (or maybe-out out-id)
            text-id (block-at-text-id network instance-id index-id)]
        (when-not (and (#{2 3} (count arg-ids)) text-id)
          (throw (ex-info "block-at expects a known instance and index"
                          {:arg-ids arg-ids
                           :index (net/network-cell-strongest network
                                                             index-id)})))
        (let [network (nb/ensure-cell network outbox-id)
              [read-prop n1] ((stdlib-prop/id text-id target-id) network)
              [write-prop n2] ((p:tui-write-request target-id
                                                     text-id
                                                     outbox-id)
                               n1)]
          [n2 [read-prop write-prop] target-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 2 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[instance-id index-id maybe-out] (vec arg-ids)
             target-id (or maybe-out out-id)
             text-id (block-at-text-id current-net instance-id index-id)
             source-value (when text-id
                            (net/network-cell-strongest current-net
                                                        text-id))
             target-value (net/network-cell-strongest current-net target-id)
             write-message (when (and text-id
                                      (not (value/nothing? target-value)))
                             (let [epoch (or (:program/epoch
                                              (net/net-dict-or-empty current-net))
                                             0)
                                   effect-id [:tui/write-block
                                              text-id
                                              epoch
                                              (hash target-value)]]
                               (message outbox-id
                                        (obj/compound-object
                                         {(effect-slot-key effect-id)
                                          (tui-write-effect-request effect-id
                                                                    text-id
                                                                    target-value
                                                                    epoch)}))))]
         (when-not (#{2 3} (count arg-ids))
           (throw (ex-info "block-at expects instance, index, and optional output"
                           {:arg-ids arg-ids})))
         (cond-> []
           text-id
           (conj (message target-id source-value))

           write-message
           (conj write-message))))}))

(defn- be-block-at-operator [outbox-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[instance-id index-id source-id] (vec arg-ids)
            display-id (block-at-display-id network instance-id index-id)]
        (when-not (and (= 3 (count arg-ids)) display-id)
          (throw (ex-info "be:block-at expects a known instance, index, and source"
                          {:arg-ids arg-ids
                           :index (net/network-cell-strongest network
                                                             index-id)})))
        (let [network (nb/ensure-cell network outbox-id)
              [write-prop n] ((p:tui-display-request source-id
                                                     display-id
                                                     outbox-id)
                              network)]
          [n [write-prop] source-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 2 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[instance-id index-id source-id] (vec arg-ids)
             source-id (or source-id out-id)
             display-id (when (and instance-id index-id)
                          (block-at-display-id current-net
                                               instance-id
                                               index-id))
             target-value (when source-id
                            (net/network-cell-strongest current-net source-id))
             tick (or (:runtime/commit-tick (net/net-dict-or-empty current-net))
                      (:program/epoch (net/net-dict-or-empty current-net))
                      0)
             write-message (when (and display-id
                                      source-id
                                      (not (value/nothing? target-value)))
                             (let [effect-id [:tui/write-display
                                              display-id
                                              tick
                                              (hash target-value)]]
                               (message outbox-id
                                        (obj/compound-object
                                         {(effect-slot-key effect-id)
                                          (tui-display-effect-request effect-id
                                                                      display-id
                                                                      target-value
                                                                      tick)}))))]
         (when-not (= 3 (count arg-ids))
           (throw (ex-info "be:block-at expects instance, index, and source"
                           {:arg-ids arg-ids})))
         (cond-> []
           write-message
           (conj write-message))))}))

(defn- instance-operator []
  (with-meta
    (fn [network arg-ids out-id]
      (let [[instance-id maybe-out] (vec arg-ids)
            target-id (or maybe-out out-id)]
        (when-not (#{1 2} (count arg-ids))
          (throw (ex-info "instance expects instance and optional output"
                          {:arg-ids arg-ids})))
        (let [[read-prop n1] ((stdlib-prop/id instance-id target-id) network)
              [write-prop n2] ((stdlib-prop/id target-id instance-id) n1)]
          [n2 [read-prop write-prop] target-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 1 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[instance-id maybe-out] (vec arg-ids)
             target-id (or maybe-out out-id)
             source-value (net/network-cell-strongest current-net instance-id)
             target-value (net/network-cell-strongest current-net target-id)]
         (when-not (#{1 2} (count arg-ids))
           (throw (ex-info "instance expects instance and optional output"
                           {:arg-ids arg-ids})))
         (cond-> []
           (not (value/unusable? source-value))
           (conj (message target-id source-value))

           (not (value/unusable? target-value))
           (conj (message instance-id target-value)))))}))

(def ^:private jp->en
  {"猫" "cat"
   "犬" "dog"
   "水" "water"})

(def ^:private en->jp
  (into {} (map (fn [[jp en]] [en jp]) jp->en)))

(defn- translate-messages
  [network jp-id en-id]
  (let [jp (net/network-cell-strongest network jp-id)
        en (net/network-cell-strongest network en-id)
        en' (when (string? jp) (get jp->en jp))
        jp' (when (string? en) (get en->jp en))]
    (cond-> []
      en' (conj (message en-id en'))
      jp' (conj (message jp-id jp')))))

(defn- translate-operator []
  (with-meta
    (fn [network arg-ids out-id]
      (let [[jp-id en-id] (vec arg-ids)]
        (when-not (and jp-id en-id (= 2 (count arg-ids)))
          (throw (ex-info "translate expects Japanese and English cells"
                          {:arg-ids arg-ids})))
        (let [[prop-id n]
              ((prop/construct-propagator
                (prop/concrete-propagator
                 (fn [_inputs _outputs current-net]
                   (translate-messages current-net jp-id en-id)))
                [jp-id en-id]
                [jp-id en-id])
               network)]
          [n [prop-id] (or out-id en-id)])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (second (vec arg-ids)) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids _out-id]
       (let [[jp-id en-id] (vec arg-ids)]
         (when-not (and jp-id en-id (= 2 (count arg-ids)))
           (throw (ex-info "translate expects Japanese and English cells"
                           {:arg-ids arg-ids})))
         (translate-messages current-net jp-id en-id)))}))

(defn- trace-request-source
  [graph source-id source-value direction]
  (if (or (symbol? source-value)
          (string? source-value)
          (map? source-value)
          (seq? source-value))
    (semantic-trace/trace-request source-value direction)
    (if-let [label (some->> (get-in graph [:node-aliases source-id])
                            (#(cond
                                (set? %) %
                                (sequential? %) (set %)
                                (some? %) #{%}
                                :else #{}))
                            (keep (:nodes graph))
                            first)]
      (semantic-trace/trace-request label direction)
      {:node source-id :direction direction})))

(defn- p:runtime-trace-request
  [source-id direction-id graph-id out-id]
  (let [inputs (cond-> [source-id graph-id] direction-id (conj direction-id))]
    (prop/construct-propagator
     (fn [_inputs _outputs network]
       (let [direction (if direction-id
                         (net/network-cell-strongest network direction-id)
                         :upstream)
             graph (net/network-cell-strongest network graph-id)
             source-value (net/network-cell-strongest network source-id)]
         (if (or (value/unusable? direction)
                 (value/unusable? graph))
           []
           [(message out-id
                     (trace-request-source graph
                                           source-id
                                           source-value
                                           direction))])))
     inputs
     [out-id])))

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
        (let [[request-prop n1] ((p:runtime-trace-request source-id
                                                          direction-id
                                                          graph-id
                                                          request-id)
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
             request (trace-request-source graph
                                           source-id
                                           source-value
                                           direction)]
         (if (or (value/unusable? graph)
                 (value/unusable? direction))
           []
           [(message target-id
                     (semantic-trace/trace-graph
                      graph
                      request))])))}))

(defn- xr-effect-request
  [effect-id trace-graph receipt-id epoch]
  (boundary/xr-effect-request effect-id trace-graph receipt-id epoch))

(defn- xr-receipt
  [request status]
  (boundary/xr-receipt request status))

(defn- p:xr-io-request
  [trace-id outbox-id receipt-id]
  (prop/construct-propagator
   (fn [_inputs _outputs network]
     (let [trace-graph (net/network-cell-strongest network trace-id)
           epoch (or (:program/epoch (net/net-dict-or-empty network)) 0)
           effect-id [:xr/launch-trace trace-id receipt-id epoch (hash trace-graph)]]
       (if (or (value/unusable? trace-graph)
               (not (semantic-trace/semantic-trace-graph? trace-graph)))
         []
         [(message outbox-id
                   (obj/compound-object
                    {(effect-slot-key effect-id)
                     (xr-effect-request effect-id
                                        trace-graph
                                        receipt-id
                                        epoch)}))])))
   [trace-id]
   [outbox-id]))

(defn- xr-io-operator [outbox-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[trace-id maybe-receipt-id] (vec arg-ids)
            receipt-id (or maybe-receipt-id out-id)]
        (when-not (and trace-id receipt-id (#{1 2} (count arg-ids)))
          (throw (ex-info "xr-io expects trace graph and optional receipt output"
                          {:arg-ids arg-ids})))
        (let [network* (-> network
                           (nb/ensure-cell outbox-id)
                           (nb/ensure-cell receipt-id))
              [prop-id n] ((p:xr-io-request trace-id outbox-id receipt-id)
                           network*)]
          [n [prop-id] receipt-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (second (vec arg-ids)) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[trace-id maybe-receipt-id] (vec arg-ids)
             receipt-id (or maybe-receipt-id out-id)
             trace-graph (net/network-cell-strongest current-net trace-id)
             epoch (or (:program/epoch (net/net-dict-or-empty current-net)) 0)
             effect-id [:xr/launch-trace trace-id receipt-id epoch (hash trace-graph)]]
         (if (or (value/unusable? trace-graph)
                 (not (semantic-trace/semantic-trace-graph? trace-graph)))
           []
           [(message outbox-id
                     (obj/compound-object
                      {(effect-slot-key effect-id)
                       (xr-effect-request effect-id
                                          trace-graph
                                          receipt-id
                                          epoch)}))])))}))

(defn- all-blocks [state]
  (block-model/all-blocks state))

(defn- seed-program-block-cell [program-net runtime-net id]
  (let [v (net/network-cell-strongest runtime-net id)]
    (if (or (value/unusable? v)
            (semantic-trace/semantic-trace-graph? v))
      (nb/ensure-cell program-net id)
      (nb/install-cell program-net id v v))))

(defn- install-instance-slots
  [n {:keys [instance-id blocks-id]}]
  (let [[blocks-prop n] ((obj/p:slot :instance/blocks blocks-id instance-id) n)]
    (nb/run-propagators n [blocks-prop])))

(defn- seed-program-instances [state program-net]
  (reduce-kv
   (fn [n _client-id {:keys [instance-id blocks-id]}]
     (-> n
         (seed-program-block-cell (:network state) instance-id)
         (seed-program-block-cell (:network state) blocks-id)
         (install-instance-slots {:instance-id instance-id
                                  :blocks-id blocks-id})))
   program-net
   (:tuis state)))

(defn- seed-program-blocks [state program-net]
  (reduce
   (fn [n block]
     (let [n0 (reduce #(seed-program-block-cell %1 (:network state) %2)
                      n
                      [(:block-id block) (:index-id block)
                       (:next-id block) (:text-id block)
                       (:display-id block)])]
       (install-block-slots n0 block)))
   program-net
   (all-blocks state)))

(defn- runtime-env [state base-env graph-id current-client-id]
  (as-> base-env env
    (cenv/bind-at env 'block-at (block-at-operator (boundary-outbox-id)) 0)
    (cenv/bind-at env 'be:block-at (be-block-at-operator (boundary-outbox-id)) 0)
    (cenv/bind-at env 'instance (instance-operator) 0)
    (cenv/bind-at env 'trace-target (trace-target-operator) 0)
    (cenv/bind-at env 'trace (trace-operator graph-id) 0)
    (cenv/bind-at env 'xr-io (xr-io-operator (boundary-outbox-id)) 0)
    (cenv/bind-at env 'io:xr (xr-io-operator (boundary-outbox-id)) 0)
    (cenv/bind-at env 'slider-io
                  (runtime-widget/slider-io-operator (boundary-outbox-id))
                  0)
    (cenv/bind-at env 'slider-panel-io
                  (runtime-widget/slider-panel-io-operator (boundary-outbox-id))
                  0)
    (cenv/bind-at env 'io:slider
                  (runtime-widget/io-slider-operator (boundary-outbox-id))
                  0)
    (cenv/bind-at env 'io:slider-panel
                  (runtime-widget/io-slider-panel-operator (boundary-outbox-id))
                  0)
    (cenv/bind-at env 'translate (translate-operator) 0)
    (if-let [instance-id (get-in state [:tuis current-client-id :instance-id])]
      (cenv/bind-at env 'block (block-target-operator (boundary-outbox-id)
                                                      instance-id)
                    0)
      env)
    (reduce-kv (fn [e client-id {:keys [instance-id]}]
                 (cenv/bind-at e
                               (symbol client-id)
                               (cenv/cell-binding instance-id)
                               0))
               env
               (:tuis state))
    (if-let [instance-id (get-in state [:tuis current-client-id :instance-id])]
      (cenv/bind-at env '% (cenv/cell-binding instance-id) 0)
      env)))

(defn- retained-application-props
  [program-net]
  (vec (get (net/net-dict-or-empty program-net)
            compiler-app/apply-application-props-key
            #{})))

(defn- settle-application-props
  [program-net current-props]
  (let [props (vec (distinct (concat (retained-application-props program-net)
                                     current-props)))]
    (-> program-net
        (nb/run-propagators props)
        (nb/run-propagators props))))

(defn- top-level-form-head
  [source]
  (try
    (let [form (compiler-parser/read-form source)]
      (when (seq? form)
        (first form)))
    (catch Throwable _
      nil)))

(defn- top-level-declaration? [source]
  (contains? '#{def def-cell def-cells def-net def-constraint
                <-> -> block block-at be:block-at translate
                xr-io io:xr
                slider-io slider-panel-io io:slider io:slider-panel
                behavior behavior-cell}
             (top-level-form-head source)))

(defn- trace-source? [source]
  (= 'trace (top-level-form-head source)))

(def ^:private source-reader-eof (Object.))

(defn- read-source-forms
  [source]
  (let [source (str/replace source
                            #"\(\s*::(?=\s)"
                            (str "(" compiler-parser/network-marker))
        reader (PushbackReader. (StringReader. source))]
    (loop [forms []]
      (let [form (edn/read {:eof source-reader-eof} reader)]
        (if (identical? source-reader-eof form)
          (do
            (when-not (seq forms)
              (throw (ex-info "empty compiler-2 source" {:source source})))
            forms)
          (recur (conj forms form)))))))

(defn- trace-form? [source]
  (try
    (let [form (compiler-parser/read-form source)]
      (boolean
       (some (fn [x]
               (and (seq? x)
                    (= 'trace (first x))))
             (tree-seq coll? seq form))))
    (catch Throwable _
      false)))

(defn- normalize-trace-source
  [source]
  (try
    (let [form (compiler-parser/read-form source)]
      (pr-str
       (walk/postwalk
        (fn [form]
          (if (and (seq? form)
                   (= 'trace (first form))
                   (symbol? (second form)))
            (cons 'trace
                  (cons (list 'trace-target
                              (name (second form))
                              (second form))
                        (nnext form)))
            form))
        form)))
    (catch Throwable _
      source)))

(defn- auto-output-source [state block source]
  (if (or (top-level-declaration? source)
          (nil? (block-by-index state (:client-id block) (inc (:index block)))))
    source
    (format "(let-cell [__runtime_out]
               (<-> %s __runtime_out)
               (block-at %% %d __runtime_out)
               __runtime_out)"
            source
            (inc (:index block)))))

(defn- compile-program-form
  [state block epoch source]
  (try
    (let [source (normalize-trace-source source)
          source (auto-output-source state block source)
          needs-live-graph? (trace-form? source)
          top-level-trace? (trace-source? source)
          graph-id (runtime-graph-id)
          env (runtime-env state (:program/env state) graph-id (:client-id block))
          program-net-input (nb/install-cell (:program/net state)
                                             graph-id
                                             (:graph state)
                                             (:graph state))
          compiled (compiler/compile-source
                    source
                    env
                    {:net program-net-input
                     :seed [:runtime/block (:order block) (:epoch block)]})
          program-net0 (nb/run-propagators (:net compiled) (:props compiled))
          program-net2 (if (and needs-live-graph?
                                (not top-level-trace?))
                         (let [graph* (namespace-graph
                                       (semantic-repl/compiled-semantic-graph
                                        compiled
                                        program-net0)
                                       (str (:client-id block) "-"
                                            (:order block)))
                               graph (assoc (semantic-trace/graph-union
                                             (:graph state)
                                             graph*)
                                            :source source)
                               [tasks program-net1]
                               (core/eval-cells [(message graph-id graph)]
                                                (nb/ensure-cell program-net0
                                                                graph-id))]
                           (core/run-tasks tasks program-net1))
                         program-net0)
          program-net (settle-application-props program-net2 (:props compiled))
          graph (if top-level-trace?
                  (assoc (:graph state) :source source)
                  (let [graph* (namespace-graph
                                (semantic-repl/compiled-semantic-graph
                                 compiled
                                 program-net)
                                (str (:client-id block) "-" (:order block)))]
                    (assoc (semantic-trace/graph-union
                            (:graph state)
                            graph*)
                           :source source)))
          result (compiled-result-value compiled program-net)
          result-key [(:client-id block) (:index block)]]
      (-> state
          (assoc :program/net program-net
                 :program/env (:env compiled)
                 :program/graph graph
                 :compiled compiled
                 :compiled-network program-net
                 :graph graph
                 :source source)
          (assoc-in [:program/results result-key]
                    {:result result :compiled compiled})))
    (catch Throwable t
      (assoc-in state
                [:program/results [(:client-id block) (:index block)]]
                {:error (ex-message t)
                 :data (ex-data t)}))))

(defn- rebuild-block
  [state epoch block]
  (let [source (block-text state block)]
    (if (or (value/unusable? source)
            (not (string? source))
            (trace-source? source))
      state
      (compile-program-form state block epoch source))))

(defn- block-compile-error?
  [state block]
  (some? (get-in state
                 [:program/results [(:client-id block) (:index block)] :error])))

(defn- retry-errored-blocks
  [state epoch source-blocks]
  (reduce (fn [s block]
            (if (block-compile-error? s block)
              (rebuild-block s epoch block)
              s))
          state
          source-blocks))

(defn- retry-expression-blocks
  [state epoch source-blocks]
  (reduce (fn [s block]
            (let [source (block-text s block)]
              (if (and (string? source)
                       (not (top-level-declaration? source)))
                (rebuild-block s epoch block)
                s)))
          state
          source-blocks))

(defn- rebuild-program-state
  [state]
  (let [epoch (inc (or (:program/epoch state) 0))
        source-blocks (source-blocks state)
        base (assoc state
                    :program/net (nb/install-cell
                                  (net/assoc-net-dict-entry
                                   (nb/ensure-cell
                                    (seed-program-blocks
                                     state
                                     (seed-program-instances state
                                                             (runtime-base-net)))
                                    (boundary-outbox-id))
                                   :program/epoch
                                   epoch)
                                  (runtime-graph-id)
                                  (semantic-trace/graph-union (empty-graph))
                                  (semantic-trace/graph-union (empty-graph)))
                    :program/env (runtime-compiler-env)
                    :program/graph (empty-graph)
                    :program/results {}
                    :program/epoch epoch
                    :compiled nil
                    :compiled-network net/empty-net
                    :graph (empty-graph)
                    :source nil)]
    (as-> (reduce (fn [s block]
                    (rebuild-block s epoch block))
                  base
                  source-blocks) s
      (retry-errored-blocks s epoch source-blocks)
      (retry-expression-blocks s epoch source-blocks)
      (reduce (fn [s block]
                (let [source (block-text s block)]
                  (if (and (string? source) (trace-source? source))
                    (compile-program-form s block epoch source)
                    s)))
              s
              source-blocks))))

(defn- outbox-effects
  [program-net]
  (if-not (contains? (net/net-env program-net) (boundary-outbox-id))
    []
    (let [outbox (net/network-cell-strongest program-net (boundary-outbox-id))]
      (if (value/unusable? outbox)
        []
        (->> (obj/public-slot-keys outbox)
             (keep (fn [slot-key]
                     (let [request (obj/slot-value outbox slot-key)]
                       (when (:boundary/effect request)
                         request))))
             vec)))))

(defn- receipt-message
  [request status]
  (message (:boundary/receipt-id request)
           (obj/compound-object
            {(receipt-slot-key (:boundary/id request))
             (xr-receipt request status)})))

(defn- record-xr-launch
  [state request]
  (if (get-in state [:xr :launched (:boundary/id request)])
    state
    (let [receipt (receipt-message request :delivered)
          [tasks program-net] (core/eval-cells [receipt] (:program/net state))]
      (-> state
          (assoc :program/net program-net)
          (update-in [:xr :launched]
                     (fnil assoc {})
                     (:boundary/id request)
                     {:request request
                      :receipt (:value receipt)})
          (update-in [:xr :effects]
                     (fnil conj [])
                     request)
          (assoc :program/pending-after-effects tasks)))))

(defn- record-tui-write
  [state request]
  (let [text-id (get-in request [:boundary/target :text-id])
        payload (:boundary/payload request)]
    (if-let [block (block-by-text-id state text-id)]
      (if (some? (:order block))
        state
        (-> state
            (write-block-value block (:boundary/epoch request) payload)
            (update-in [:tui :effects] (fnil conj []) request)))
      state)))

(defn- display-behavior-update
  [display-id tick payload]
  (behavior/retained-value [:tui/display display-id]
                           tick
                           payload
                           #{[:tui/display display-id tick]}))

(defn- write-block-display-value
  [state block tick payload]
  (let [update (display-behavior-update (:display-id block) tick payload)
        [tasks n1] (core/eval-cells [(message (:display-id block) update)]
                                    (:network state))
        n2 (core/run-tasks tasks n1)]
    (assoc state :network n2)))

(defn- record-tui-display
  [state request]
  (let [display-id (get-in request [:boundary/target :display-id])
        payload (:boundary/payload request)
        tick (or (:boundary/tick request)
                 (:boundary/epoch request)
                 0)]
    (if-let [block (block-by-display-id state display-id)]
      (-> state
          (write-block-display-value block tick payload)
          (update-in [:tui :effects] (fnil conj []) request))
      state)))

(defn- display-widget-value
  [v]
  (when-not (value/unusable? v)
    (semantic-repl/display-cell-value v)))

(defn- widget-node-id
  [widget-id]
  (stable-node-id :xr :widget widget-id))

(defn- graph-cell-node-id
  [graph cell-id]
  (or (first (sort-by pr-str (get-in graph [:node-aliases cell-id])))
      (stable-node-id :xr :widget-cell cell-id)))

(defn- graph-cell-node-ids
  [graph cell-id]
  (let [aliases (get-in graph [:node-aliases cell-id])]
    (if (seq aliases)
      aliases
      #{})))

(defn- ensure-graph-cell-node
  [graph label cell-id]
  (let [node-id (graph-cell-node-id graph cell-id)]
    (-> graph
        (assoc-in [:nodes node-id] (or label "cell"))
        (update-in [:node-aliases cell-id] (fnil conj #{}) node-id))))

(defn- assoc-graph-cell-value
  [state cell-id]
  (let [label (get (labels state) cell-id)
        graph0 (ensure-graph-cell-node (:graph state) label cell-id)
        node-id (graph-cell-node-id graph0 cell-id)
        strongest (net/network-cell-strongest (:program/net state) cell-id)
        graph1 (if (value/unusable? strongest)
                 (update graph0 :values dissoc node-id)
                 (assoc-in graph0 [:values node-id] strongest))]
    (assoc state
           :graph graph1
           :program/graph graph1)))

(defn- program-strongest-snapshot
  [program-net]
  (into {}
        (keep (fn [[id entry]]
                (when (cell/cell? entry)
                  [id (cell/cell-strongest entry)])))
        (net/net-env program-net)))

(def ^:private missing-cell ::missing-cell)

(defn- changed-cell-ids
  [before after]
  (->> (set/union (set (keys before)) (set (keys after)))
       (filter (fn [id]
                 (not (value/cell-value-equal?
                       (get before id missing-cell)
                       (get after id missing-cell)))))
       (sort-by pr-str)
       vec))

(defn- record-runtime-transaction
  [state before-program-net]
  (let [before (if before-program-net
                 (program-strongest-snapshot before-program-net)
                 {})
        after (program-strongest-snapshot (:program/net state))
        graph (:graph state)
        changed (changed-cell-ids before after)
        pairs (keep (fn [cell-id]
                      (let [node-ids (seq (graph-cell-node-ids graph cell-id))]
                        (when node-ids
                          [cell-id node-ids])))
                    changed)
        cells (mapv first pairs)
        nodes (->> pairs
                   (mapcat second)
                   distinct
                   vec)]
    (assoc state
           :runtime/changed-cells cells
           :runtime/changed-node-ids nodes)))

(defn- widget-channel-view
  [state {:keys [widget/channel widget/view-cell widget/event-cell widget/view-value]}]
  (let [labels (labels state)]
    {:channel (str channel)
     :view-cell view-cell
     :view-cell-id (pr-str view-cell)
     :view-label (get labels view-cell)
     :event-cell event-cell
     :event-cell-id (pr-str event-cell)
     :event-label (get labels event-cell)
     :current (display-widget-value view-value)}))

(defn- widget-ui
  [widget-type widget-id channels]
  {:kind "widget"
   :type (name widget-type)
   :widget-id (str widget-id)
   :channels (mapv #(select-keys %
                                 [:channel
                                  :view-label
                                  :event-label
                                  :current])
                   channels)})

(defn- add-widget-to-graph
  [state widget-type widget-id channels]
  (let [node-id (widget-node-id widget-id)
        graph0 (:graph state)
        graph1 (assoc-in graph0 [:nodes node-id] (str widget-id))
        graph2 (assoc-in graph1 [:node-ui node-id] (widget-ui widget-type
                                                              widget-id
                                                              channels))
        graph3 (reduce
                (fn [graph {:keys [view-cell event-cell view-label event-label]}]
                  (let [view-node (graph-cell-node-id graph view-cell)
                        event-node (graph-cell-node-id graph event-cell)]
                    (-> graph
                        (ensure-graph-cell-node view-label view-cell)
                        (ensure-graph-cell-node event-label event-cell)
                        (update :edges (fnil into [])
                                [[view-node node-id]
                                 [node-id event-node]]))))
                graph2
                channels)]
    (assoc state
           :graph graph3
           :program/graph graph3)))

(defn- record-widget-register
  [state request]
  (let [{:widget/keys [type id channels]} (:boundary/payload request)
        widget-id (str id)
        channels* (mapv #(widget-channel-view state %) channels)
        channel-map (into {}
                          (map (fn [channel]
                                 [(:channel channel) channel]))
                          channels*)]
    (-> state
        (assoc-in [:xr :widgets widget-id]
                  {:id widget-id
                   :type (name type)
                   :channels channel-map
                   :epoch (:boundary/epoch request)})
        (update-in [:xr :effects] (fnil conj []) request)
        (add-widget-to-graph type widget-id channels*))))

(defn- graph-size
  [request]
  (let [payload (:boundary/payload request)
        graph (if (semantic-trace/semantic-trace-graph? payload)
                payload
                (:graph payload))]
    (+ (count (:nodes graph))
       (count (:edges graph)))))

(defn- delivery-key
  [request]
  (case [(:boundary/port request) (:boundary/kind request)]
    [:tui :tui/write-display]
    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/target request)
     (:boundary/epoch request)]

    [:tui :tui/write-block]
    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/target request)
     (:boundary/id request)
     (:boundary/epoch request)]

    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/receipt-id request)
     (:boundary/epoch request)]))

(defn- collapse-boundary-effects
  [requests]
  (->> requests
       (reduce (fn [acc request]
                 (update acc
                         (delivery-key request)
                         (fn [old]
                           (if (and old
                                    (>= (graph-size old) (graph-size request)))
                             old
                             request))))
               {})
       vals))

(defn- perform-boundary-effects
  [state]
  (reduce (fn [s request]
            (case [(:boundary/port request) (:boundary/kind request)]
              [:xr :xr/launch-trace] (record-xr-launch s request)
              [:xr :xr/widget-register] (record-widget-register s request)
              [:tui :tui/write-block] (record-tui-write s request)
              [:tui :tui/write-display] (record-tui-display s request)
              s))
          state
          (collapse-boundary-effects (outbox-effects (:program/net state)))))

(defn- refresh-program-graph
  [state]
  (if (and (:compiled state) (:program/net state))
    (let [fresh (semantic-repl/compiled-semantic-graph (:compiled state)
                                                       (:program/net state))
          graph (semantic-trace/graph-union (:graph state) fresh)]
      (assoc state
             :graph graph
             :program/graph graph
             :compiled-network (:program/net state)))
    state))

(defn run-runtime-cycle
  "Apply already-committed model state through propagation effects.

  Public for tests; command handlers should keep using the higher-level TUI/XR
  operations unless they are deliberately testing the runtime cycle boundary."
  [state]
  (perform-boundary-effects (refresh-program-graph state)))

(defn- next-widget-epoch
  [state widget-id]
  (inc (long (get-in state [:xr :widget-epochs widget-id] 0))))

(defn- next-runtime-commit-tick
  [state]
  (inc (long (or (:runtime/commit-tick state) 0))))

(defn- assoc-program-commit-tick
  [state tick]
  (assoc state
         :program/net
         (net/assoc-net-dict-entry (:program/net state)
                                   :runtime/commit-tick
                                   tick)))

(defn- widget-channel
  [state widget-id channel]
  (or (get-in state [:xr :widgets widget-id :channels channel])
      (throw (ex-info "xr widget channel not found"
                      {:widget-id widget-id
                       :channel channel}))))

(defn- annotate-widget-cell-values
  [state widget-id]
  (let [channels (vals (get-in state [:xr :widgets widget-id :channels]))
        cell-ids (distinct (mapcat (juxt :event-cell :view-cell) channels))]
    (reduce (fn [s cell-id]
              (if cell-id
                (assoc-graph-cell-value s cell-id)
                s))
            state
            cell-ids)))

(defn- apply-program-updates
  [state updates]
  (let [updates (vec (remove (comp nil? :cell-id) updates))
        n0 (reduce (fn [n {:keys [cell-id]}]
                     (nb/ensure-cell n cell-id))
                   (:program/net state)
                   updates)
        [tasks n1] (core/eval-cells
                    (mapv (fn [{:keys [cell-id update]}]
                            (message cell-id update))
                          updates)
                    n0)
        n2 (core/run-tasks tasks n1)
        n3 (settle-application-props n2 [])]
    (assoc state :program/net n3)))

(defn- replay-widget-updates
  [state {:keys [widget-id updates]}]
  (let [widget-id (str widget-id)
        channels (get-in state [:xr :widgets widget-id :channels])
        updates* (vec
                  (keep (fn [{:keys [channel value update]}]
                          (when-let [event-cell (get-in channels
                                                         [(str channel)
                                                          :event-cell])]
                            {:cell-id event-cell
                             :value value
                             :update update}))
                        updates))]
    (-> state
        (apply-program-updates updates*)
        (annotate-widget-cell-values widget-id))))

(defn- replay-runtime-input
  [state input]
  (let [state (if-let [tick (:runtime/commit-tick input)]
                (assoc-program-commit-tick state tick)
                state)]
    (case (:runtime/input input)
      (:cell-message :xr/message)
      (apply-program-updates state [(select-keys input [:cell-id :update])])

      :xr/widget-event
      (replay-widget-updates state input)

      state)))

(defn- replay-runtime-inputs
  [state]
  (reduce replay-runtime-input state (:runtime/inputs state)))

(defn commit-runtime-input
  "Commit an external cell message into the runtime model, then propagate/effect."
  [state input]
  (let [tick (or (:runtime/commit-tick input)
                 (next-runtime-commit-tick state))
        input (assoc input :runtime/commit-tick tick)
        state (-> state
                  (assoc :runtime/commit-tick tick)
                  (assoc-program-commit-tick tick))]
    (case (:runtime/input input)
    (:cell-message :xr/message)
    (let [before-net (:program/net state)
          cell-id (:cell-id input)
          update-value (:update input)
          n3 (:program/net
              (apply-program-updates state [{:cell-id cell-id
                                             :update update-value}]))]
      (-> state
          (assoc :program/net n3)
          (update :runtime/inputs (fnil conj []) input)
          run-runtime-cycle
          (record-runtime-transaction before-net)))

    :xr/widget-event
    (let [before-net (:program/net state)
          widget-id (str (:widget-id input))
          channel (str (or (:channel input) "value"))
          _widget-channel (widget-channel state widget-id channel)
          epoch (next-widget-epoch state widget-id)
          latest-values (assoc (get-in state [:xr :widget-latest widget-id] {})
                               channel
                               (:value input))
          channels (get-in state [:xr :widgets widget-id :channels])
          updates (vec
                   (keep (fn [[channel-name channel-info]]
                           (when (contains? latest-values channel-name)
                             {:channel channel-name
                              :cell-id (:event-cell channel-info)
                              :value (get latest-values channel-name)
                              :update (obj/compound-object
                                       {epoch (get latest-values channel-name)})}))
                         channels))
          n3 (:program/net (apply-program-updates state updates))]
      (-> state
          (assoc :program/net n3)
          (assoc-in [:xr :widget-epochs widget-id] epoch)
          (assoc-in [:xr :widget-latest widget-id] latest-values)
          (update :runtime/inputs (fnil conj [])
                  (assoc input
                         :runtime/input :xr/widget-event
                         :widget-id widget-id
                         :channel channel
                         :epoch epoch
                         :updates updates))
          run-runtime-cycle
          (annotate-widget-cell-values widget-id)
          (record-runtime-transaction before-net)))

    (throw (ex-info "unsupported runtime input" {:input input})))))

(defn commit-runtime-input!
  [session input]
  (swap! session commit-runtime-input input)
  @session)

(defn- rebuild-program!
  [session]
  (swap! session
         (fn [state]
           (let [before-net (:program/net state)]
             (-> state
                 rebuild-program-state
                 perform-boundary-effects
                 replay-runtime-inputs
                 run-runtime-cycle
                 (record-runtime-transaction before-net)))))
  @session)

(defn extend-source!
  "Append compiler source to the active runtime without replacing TUI state.

  The source may contain multiple top-level forms. Each form is installed as a
  hidden ordered source block, so definitions can extend the existing compiler
  environment the same way visible TUI blocks do.
  "
  [session {:keys [source client-id] :or {client-id default-xr-client-id}}]
  (ensure-session-state! session)
  (let [forms (read-source-forms source)
        installed (atom [])]
    (swap! session
           (fn [state]
             (let [start-order (next-global-order state)
                   start-index (count (:external-sources state))
                   blocks (mapv
                           (fn [offset form]
                             {:client-id external-source-client-id
                              :source-client-id client-id
                              :index (+ start-index offset)
                              :order (+ start-order offset)
                              :epoch 0
                              :source (pr-str form)
                              :external? true})
                           (range)
                           forms)]
               (reset! installed blocks)
               (-> state
                   (update :external-sources (fnil into []) blocks)
                   (assoc :next-order (+ start-order (count blocks)))))))
    (rebuild-program! session)
    {:client-id client-id
     :blocks @installed}))

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
  (cond
    (:node request)
    request

    (:label request)
    (if-let [cell-id (trace-cell-id-for-label state (:label request))]
      (-> request
          (assoc :node cell-id)
          (dissoc :label))
      request)

    :else
    (assoc request :label (:label (resolve-cell-row state request)))))

(defn semantic-trace
  [state request]
  (let [state (require-state state)
        request* (resolved-trace-request state request)]
    (trace-via-propagator (:graph state) request*)))

(defn semantic-expansion
  [state request]
  (let [graph (:graph (require-state state))]
    (or (semantic-repl/expansion graph request)
        (throw (ex-info "semantic expansion not found"
                        (select-keys request [:node :label]))))))

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
  [n {:keys [block-id index-id text-id display-id next-id]}]
  (let [[index-prop n] ((obj/p:slot :block/index index-id block-id) n)
        [text-prop n] ((obj/p:slot :block/text text-id block-id) n)
        [display-prop n] ((obj/p:slot :block/display display-id block-id) n)
        [next-prop n] ((obj/p:cdr next-id block-id) n)]
    (nb/run-propagators n [index-prop text-prop display-prop next-prop])))

(defn- validate-client-id!
  [client-id]
  (when-not client-id
    (throw (ex-info "missing client-id" {})))
  (when-not (valid-client-id? client-id)
    (throw (ex-info "invalid client-id for compiler env binding"
                    {:client-id client-id}))))

(defn- create-tui
  [client-id]
  (let [view-id (stable-node-id :tui client-id :view)
        instance-id (stable-node-id :tui client-id :instance)
        blocks-id (stable-node-id :tui client-id :blocks)]
    {:client-id client-id
     :view-id view-id
     :instance-id instance-id
     :blocks-id blocks-id
     :head-id nil
     :tail-id nil
     :next-index 0
     :client-count 0
     :blocks []}))

(defn- install-tui-instance
  [network {:keys [view-id instance-id blocks-id]}]
  (-> network
      (nb/ensure-cell view-id)
      (nb/install-cell instance-id instance-id instance-id)
      (nb/ensure-cell blocks-id)
      (install-instance-slots {:instance-id instance-id
                               :blocks-id blocks-id})))

(defn- ensure-tui!
  [session client-id]
  (validate-client-id! client-id)
  (let [state (ensure-session-state! session)]
    (or (get-in state [:tuis client-id])
        (let [tui (create-tui client-id)
              n (install-tui-instance (:network state) tui)]
          (swap! session #(-> %
                              (assoc :network n)
                              (assoc-in [:tuis client-id] tui)))
          tui))))

(defn register-tui!
  [session {:keys [client-id]}]
  (let [tui (ensure-tui! session client-id)]
    (swap! session update-in [:tuis client-id :client-count] (fnil inc 0))
    (let [tui (get-in @session [:tuis client-id])]
      {:client-id (:client-id tui)
       :view-id (pr-str (:view-id tui))
       :next-index (:next-index tui)})))

(defn- tui!
  [session client-id]
  (ensure-tui! session client-id)
  (get-in @session [:tuis client-id]))

(declare prepare-block-targets!)

(defn append-tui-block!
  [session {:keys [client-id text rebuild?] :or {rebuild? true} :as command}]
  (let [tui (tui! session client-id)
        index (:next-index tui)
        has-text? (contains? command :text)
        block {:block-id (ids/new-node-id)
               :index-id (ids/new-node-id)
               :text-id (ids/new-node-id)
               :display-id (ids/new-node-id)
               :next-id (ids/new-node-id)
               :index index
               :epoch 0}
        state @session
        n0 (-> (:network state)
               (nb/install-cell (:block-id block))
               (nb/install-cell (:index-id block) index index)
               ((if has-text?
                  #(nb/install-cell % (:text-id block) text text)
                  #(nb/install-cell % (:text-id block))))
               (nb/install-cell (:display-id block))
               (nb/install-cell (:next-id block)))
        n1 (install-block-slots n0 block)
        messages (cond-> []
                   (nil? (:head-id tui)) (conj (message (:view-id tui)
                                                        (:block-id block))
                                                   (message (:blocks-id tui)
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
    (when has-text?
      (prepare-block-targets! session client-id index text)
      (swap! session assign-source-order client-id index))
    (when rebuild?
      (rebuild-program! session))
    {:client-id client-id
     :index index
     :block-id (pr-str (:block-id block))
     :text-id (pr-str (:text-id block))
     :display-id (pr-str (:display-id block))}))

(defn- next-view-block-index
  [view]
  (or (some->> (:blocks view)
               (map :index)
               seq
               (apply max)
               inc)
      0))

(defn- input-view-block-index
  [view]
  (let [blocks (:blocks view)
        last-block (peek blocks)]
    (if (and last-block
             (= value/nothing (:value last-block))
             (not (:referenced? last-block)))
      (:index last-block)
      (next-view-block-index view))))

(defn- block-targets
  [source]
  (try
    (let [forms (read-source-forms source)]
      (into []
            (keep (fn [x]
                    (when (seq? x)
                      (case (first x)
                        block (let [[_ index] x]
                                (when (integer? index)
                                  {:index index :strict-past? true}))
                        block-at (let [[_ _ index] x]
                                   (when (integer? index)
                                     {:index index :strict-past? false}))
                        be:block-at (let [[_ _ index] x]
                                      (when (integer? index)
                                        {:index index :strict-past? false}))
                        nil))))
            (mapcat #(tree-seq coll? seq %) forms)))
    (catch Throwable _
      [])))

(defn- block-target-indexes
  [source]
  (set (map :index (block-targets source))))

(defn- empty-block? [state block]
  (let [v (block-text state block)]
    (or (value/nothing? v)
        (and (string? v) (str/blank? v)))))

(defn- ensure-block-index! [session client-id index]
  (loop []
    (let [tui (tui! session client-id)]
      (when (<= (:next-index tui) index)
        (append-tui-block! session {:client-id client-id
                                    :rebuild? false})
        (recur)))))

(defn- prepare-block-targets!
  [session client-id source-index source]
  (doseq [{:keys [index strict-past?]} (sort-by :index (block-targets source))]
    (let [state @session
          target-block (block-by-index state client-id index)]
      (cond
        (and target-block
             strict-past?
             (< index source-index)
             (not (empty-block? state target-block)))
        (throw (ex-info "block target points to a non-empty past block"
                        {:client-id client-id
                         :source-index source-index
                         :target-index index}))

        (nil? target-block)
        (ensure-block-index! session client-id index)))))

(defn edit-tui-block!
  [session {:keys [client-id index text]}]
  (let [tui (tui! session client-id)
        block0 (some #(when (= index (:index %)) %) (:blocks tui))]
    (when-not block0
      (throw (ex-info "block not found" {:client-id client-id :index index})))
    (prepare-block-targets! session client-id index text)
    (let [block block0
          new-epoch (inc (or (:epoch block) 0))
          _ (swap! session update-block client-id index
                   #(assoc % :epoch new-epoch))
          [tasks n1] (core/eval-cells [(message (:text-id block) text)]
                                      (:network @session))
          n2 (core/run-tasks tasks n1)]
      (swap! session assoc :network n2)
      (when-not (some? (:order block))
        (swap! session assign-source-order client-id index))
      (rebuild-program! session)
      {:client-id client-id
       :index index})))

(defn submit-tui-block!
  [session {:keys [client-id text]}]
  (let [view (read-tui-view @session {:client-id client-id})
        current-index (input-view-block-index view)
        has-current? (= current-index (some-> (:blocks view) peek :index))]
    (when-not has-current?
      (append-tui-block! session {:client-id client-id :rebuild? false}))
    (append-tui-block! session {:client-id client-id :rebuild? false})
    (edit-tui-block! session {:client-id client-id
                              :index current-index
                              :text text})
    (let [view (read-tui-view @session {:client-id client-id})]
      (when-not (and (seq (:blocks view))
                     (= value/nothing (-> view :blocks peek :value))
                     (not (-> view :blocks peek :referenced?)))
        (append-tui-block! session {:client-id client-id :rebuild? false})))
    (read-tui-view @session {:client-id client-id})))

(defn- block-value
  [state block]
  (net/network-cell-strongest (:network state) (:text-id block)))

(defn- block-display-value
  [state block]
  (net/network-cell-strongest (:network state) (:display-id block)))

(defn- block-view-value
  [state block]
  (let [display (when (nil? (:order block))
                  (block-display-value state block))]
    (if (and display (not (value/unusable? display)))
      display
      (block-value state block))))

(defn- behavior-projection?
  [v]
  (and (contains? (obj/public-slot-keys v) behavior/base-layer)
       (contains? (obj/public-slot-keys v) behavior/summary-layer)))

(defn- project-tui-value
  [v]
  (cond
    (value/unusable? v)
    v

    (semantic-trace/semantic-trace-graph? v)
    v

    (behavior-projection? v)
    (project-tui-value (behavior/base-value v))

    (behavior/behavior-value? v)
    (let [current (behavior/strongest-value v)]
      (if (value/unusable? current)
        current
        (project-tui-value current)))

    (tms/distributed-value? v)
    (let [projected (tms/strongest-distributed-value v)]
      (if (value/unusable? projected)
        projected
        (tms/distributed-base-value projected)))

    (net/network? v)
    (or (semantic-repl/display-cell-value v) "network")

    :else
    v))

(defn- referenced-block-indexes
  [state blocks]
  (reduce (fn [indexes block]
            (let [v (block-value state block)]
              (if (string? v)
                (into indexes (block-target-indexes v))
                indexes)))
          #{}
          blocks))

(defn project-tui-view
  [state {:keys [client-id]}]
  (let [tui (get-in (require-state state) [:tuis client-id])]
    (when-not tui
      (throw (ex-info "tui client not found" {:client-id client-id})))
    (let [referenced-indexes (referenced-block-indexes state (:blocks tui))]
      {:client-id client-id
       :view-id (pr-str (:view-id tui))
       :changed-cells (mapv pr-str (:runtime/changed-cells state))
       :changed-node-ids (mapv pr-str (:runtime/changed-node-ids state))
       :blocks (mapv (fn [block]
                       {:index (:index block)
                        :block-id (pr-str (:block-id block))
                        :text-id (pr-str (:text-id block))
                        :display-id (pr-str (:display-id block))
                        :referenced? (contains? referenced-indexes
                                                (:index block))
                        :value (project-tui-value (block-view-value state block))})
                     (:blocks tui))})))

(defn read-tui-view
  [state command]
  (project-tui-view state command))

(defn unregister-tui!
  [session {:keys [client-id]}]
  (let [remaining (atom nil)]
    (swap! session
           (fn [state]
             (let [count (get-in state [:tuis client-id :client-count] 0)
                   next-count (max 0 (dec count))]
               (reset! remaining next-count)
               (if (pos? next-count)
                 (assoc-in state [:tuis client-id :client-count] next-count)
                 (update state :tuis dissoc client-id)))))
    {:client-id client-id
     :remaining-clients @remaining
     :unregistered true}))

(defn project-xr-effects
  [state]
  {:effects (vec (get-in (require-state state) [:xr :effects] []))
   :launched (vals (get-in state [:xr :launched] {}))
   :widgets (get-in state [:xr :widgets] {})
   :changed-cells (mapv pr-str (:runtime/changed-cells state))
   :changed-node-ids (mapv pr-str (:runtime/changed-node-ids state))
   :tui-effects (vec (get-in state [:tui :effects] []))})

(defn read-xr-effects
  [state]
  (project-xr-effects state))

(defn handle-command!
  [session {:keys [op source] :as command}]
  (try
    (locking session
      {:ok true
       :result
       (case op
         :tui/register (register-tui! session command)
         :tui/append-block (append-tui-block! session command)
         :tui/edit-block (edit-tui-block! session command)
         :tui/submit-block (submit-tui-block! session command)
         :tui/read-view (read-tui-view @session command)
         :tui/unregister (unregister-tui! session command)
         :compile/source (compile-source! session source)
         :cells/list (list-cells @session)
         :cell/read (read-cell @session command)
         :semantic/graph (:graph (require-state @session))
         :semantic/trace (semantic-trace @session command)
         :semantic/expand (semantic-expansion @session command)
         :semantic/trace/install (install-semantic-trace! session command)
         :semantic/trace/read (read-installed-trace @session command)
         :semantic/trace/stop (stop-installed-trace! session command)
         :xr/effects (read-xr-effects @session)
         (throw (ex-info "unknown runtime op" {:op op})))})
    (catch Throwable t
      {:ok false
       :error (ex-message t)
       :data (ex-data t)})))
