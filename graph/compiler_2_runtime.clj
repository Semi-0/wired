(ns graph.compiler-2-runtime
  "Shared compiler-2 runtime session for socket clients."
  (:require [graph.compiler-2-semantic-repl :as semantic-repl]
            [graph.vijual-compiler-2-demo :as demo]
            [propagators.cells.cell :as cell]
            [propagators.cells.value :as value]
            [propagators.compiler-2.env :as cenv]
            [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.compiler-2.main :as compiler]
            [propagators.compiler-2.parser :as compiler-parser]
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]
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

(defn- runtime-graph-id []
  (stable-node-id :runtime :semantic-graph))

(defn- xr-outbox-id []
  (stable-node-id :runtime :xr :outbox))

(defn- receipt-slot-key
  [effect-id]
  (str "boundary/receipt:" (hash effect-id)))

(defn- effect-slot-key
  [effect-id]
  (str "boundary/effect:" (hash effect-id)))

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
     :xr {:launched {}}
     :graph graph}))

(defn- empty-state []
  {:network net/empty-net
   :program/net net/empty-net
   :program/env (compiler-helpers/default-env)
   :program/graph {:nodes {} :edges [] :values {} :expansions {}}
   :program/results {}
   :program/epoch 0
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

(declare append-tui-block! edit-tui-block! install-block-slots read-tui-view semantic-trace)

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
                      #(assoc % :order order))
        (update :block-order (fnil conj []) {:client-id client-id
                                             :index index})
        (assoc :next-order (inc order)))))

(defn- source-blocks
  [state]
  (->> (:block-order state)
       (keep (fn [{:keys [client-id index]}]
               (when-let [block (block-by-index state client-id index)]
                 (when (some? (:order block))
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
  (and (string? client-id)
       (boolean (re-matches #"[A-Za-z_][A-Za-z0-9_-]*" client-id))))

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

(defn- block-at-text-id
  [network instance-id index-id]
  (let [wanted-index (net/network-cell-strongest network index-id)
        first-block-id (instance-block-head-id network instance-id)]
    (loop [block-id first-block-id
           seen #{}]
      (when (and (ids/node-id? block-id)
                 (not (contains? seen block-id)))
        (let [index-cell-id (declared-slot-parent-id network
                                                     block-id
                                                     :block/index)
              text-cell-id (declared-slot-parent-id network
                                                    block-id
                                                    :block/text)
              next-cell-id (declared-slot-parent-id network block-id :cdr)
              block-index (when index-cell-id
                            (net/network-cell-strongest network index-cell-id))]
          (if (= wanted-index block-index)
            text-cell-id
            (recur (when next-cell-id
                     (net/network-cell-strongest network next-cell-id))
                   (conj seen block-id))))))))

(defn- block-at-operator []
  (with-meta
    (fn [network arg-ids out-id]
      (let [[instance-id index-id maybe-out] (vec arg-ids)
            target-id (or maybe-out out-id)
            text-id (block-at-text-id network instance-id index-id)]
        (when-not (and (= 3 (count arg-ids)) text-id)
          (throw (ex-info "block-at expects a known instance and index"
                          {:arg-ids arg-ids
                           :index (net/network-cell-strongest network
                                                             index-id)})))
        (let [[read-prop n1] ((stdlib-prop/id text-id target-id) network)
              [write-prop n2] ((stdlib-prop/id target-id text-id) n1)]
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
             target-value (net/network-cell-strongest current-net target-id)]
         (when-not (= 3 (count arg-ids))
           (throw (ex-info "block-at expects instance, index, and output"
                           {:arg-ids arg-ids})))
         (cond-> []
           text-id
           (conj (message target-id source-value))

           text-id
           (conj (message text-id target-value)))))}))

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
  {:boundary/effect true
   :boundary/id effect-id
   :boundary/port :xr
   :boundary/kind :xr/launch-trace
   :boundary/payload {:graph trace-graph}
   :boundary/receipt-id receipt-id
   :boundary/epoch epoch})

(defn- xr-receipt
  [request status]
  {:boundary/receipt true
   :boundary/id (:boundary/id request)
   :boundary/port (:boundary/port request)
   :boundary/kind (:boundary/kind request)
   :boundary/status status
   :boundary/epoch (:boundary/epoch request)})

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
  (mapcat (fn [[client-id tui]]
            (map #(assoc % :client-id client-id) (:blocks tui)))
          (:tuis state)))

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
                       (:next-id block) (:text-id block)])]
       (install-block-slots n0 block)))
   program-net
   (all-blocks state)))

(defn- runtime-env [state base-env graph-id current-client-id]
  (as-> base-env env
    (cenv/bind-at env 'block-at (block-at-operator) 0)
    (cenv/bind-at env 'instance (instance-operator) 0)
    (cenv/bind-at env 'trace (trace-operator graph-id) 0)
    (cenv/bind-at env 'xr-io (xr-io-operator (xr-outbox-id)) 0)
    (cenv/bind-at env 'translate (translate-operator) 0)
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

(defn- sync-program-block-writes [state program-net epoch]
  (reduce (fn [s block]
            (let [v (net/network-cell-strongest program-net (:text-id block))]
              (if (= value/nothing v)
                s
                (write-block-value s block epoch v))))
          state
          (all-blocks state)))

(defn- top-level-declaration? [source]
  (try
    (let [form (compiler-parser/read-form source)]
      (and (seq? form)
           (contains? '#{def def-cell def-net <-> block-at translate}
                      (first form))))
    (catch Throwable _
      false)))

(defn- trace-source? [source]
  (try
    (let [form (compiler-parser/read-form source)]
      (and (seq? form) (= 'trace (first form))))
    (catch Throwable _
      false)))

(defn- normalize-trace-source
  [source]
  (try
    (let [form (compiler-parser/read-form source)]
      (if (and (seq? form)
               (= 'trace (first form))
               (symbol? (second form)))
        (pr-str (cons 'trace (cons (name (second form)) (nnext form))))
        source))
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
          graph* (namespace-graph
                  (semantic-repl/compiled-semantic-graph compiled program-net0)
                  (str (:client-id block) "-" (:order block)))
          graph (assoc (semantic-trace/graph-union
                        (:graph state)
                        graph*)
                       :source source)
          [tasks program-net1] (core/eval-cells [(message graph-id graph)]
                                                (nb/ensure-cell program-net0
                                                                graph-id))
          program-net (nb/run-propagators (core/run-tasks tasks program-net1)
                                          (:props compiled))
          graph* (namespace-graph
                  (semantic-repl/compiled-semantic-graph compiled program-net)
                  (str (:client-id block) "-" (:order block)))
          graph (assoc (semantic-trace/graph-union
                        (:graph state)
                        graph*)
                       :source source)
          result (compiled-result-value compiled program-net)
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
            (not (string? source)))
      state
      (compile-program-form state block epoch source))))

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
                                     (seed-program-instances state net/empty-net))
                                    (xr-outbox-id))
                                   :program/epoch
                                   epoch)
                                  (runtime-graph-id)
                                  (semantic-trace/graph-union (empty-graph))
                                  (semantic-trace/graph-union (empty-graph)))
                    :program/env (compiler-helpers/default-env)
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
      (reduce (fn [s block]
                (let [source (block-text s block)]
                  (if (and (string? source) (trace-source? source))
                    (compile-program-form s block epoch source)
                    s)))
              s
              source-blocks))))

(defn- outbox-effects
  [program-net]
  (if-not (contains? (net/net-env program-net) (xr-outbox-id))
    []
    (let [outbox (net/network-cell-strongest program-net (xr-outbox-id))]
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

(defn- graph-size
  [request]
  (let [graph (get-in request [:boundary/payload :graph])]
    (+ (count (:nodes graph))
       (count (:edges graph)))))

(defn- delivery-key
  [request]
  [(:boundary/port request)
   (:boundary/kind request)
   (:boundary/receipt-id request)
   (:boundary/epoch request)])

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
  (reduce record-xr-launch
          state
          (collapse-boundary-effects (outbox-effects (:program/net state)))))

(defn- rebuild-program!
  [session]
  (swap! session #(perform-boundary-effects (rebuild-program-state %)))
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
            instance-id (stable-node-id :tui client-id :instance)
            blocks-id (stable-node-id :tui client-id :blocks)
            n (-> (:network state)
                  (nb/ensure-cell view-id)
                  (nb/install-cell instance-id instance-id instance-id)
                  (nb/ensure-cell blocks-id)
                  (install-instance-slots {:instance-id instance-id
                                           :blocks-id blocks-id}))
            tui {:client-id client-id
                 :view-id view-id
                 :instance-id instance-id
                 :blocks-id blocks-id
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

(defn append-tui-block!
  [session {:keys [client-id text rebuild?] :or {rebuild? true} :as command}]
  (let [tui (tui! session client-id)
        index (:next-index tui)
        has-text? (contains? command :text)
        block {:block-id (ids/new-node-id)
               :index-id (ids/new-node-id)
               :text-id (ids/new-node-id)
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
      (swap! session assign-source-order client-id index))
    (when rebuild?
      (rebuild-program! session))
    {:client-id client-id
     :index index
     :block-id (pr-str (:block-id block))
     :text-id (pr-str (:text-id block))}))

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

(defn edit-tui-block!
  [session {:keys [client-id index text]}]
  (let [tui (tui! session client-id)
        block0 (some #(when (= index (:index %)) %) (:blocks tui))]
    (when-not block0
      (throw (ex-info "block not found" {:client-id client-id :index index})))
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

(defn- block-at-target-indexes
  [source]
  (try
    (let [form (compiler-parser/read-form source)]
      (into #{}
            (keep (fn [x]
                    (when (and (seq? x)
                               (= 'block-at (first x)))
                      (let [[_ _ index] x]
                        (when (integer? index)
                          index)))))
            (tree-seq coll? seq form)))
    (catch Throwable _
      #{})))

(defn- referenced-block-indexes
  [state blocks]
  (reduce (fn [indexes block]
            (let [v (block-value state block)]
              (if (string? v)
                (into indexes (block-at-target-indexes v))
                indexes)))
          #{}
          blocks))

(defn read-tui-view
  [state {:keys [client-id]}]
  (let [tui (get-in (require-state state) [:tuis client-id])]
    (when-not tui
      (throw (ex-info "tui client not found" {:client-id client-id})))
    (let [referenced-indexes (referenced-block-indexes state (:blocks tui))]
      {:client-id client-id
       :view-id (pr-str (:view-id tui))
       :blocks (mapv (fn [block]
                       {:index (:index block)
                        :block-id (pr-str (:block-id block))
                        :text-id (pr-str (:text-id block))
                        :referenced? (contains? referenced-indexes
                                                (:index block))
                        :value (block-value state block)})
                     (:blocks tui))})))

(defn unregister-tui!
  [session {:keys [client-id]}]
  (swap! session update :tuis dissoc client-id)
  {:client-id client-id
   :unregistered true})

(defn read-xr-effects
  [state]
  {:effects (vec (get-in (require-state state) [:xr :effects] []))
   :launched (vals (get-in state [:xr :launched] {}))})

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
