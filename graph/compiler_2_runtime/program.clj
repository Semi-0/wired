(ns graph.compiler-2-runtime.program
  "Program compilation and rebuild logic for compiler-2 runtime."
  (:require [graph.compiler-2-runtime.block-model :as block-model]
            [graph.compiler-2-runtime.boundary :as boundary]
            [graph.compiler-2-runtime.effects :as effects]
            [graph.compiler-2-runtime.operators :as runtime-ops]
            [graph.compiler-2-runtime.program-source :as source]
            [graph.compiler-2-runtime.state :as state]
            [graph.compiler-2-runtime.widget :as runtime-widget]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.cells.value :as value]
            [propagators.compiler-2.runtime.application :as compiler-app]
            [propagators.compiler-2.model.env :as cenv]
            [propagators.compiler-2.main :as compiler]
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.scope-source :as scope-source]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.semantic-trace :as semantic-trace]))

(def default-xr-client-id state/default-xr-client-id)
(def empty-state state/empty-state)
(def empty-graph state/empty-graph)
(def runtime-base-net state/runtime-base-net)
(def runtime-compiler-env state/runtime-compiler-env)
(def runtime-graph-id state/runtime-graph-id)
(def boundary-outbox-id state/boundary-outbox-id)
(def perform-boundary-effects effects/perform-boundary-effects)
(def block-text block-model/block-text)
(def source-blocks block-model/source-blocks)
(def all-blocks block-model/all-blocks)
(def block-by-index block-model/block-by-index)
(def read-source-forms source/read-source-forms)
(def top-level-declaration? source/top-level-declaration?)
(def trace-source? source/trace-source?)
(def trace-form? source/trace-form?)
(def normalize-trace-source source/normalize-trace-source)
(def auto-output-source source/auto-output-source)

(declare runtime-compiler)

(defn runtime-application-installer
  [application-id operator-id args-id arg-ids context-id out-id]
  (compiler-app/p:apply-application-with runtime-compiler
                                         application-id
                                         operator-id
                                         args-id
                                         arg-ids
                                         context-id
                                         out-id))

(defn runtime-compiler
  [compiler-state expr]
  (compiler/default-compiler
   (assoc compiler-state :application-installer runtime-application-installer)
   expr))

(defn runtime-compile-options
  [opts]
  (assoc opts
         :compiler runtime-compiler
         :application-installer runtime-application-installer))

(declare runtime-env
         settle-application-props)

(defn- expose-application-boundary-outputs
  [program-net]
  (net/update-net-dict-entry
   program-net
   compiler-app/application-extra-output-ids-key
   (fnil conj #{})
   (boundary-outbox-id)))

(defn compiled-state
  [source]
  (let [graph-id (runtime-graph-id)
        base-state (empty-state)
        base-net (-> (:program/net base-state)
                     expose-application-boundary-outputs
                     (nb/ensure-cell (boundary-outbox-id))
                     (nb/install-cell graph-id
                                      (semantic-trace/graph-union (empty-graph))
                                      (semantic-trace/graph-union (empty-graph))))
        environment (runtime-env base-state
                                 base-net
                                 (:program/env base-state)
                                 graph-id
                                 default-xr-client-id
                                 [:compiled-source source])
        compiled (compiler/compile-source source
                                          (:env environment)
                                          (runtime-compile-options
                                           {:net (:net environment)
                                            :seed [:runtime/xr source]}))
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

(defn compile-source!
  [session source]
  (doseq [{:keys [stop]} (vals (:traces @session))]
    (when stop (stop)))
  (let [state (perform-boundary-effects
               (assoc (compiled-state source) :traces {}))]
    (reset! session state)
    (:graph state)))

(defn compiled-result-value
  [compiled network]
  (let [result-id (:cell compiled)
        result (scope-source/unwrap
                (net/network-cell-strongest network result-id))]
    (cond
      (value/unusable? result) value/nothing
      (net/network? result) value/nothing
      :else result)))

(defn namespace-graph
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

(defn seed-program-block-cell [program-net runtime-net id]
  (let [v (net/network-cell-strongest runtime-net id)]
    (if (or (value/unusable? v)
            (semantic-trace/semantic-trace-graph? v))
      (nb/ensure-cell program-net id)
      (nb/install-cell program-net id v v))))

(defn install-instance-slots
  [n {:keys [instance-id blocks-id]}]
  (let [[_cell-id prop-ids n]
        (obj/install-slot-access n :instance/blocks instance-id blocks-id)]
    (nb/run-propagators n prop-ids)))

(defn install-block-slots
  [n {:keys [block-id index-id text-id display-id next-id]}]
  (let [[_index-cell index-props n]
        (obj/install-slot-access n :block/index block-id index-id)
        [_text-cell text-props n]
        (obj/install-slot-access n :block/text block-id text-id)
        [_display-cell display-props n]
        (obj/install-slot-access n :block/display block-id display-id)
        [_next-cell next-props n]
        (obj/install-slot-access n :cdr block-id next-id)]
    (nb/run-propagators n
                        (into [] cat [index-props
                                     text-props
                                     display-props
                                     next-props]))))

(defn seed-program-instances [state program-net]
  (reduce-kv
   (fn [n _client-id {:keys [instance-id blocks-id]}]
     (-> n
         (seed-program-block-cell (:network state) instance-id)
         (seed-program-block-cell (:network state) blocks-id)
         (install-instance-slots {:instance-id instance-id
                                  :blocks-id blocks-id})))
   program-net
   (:tuis state)))

(defn seed-program-blocks [state program-net]
  (reduce
   (fn [n block]
     (let [n0 (reduce #(seed-program-block-cell %1 (:network state) %2)
                      n
                      [(:block-id block) (:index-id block)
                       (:next-id block) (:text-id block)
                       (:display-id block)])]
       (install-block-slots
        (if (:text-current-source? block)
          (nb/install-cell n0
                           (:text-id block)
                           (:text-current block)
                           (:text-current block))
          n0)
        block)))
   program-net
   (all-blocks state)))

(defn- static-runtime-bindings
  [graph-id]
  [['block-at (runtime-ops/block-at-operator (boundary-outbox-id))]
   ['be:block-at (runtime-ops/be-block-at-operator (boundary-outbox-id))]
   ['instance (runtime-ops/instance-operator)]
   ['trace-target (runtime-ops/trace-target-operator)]
   ['trace (runtime-ops/trace-operator graph-id (boundary-outbox-id))]
   ['xr-io (runtime-ops/xr-io-operator (boundary-outbox-id))]
   ['io:xr (runtime-ops/io-xr-operator (boundary-outbox-id))]
   ['slider-io (runtime-widget/slider-io-operator (boundary-outbox-id))]
   ['slider-panel-io (runtime-widget/slider-panel-io-operator (boundary-outbox-id))]
   ['io:slider (runtime-widget/io-slider-operator (boundary-outbox-id))]
   ['io:slider-panel (runtime-widget/io-slider-panel-operator (boundary-outbox-id))]
   ['io:slider-panels (runtime-widget/io-slider-panel-operator (boundary-outbox-id))]
   ['io:slider-panel-name
    (runtime-widget/io-slider-panel-name-operator (boundary-outbox-id))]
   ['runtime:clients (runtime-ops/runtime-clients-operator)]
   ['runtime:client-pipe (runtime-ops/runtime-client-pipe-operator)]
   ['runtime:list-text-events (runtime-ops/list-text-events-operator)]
   ['translate (runtime-ops/translate-operator)]])

(defn- dynamic-runtime-bindings
  [runtime-state current-client-id]
  (let [instance-id (get-in runtime-state [:tuis current-client-id :instance-id])]
    (cond-> (mapv (fn [[client-id {:keys [instance-id]}]]
                    [(symbol client-id) (cenv/cell-binding instance-id)])
                  (:tuis runtime-state))
      instance-id
      (into [['block (runtime-ops/block-target-operator (boundary-outbox-id)
                                                        instance-id)]
             ['be:block (runtime-ops/be-block-target-operator
                         (boundary-outbox-id) instance-id)]
             ['% (cenv/cell-binding instance-id)]]))))

(defn runtime-env
  [runtime-state network base-env graph-id current-client-id scope-key]
  (let [dynamic (dynamic-runtime-bindings runtime-state current-client-id)]
    (if-not (ids/node-id? base-env)
      (let [root-id (state/stable-node-id :compiler-2 :runtime-env :root)
            initial (reduce (fn [environment [sym binding]]
                              (cenv/bind-at environment sym binding 0))
                            base-env
                            (into dynamic (static-runtime-bindings graph-id)))
            [network env-id] (cenv/import-environment network root-id initial)]
        {:net network :env env-id :props []})
      (let [child-id (state/stable-node-id :compiler-2 :runtime-env scope-key)
            [scope-props network] ((cenv/p:sub-env base-env child-id)
                                   (nb/ensure-cell network base-env))
            declared (cenv/declare-bindings network child-id child-id dynamic)
            props (into (vec scope-props) (:props declared))]
        {:net (nb/run-propagators (:net declared) props)
         :env child-id
         :props props}))))

(defn retained-application-props
  [program-net]
  (vec (get (net/net-dict-or-empty program-net)
            compiler-app/apply-application-props-key
            #{})))

(defn settle-application-props
  [program-net current-props]
  (let [props (vec (distinct (concat (retained-application-props program-net)
                                     current-props)))]
    (-> program-net
        (nb/run-propagators props)
        (nb/run-propagators props))))

(defn compile-program-form
  [state block epoch source]
  (try
    (let [source (normalize-trace-source source)
          source (auto-output-source state block source)
          needs-live-graph? (trace-form? source)
          top-level-trace? (trace-source? source)
          graph-id (runtime-graph-id)
          program-net-input (-> (:program/net state)
                                expose-application-boundary-outputs
                                (nb/install-cell graph-id
                                                 (:graph state)
                                                 (:graph state)))
          environment (runtime-env state
                                   program-net-input
                                   (:program/env state)
                                   graph-id
                                   (:client-id block)
                                   [:block (:order block) (:epoch block)])
          compiled (compiler/compile-source
                    source
                    (:env environment)
                    (runtime-compile-options
                     {:net (:net environment)
                      :seed [:runtime/block (:order block) (:epoch block)]
                      :reuse-existing-bindings? true}))
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
                {:error (or (ex-message t) (str (class t)))
                 :class (str (class t))
                 :data (ex-data t)}))))

(defn rebuild-block
  [state epoch block]
  (let [source (block-text state block)]
    (if (or (value/unusable? source)
            (not (string? source))
            (trace-source? source))
      state
      (compile-program-form state block epoch source))))

(defn block-compile-error?
  [state block]
  (some? (get-in state
                 [:program/results [(:client-id block) (:index block)] :error])))

(defn retry-errored-blocks
  [state epoch source-blocks]
  (reduce (fn [s block]
            (if (block-compile-error? s block)
              (rebuild-block s epoch block)
              s))
          state
          source-blocks))

(defn retry-expression-blocks
  [state epoch source-blocks]
  (reduce (fn [s block]
            (let [source (block-text s block)]
              (if (and (string? source)
                       (not (top-level-declaration? source)))
                (rebuild-block s epoch block)
                s)))
          state
          source-blocks))

(defn rebuild-program-state
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
