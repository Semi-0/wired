(ns graph.compiler-2-runtime.program-rebuild
  "Transactional rebuild path for compiler-2 runtime source blocks."
  (:require [graph.compiler-2-runtime.program :as program]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.cells.value :as value]
            [propagators.compiler-2.main :as compiler]
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.semantic-trace :as semantic-trace]))

(defn- result-key
  [block]
  [(:client-id block) (:index block)])

(defn- topology-props
  [program-net props]
  (let [applications (set (program/retained-application-props program-net))]
    (filterv (complement applications) props)))

(defn- compile-topology-form
  [state block epoch source]
  (try
    (let [source (program/normalize-trace-source source)
          source (program/auto-output-source state block source)
          graph-id (program/runtime-graph-id)
          env (program/runtime-env state
                                   (:program/env state)
                                   graph-id
                                   (:client-id block))
          program-net-input (nb/install-cell (:program/net state)
                                             graph-id
                                             (:graph state)
                                             (:graph state))
          compiled (compiler/compile-source
                    source
                    env
                    {:net program-net-input
                     :seed [:runtime/block (:order block) (:epoch block)]
                     :reuse-existing-bindings? true})
          program-net (nb/run-propagators
                       (:net compiled)
                       (topology-props (:net compiled) (:props compiled)))]
      (-> state
          (assoc :program/net program-net
                 :program/env (:env compiled)
                 :compiled compiled
                 :compiled-network program-net
                 :source source)
          (assoc-in [:program/results (result-key block)]
                    {:compiled compiled
                     :source source
                     :block block
                     :top-level-trace? (program/trace-source? source)})))
    (catch Throwable t
      (assoc-in state
                [:program/results (result-key block)]
                {:error (ex-message t)
                 :data (ex-data t)}))))

(defn- rebuild-topology-block
  [state epoch block]
  (let [source (program/block-text state block)]
    (if (or (value/unusable? source)
            (not (string? source))
            (program/trace-source? source))
      state
      (compile-topology-form state block epoch source))))

(defn- block-error?
  [state block]
  (some? (get-in state [:program/results (result-key block) :error])))

(defn- retry-errored-blocks
  [state epoch source-blocks]
  (reduce (fn [s block]
            (if (block-error? s block)
              (rebuild-topology-block s epoch block)
              s))
          state
          source-blocks))

(defn- retry-expression-blocks
  [state epoch source-blocks]
  (reduce (fn [s block]
            (let [source (program/block-text s block)]
              (if (and (string? source)
                       (not (program/top-level-declaration? source)))
                (rebuild-topology-block s epoch block)
                s)))
          state
          source-blocks))

(defn- compile-trace-source-blocks
  [state epoch source-blocks]
  (reduce (fn [s block]
            (let [source (program/block-text s block)]
              (if (and (string? source) (program/trace-source? source))
                (compile-topology-form s block epoch source)
                s)))
          state
          source-blocks))

(defn- seed-instance
  [runtime-net [program-net props] {:keys [instance-id blocks-id]}]
  (let [program-net (-> program-net
                        (program/seed-program-block-cell runtime-net instance-id)
                        (program/seed-program-block-cell runtime-net blocks-id))
        [prop-id program-net] ((obj/p:slot :instance/blocks
                                           blocks-id
                                           instance-id)
                               program-net)]
    [program-net (conj props prop-id)]))

(defn- seed-block
  [runtime-net [program-net props] block]
  (let [program-net (reduce #(program/seed-program-block-cell %1
                                                              runtime-net
                                                              %2)
                            program-net
                            [(:block-id block) (:index-id block)
                             (:next-id block) (:text-id block)
                             (:display-id block)])
        [index-prop program-net] ((obj/p:slot :block/index
                                              (:index-id block)
                                              (:block-id block))
                                  program-net)
        program-net (if (:text-current-source? block)
                      (nb/install-cell program-net
                                       (:text-id block)
                                       (:text-current block)
                                       (:text-current block))
                      program-net)
        [text-prop program-net] ((obj/p:slot :block/text
                                             (:text-id block)
                                             (:block-id block))
                                 program-net)
        [display-prop program-net] ((obj/p:slot :block/display
                                                (:display-id block)
                                                (:block-id block))
                                    program-net)
        [next-prop program-net] ((obj/p:cdr (:next-id block)
                                            (:block-id block))
                                 program-net)]
    [program-net (into props [index-prop text-prop display-prop next-prop])]))

(defn- seed-program-topology
  [state program-net]
  (let [runtime-net (:network state)
        [program-net props] (reduce (partial seed-instance runtime-net)
                                    [program-net []]
                                    (vals (:tuis state)))
        [program-net props] (reduce (partial seed-block runtime-net)
                                    [program-net props]
                                    (program/all-blocks state))]
    (nb/run-propagators program-net props)))

(defn- base-rebuild-state
  [state epoch]
  (assoc state
         :program/net (nb/install-cell
                       (net/assoc-net-dict-entry
                        (nb/ensure-cell
                         (seed-program-topology state
                                                (program/runtime-base-net))
                         (program/boundary-outbox-id))
                        :program/epoch
                        epoch)
                       (program/runtime-graph-id)
                       (semantic-trace/graph-union (program/empty-graph))
                       (semantic-trace/graph-union (program/empty-graph)))
         :program/env (program/runtime-compiler-env)
         :program/graph (program/empty-graph)
         :program/results {}
         :program/epoch epoch
         :compiled nil
         :compiled-network net/empty-net
         :graph (program/empty-graph)
         :source nil))

(defn- compiled-entry
  [state block]
  (get-in state [:program/results (result-key block)]))

(defn- entry-graph
  [entry program-net]
  (when (and (:compiled entry)
             (not (:top-level-trace? entry)))
    (program/namespace-graph
     (semantic-repl/compiled-application-semantic-graph (:compiled entry)
                                                        program-net)
     (str (-> entry :block :client-id) "-"
          (-> entry :block :order)))))

(defn- project-program-graph
  [state source-blocks]
  (let [program-net (:program/net state)]
    (reduce (fn [graph block]
              (let [entry (compiled-entry state block)]
                (cond
                  (:error entry)
                  graph

                  (:top-level-trace? entry)
                  (assoc graph :source (:source entry))

                  :else
                  (if-let [graph* (entry-graph entry program-net)]
                    (assoc (semantic-trace/graph-union graph graph*)
                           :source (:source entry))
                    graph))))
            (program/empty-graph)
            source-blocks)))

(defn- publish-graph
  [state graph]
  (let [graph-id (program/runtime-graph-id)
        [tasks n1] (core/eval-cells [(message graph-id graph)]
                                    (nb/ensure-cell (:program/net state)
                                                    graph-id))
        n2 (core/run-tasks tasks n1)]
    (assoc state
           :program/net n2
           :program/graph graph
           :graph graph)))

(defn- settle-state
  [state]
  (assoc state :program/net (program/settle-application-props
                             (:program/net state)
                             [])))

(defn- project-results
  [state source-blocks]
  (reduce (fn [s block]
            (let [entry (compiled-entry s block)]
              (if-let [compiled (:compiled entry)]
                (assoc-in s
                          [:program/results (result-key block) :result]
                          (program/compiled-result-value compiled
                                                         (:program/net s)))
                s)))
          state
          source-blocks))

(defn transactional-rebuild-program-state
  [state]
  (let [epoch (inc (or (:program/epoch state) 0))
        source-blocks (program/source-blocks state)
        compiled (as-> (reduce (fn [s block]
                                  (rebuild-topology-block s epoch block))
                                (base-rebuild-state state epoch)
                                source-blocks) s
                   (retry-errored-blocks s epoch source-blocks)
                   (retry-expression-blocks s epoch source-blocks)
                   (compile-trace-source-blocks s epoch source-blocks))
        graph (project-program-graph compiled source-blocks)]
    (as-> compiled s
      (publish-graph s graph)
      (settle-state s)
      (publish-graph s (project-program-graph s source-blocks))
      (settle-state s)
      (project-results s source-blocks)
      (assoc s :compiled-network (:program/net s)))))

(defn incremental-block-state
  [state block]
  (let [source (program/block-text state block)]
    (when (and (string? source)
               (not (value/unusable? source))
               (some? (:order block))
               (not (program/trace-source? source)))
      (let [epoch (inc (or (:program/epoch state) 0))
            state (assoc state
                         :program/net
                         (net/assoc-net-dict-entry
                          (nb/ensure-cell
                           (seed-program-topology state (:program/net state))
                           (program/boundary-outbox-id))
                          :program/epoch
                          epoch))
            compiled (program/compile-program-form
                      (assoc state :program/epoch epoch)
                      (assoc block :epoch (:epoch block))
                      epoch
                      source)
            entry (compiled-entry compiled block)]
        (when-not (:error entry)
          (let [settled (-> compiled
                            (publish-graph (:graph compiled))
                            settle-state)]
            (assoc settled
                   :compiled-network (:program/net settled)
                   :runtime/incremental-installs
                   (inc (long (or (:runtime/incremental-installs settled)
                                  0))))))))))

(defn- transaction-supported?
  [state]
  (let [source-update-symbols '#{behavior-event premise-input premise-retract}]
    (letfn [(source-uses-symbols? [source syms]
              (try
                (boolean
                 (some syms
                       (mapcat #(tree-seq coll? seq %)
                               (program/read-source-forms source))))
                (catch Throwable _
                  false)))
            (unsupported-block? [block]
              (let [source (program/block-text state block)]
                (or (program/trace-source? source)
                    (pos? (long (or (:epoch block) 0)))
                    (source-uses-symbols? source source-update-symbols))))]
      (not-any? unsupported-block? (program/source-blocks state)))))

(defn rebuild-program-state
  [state]
  (try
    (if (transaction-supported? state)
      (transactional-rebuild-program-state state)
      (program/rebuild-program-state state))
    (catch Throwable _
      (program/rebuild-program-state state))))
