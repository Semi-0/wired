(ns graph.compiler-2-runtime.operators.trace
  "Trace compiler-2 runtime operators."
  (:require [propagators.cells.value :as value]
            [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]
            [propagators.semantic-trace :as semantic-trace]))

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

(defn trace-operator [graph-id]
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
