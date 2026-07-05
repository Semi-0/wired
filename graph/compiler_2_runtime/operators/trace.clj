(ns graph.compiler-2-runtime.operators.trace
  "Trace compiler-2 runtime operators."
  (:require [propagators.cells.value :as value]
            [propagators.compiler-2.operator-value :as operator-value]
            [propagators.message :refer [message]]
            [propagators.network :as net]
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

(defn- trace-messages
  [network graph-id source-id direction-id target-id]
  (let [direction (if direction-id
                    (net/network-cell-strongest network direction-id)
                    :upstream)
        graph (net/network-cell-strongest network graph-id)
        source-value (net/network-cell-strongest network source-id)
        request (trace-request-source graph source-id source-value direction)]
    (if (or (value/unusable? graph)
            (value/unusable? direction))
      []
      [(message target-id
                (semantic-trace/trace-graph graph request))])))

(defn trace-operator [graph-id]
  (operator-value/operator-closure
   {:name 'trace
    :output-selector (fn [arg-ids fallback-id]
                       (or (last (vec arg-ids)) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[source-id direction-or-out maybe-out] (vec arg-ids)
                      target-id (or maybe-out direction-or-out out-id)
                      direction-id (when maybe-out direction-or-out)]
                  (when-not (#{2 3} (count arg-ids))
                    (throw (ex-info "trace expects source, optional direction, and output"
                                    {:arg-ids arg-ids})))
                  (trace-messages network
                                  graph-id
                                  source-id
                                  direction-id
                                  target-id)))}))
