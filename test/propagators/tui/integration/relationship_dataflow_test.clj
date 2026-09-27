(ns propagators.tui.integration.relationship-dataflow-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.tui.graph.xr-runtime :as xr]
            [propagators.compiler.model.env :as env]
            [propagators.tui.adapters.extensions.relationship-xr :as extension]
            [propagators.runtime.session.file-loader :as loader]
            [propagators.infra.helpers.task-queue :as tq]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.compiler.relationship-dataflow :as dataflow]
            [propagators.infra.runner :as runner]
            [propagators.infra.semantic-trace :as trace]))

(def options {:extensions [(extension/extension)]})

(defn graph-value [state name]
  (let [network (:program/net state)]
    (net/network-cell-strongest
     network (env/resolve-binding-id network (:program/env state) name))))

(defn labeled-edges [graph]
  (set (map #(mapv (:nodes graph) %) (:edges graph))))

(deftest chain-projects-exact-semantic-connectivity
  (let [state @(loader/load-session-from-file
                "examples/lain/relationship-chain.lain" options)
        raw (graph-value state 'relationships)
        graph (graph-value state 'semantic)]
    (is (= 8 (count (:nodes graph))))
    (is (< (count (:nodes graph)) (count (:nodes raw))))
    (is (= #{["1" 'source] ['source "1"] ['source '+] ["1" '+]
             ['+ 'middle] ['middle '*] ["2" '*] ['* 'result]}
           (labeled-edges graph)))
    (is (= 8 (count (:edges graph))))
    (is (= {:cell 6 :propagator 2} (frequencies (vals (:node-kinds graph)))))
    (is (= 2 (count (filter #{"1"} (vals (:nodes graph))))))
    (is (= 4 (get (:values graph)
                  (first (keep (fn [[id label]] (when (= 'result label) id))
                               (:nodes graph))))))
    (is (= 8 (count (:nodes (xr/graph->json graph)))))))

(deftest explicit-node-kinds-survive-tracing-and-override-label-guesses
  (let [graph (trace/graph-union
               {:nodes {:a '+ :b 'custom-transformation}
                :node-kinds {:a :cell :b :propagator}
                :edges [[:a :b]]})
        traced (trace/trace-graph graph {:node :b :direction :upstream})
        json (xr/graph->json traced)]
    (is (= (:node-kinds graph) (:node-kinds traced)))
    (is (= {"+" "cell" "custom-transformation" "propagator"}
           (into {} (map (juxt :label :kind)) (:nodes json))))))

(deftest fan-out-retains-expression-result
  (let [state @(loader/load-session-from-source
                "(def-cells a b c raw semantic)
                 (<-> 1 a)
                 (let-cell [v] (-> (+ a 1) v) (-> v b) (-> v c))
                 (relationship:roots a raw)
                 (relationship:dataflow raw semantic)" options)
        edges (labeled-edges (graph-value state 'semantic))]
    (is (contains? edges ['v 'b]))
    (is (contains? edges ['v 'c]))
    (is (contains? edges ['+ 'v]))))

(deftest projection-follows-cell-updates-and-environment-replacement
  (let [source "(def-cells a b raw semantic)
                (-> (+ a 1) b)
                (relationship:roots a raw)
                (relationship:dataflow raw semantic)"
        session (loader/load-session-from-source source options)
        state @session
        network (:program/net state)
        a (env/resolve-binding-id network (:program/env state) 'a)
        [updated tasks] (nb/seed-cell! network tq/empty-queue a 5)
        settled (runner/completed-network (runner/run-network tasks updated))
        graph (graph-value (assoc state :program/net settled) 'semantic)
        b (first (keep (fn [[id label]] (when (= 'b label) id)) (:nodes graph)))]
    (is (= 6 (get (:values graph) b)))
    (loader/replace-session-from-source!
     session (str source " (<-> 9 a)") options)
    (let [fresh (graph-value @session 'semantic)
          b (first (keep (fn [[id label]] (when (= 'b label) id)) (:nodes fresh)))]
      (is (= 10 (get (:values fresh) b))))))

(deftest invalid-and-empty-input
  (is (thrown? clojure.lang.ExceptionInfo
               (dataflow/dataflow-graph net/empty-net 42)))
  (is (empty? (:nodes (dataflow/dataflow-graph
                       net/empty-net {:semantic-trace/graph true :nodes {}})))))
