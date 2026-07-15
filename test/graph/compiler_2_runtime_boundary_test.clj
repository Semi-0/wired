(ns graph.compiler-2-runtime-boundary-test
  (:require [clojure.test :refer [deftest is]]
            [graph.compiler-2-runtime.commands :as runtime]
            [graph.compiler-2-runtime.display :as display]
            [graph.compiler-2-runtime.state :as state]
            [graph.compiler-2-runtime.trace-subscriptions :as subscriptions]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.cells.value :as value]
            [propagators.datastructures.event :as event]
            [propagators.network :as net]
            [propagators.semantic-trace :as semantic-trace]))

(defn- current-event-value
  [network id]
  (some-> (net/network-cell-content network id)
          event/active-values
          vals
          first))

(deftest display-normalizes-raw-values-without-changing-semantic-values
  (let [display-id :display
        first-update (display/update-value display-id display-id 1 1)
        second-update (display/update-value display-id display-id 2 2)
        content (event/merge-content first-update second-update)
        graph (semantic-trace/graph-union
               {:nodes {:a "a"} :edges [] :values {}})]
    (is (= 2 (-> content event/active-values vals first)))
    (is (= graph (display/update-value display-id display-id 3 graph)))))

(deftest language-trace-publishes-reactively-without-tui
  (let [session (state/new-session)
        source "(let-cell [next traced]
                  ((network [x] [out] (<-> (+ x 1) out)) 4 next)
                  (trace next traced)
                  traced)"
        _ (runtime/compile-source! session source)
        target-id (get-in @session [:compiled :cell])]
    (is (empty? (:tuis @session)))
    (is (= 1 (subscriptions/refresh-now! session)))
    (let [initial (current-event-value (:program/net @session) target-id)
          next-node (some (fn [[id label]] (when (= "next" label) id))
                          (:nodes initial))
          extra-node :headless/extra
          expanded (semantic-trace/graph-union
                    (:graph @session)
                    {:nodes {extra-node "extra"}
                     :edges [[extra-node next-node]]
                     :values {}})]
      (is (semantic-trace/semantic-trace-graph? initial))
      (is next-node)
      (swap! session assoc :graph expanded :program/epoch 1)
      (is (= 1 (subscriptions/refresh-now! session)))
      (let [updated (current-event-value (:program/net @session) target-id)]
        (is (contains? (set (semantic-repl/edge-labels updated))
                       ["extra" "next"]))
        (is (not (value/unusable? updated)))))))
