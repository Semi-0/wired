(ns graph.vijual.compiler-2-semantic-repl-test
  (:require [clojure.test :refer [deftest is testing]]
            [graph.compiler-2-semantic-repl :as repl]))

(defn- labels [graph]
  (set (vals (:nodes graph))))

(defn- label-edges [graph]
  (set (repl/edge-labels graph)))

(deftest semantic-repl-projects-compiler-2-expression
  (testing "semantic graph inlines resolved closure calls and hides compiler wiring"
    (let [graph (repl/semantic-graph
                 "(let-cell [inc-local]
                    (<-> inc-local (:: [x] (+ x 1)))
                    (inc-local 5))")]
      (is (contains? (label-edges graph) ["5" "x"]))
      (is (contains? (label-edges graph) ["x" "+"]))
      (is (contains? (label-edges graph) ["1" "+"]))
      (is (not (contains? (labels graph) "call inc-local")))
      (is (not (contains? (labels graph) "prop:<->"))))))

(deftest semantic-repl-consolidates-explicit-network-output-cells
  (testing "network output applicants appear as ordinary semantic cell edges"
    (let [graph (repl/semantic-graph
                 "(let-cell [same next]
                    ((network [x] [same next]
                       (<-> x same)
                       (<-> (+ x 1) next))
                     4 same next)
                    next)")
          edges (label-edges graph)]
      (is (contains? edges ["4" "x"]))
      (is (contains? edges ["x" "<->"]))
      (is (contains? edges ["same" "<->"]))
      (is (contains? edges ["<->" "x"]))
      (is (contains? edges ["<->" "same"]))
      (is (contains? edges ["x" "+"]))
      (is (contains? edges ["1" "+"]))
      (is (contains? edges ["+" "<->"]))
      (is (contains? edges ["next" "<->"]))
      (is (= "4" (get (repl/value-labels graph) "x")))
      (is (= "4" (get (repl/value-labels graph) "same")))
      (is (= "5" (get (repl/value-labels graph) "next")))
      (is (not (contains? (labels graph) "slot same")))
      (is (not (contains? (labels graph) "slot next")))
      (is (not (contains? (labels graph) "slot closure/env")))
      (is (not (contains? (labels graph) "call ::"))))))
