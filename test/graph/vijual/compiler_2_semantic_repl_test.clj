(ns graph.vijual.compiler-2-semantic-repl-test
  (:require [clojure.test :refer [deftest is testing]]
            [graph.compiler-2-semantic-repl :as repl]))

(defn- labels [graph]
  (set (vals (:nodes graph))))

(defn- label-edges [graph]
  (set (repl/edge-labels graph)))

(defn- label-frequencies [graph]
  (frequencies (vals (:nodes graph))))

(deftest semantic-repl-projects-compiler-2-expression
  (testing "semantic graph collapses resolved closure calls and stores expansion"
    (let [graph (repl/semantic-graph
                 "(let-cell [inc-local]
                    (<-> inc-local (:: [x] (+ x 1)))
                    (inc-local 5))")
          expansion (repl/expansion graph {:label "call inc-local"})]
      (is (contains? (label-edges graph) ["5" "call inc-local"]))
      (is (contains? (label-edges graph) ["call inc-local" "result"]))
      (is (not (contains? (labels graph) "+")))
      (is (contains? (label-edges expansion) ["5" "x"]))
      (is (contains? (label-edges expansion) ["x" "+"]))
      (is (contains? (label-edges expansion) ["1" "+"]))
      (is (not (contains? (labels graph) "prop:<->"))))))

(deftest semantic-repl-consolidates-explicit-network-output-cells
  (testing "network output applicants collapse to boundary cells with expansion"
    (let [graph (repl/semantic-graph
                 "(let-cell [same next]
                    ((network [x] [same next]
                       (<-> x same)
                       (<-> (+ x 1) next))
                     4 same next)
                    next)")
          edges (label-edges graph)
          expansion (repl/expansion graph {:label "call :: [x]"})
          expansion-edges (label-edges expansion)]
      (is (contains? edges ["4" "call :: [x]"]))
      (is (contains? edges ["call :: [x]" "same"]))
      (is (contains? edges ["call :: [x]" "next"]))
      (is (not (contains? (labels graph) "+")))
      (is (contains? expansion-edges ["4" "x"]))
      (is (contains? expansion-edges ["x" "<->"]))
      (is (contains? expansion-edges ["same" "<->"]))
      (is (contains? expansion-edges ["<->" "x"]))
      (is (contains? expansion-edges ["<->" "same"]))
      (is (contains? expansion-edges ["x" "+"]))
      (is (contains? expansion-edges ["1" "+"]))
      (is (contains? expansion-edges ["+" "<->"]))
      (is (contains? expansion-edges ["next" "<->"]))
      (is (= "4" (get (repl/value-labels expansion) "x")))
      (is (= "4" (get (repl/value-labels graph) "same")))
      (is (= "5" (get (repl/value-labels graph) "next")))
      (is (= 1 (get (label-frequencies graph) "same")))
      (is (= 1 (get (label-frequencies graph) "next")))
      (is (not (contains? edges ["same" "same"])))
      (is (not (contains? edges ["next" "next"])))
      (is (not (contains? (labels graph) "slot same")))
      (is (not (contains? (labels graph) "slot next")))
      (is (not (contains? (labels graph) "slot closure/env"))))))

(deftest semantic-repl-canonicalizes-variable-cell-through-call
  (let [graph (repl/semantic-graph
               "(let-cell []
                  (def out)
                  (-> 42 out)
                  (def inc (network [a] [b] (-> (+ a 1) b)))
                  (def out3)
                  (inc out out3))")
        edges (label-edges graph)]
    (is (= 1 (get (label-frequencies graph) "out")))
    (is (contains? edges ["42" "->"]))
    (is (contains? edges ["->" "out"]))
    (is (contains? edges ["out" "call inc"]))
    (is (contains? edges ["call inc" "out3"]))))
