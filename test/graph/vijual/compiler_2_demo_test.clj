(ns graph.vijual.compiler-2-demo-test
  (:require [clojure.test :refer [deftest is testing]]
            [graph.vijual-compiler-2-demo :as demo]))

(defn stage-labels [stage]
  (set (vals (:nodes (demo/network-vijual-graph (:compiled stage)
                                                (:network stage))))))

(defn graph-labels [graph]
  (set (vals (:nodes graph))))

(defn graph-label-edges [graph]
  (let [nodes (:nodes graph)]
    (set (map (fn [[from to]]
                [(nodes from) (nodes to)])
              (:edges graph)))))

(deftest compiler-2-demo-labels-compiler-graph-from-metadata
  (testing "main graph labels use env bindings plus application IR"
    (let [[main-stage expanded-stage] (demo/compiled-progression demo/source)
          labels (stage-labels main-stage)]
      (is (contains? labels "inc-local"))
      (is (contains? labels ":: [x]"))
      (is (contains? labels "5"))
      (is (contains? labels "app:<->"))
      (is (contains? labels "prop:<->"))
      (is (contains? labels "op:<->"))
      (is (contains? labels "app:inc-local"))
      (is (contains? labels "prop:inc-local"))
      (is (contains? labels "args:inc-local"))
      (is (contains? (stage-labels expanded-stage) "result"))))

  (testing "closure body subgraph labels its argument and primitive application"
    (let [[_main-stage expanded-stage] (demo/compiled-progression demo/source)
          [closure-stage] (demo/closure-body-progression (:network expanded-stage))
          labels (stage-labels closure-stage)]
      (is (contains? labels "x"))
      (is (contains? labels "1"))
      (is (contains? labels "app:+"))
      (is (contains? labels "prop:+"))
      (is (contains? labels "op:+"))
      (is (contains? labels "args:+")))))

(deftest compiler-2-demo-infers-semantic-graph
  (testing "main semantic graph collapses sync and closure call wiring"
    (let [[_main-stage expanded-stage] (demo/compiled-progression demo/source)
          [semantic-main] (demo/semantic-progression (:compiled expanded-stage)
                                                     (:network expanded-stage))
          graph (:graph semantic-main)
          labels (graph-labels graph)
          edges (graph-label-edges graph)]
      (is (= #{"inc-local" ":: [x]" "<->"
               "call inc-local" "5" "result"}
             labels))
      (is (contains? edges ["inc-local" "<->"]))
      (is (contains? edges [":: [x]" "<->"]))
      (is (contains? edges ["<->" "inc-local"]))
      (is (contains? edges ["<->" ":: [x]"]))
      (is (contains? edges ["inc-local" "call inc-local"]))
      (is (contains? edges ["5" "call inc-local"]))
      (is (contains? edges ["call inc-local" "result"]))))

  (testing "closure semantic graph keeps only operator, inputs, and output"
    (let [[_main-stage expanded-stage] (demo/compiled-progression demo/source)
          [_semantic-main semantic-closure] (demo/semantic-progression
                                             (:compiled expanded-stage)
                                             (:network expanded-stage))
          graph (:graph semantic-closure)
          labels (graph-labels graph)
          edges (graph-label-edges graph)]
      (is (= #{"x" "1" "+" "output"} labels))
      (is (contains? edges ["x" "+"]))
      (is (contains? edges ["1" "+"]))
      (is (contains? edges ["+" "output"])))))
