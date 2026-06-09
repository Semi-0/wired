(ns graph.vijual.render-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [graph.vijual :as vijual]
            [graph.vijual.render :as render]))

(deftest graph-vijual-loads
  (testing "public facade resolves on current Clojure"
    (is (fn? vijual/draw-graph))
    (is (fn? vijual/draw-force-graph))
    (is (fn? vijual/draw-force-directed-graph))
    (is (fn? vijual/draw-stress-graph))
    (is (fn? vijual/draw-stress-directed-graph))
    (is (fn? vijual/draw-sugiyama-graph))
    (is (fn? vijual/draw-graph-comparison))
    (is (fn? vijual/draw-directed-graph-comparison))
    (is (fn? vijual/draw-directed-graph))
    (is (fn? vijual/layout-graph))
    (is (fn? vijual/layout-force-graph))
    (is (fn? vijual/layout-force-directed-graph))
    (is (fn? vijual/layout-stress-graph))
    (is (fn? vijual/layout-stress-directed-graph))
    (is (fn? vijual/layout-sugiyama-graph))
    (is (fn? vijual/draw-graph-image))))

(deftest render-integration
  (testing "ASCII render contains node labels"
    (let [text (with-out-str
                 (vijual/draw-graph [[:alpha :beta]]
                                    {:alpha "alpha" :beta "beta"}))]
      (is (str/includes? text "alpha"))
      (is (str/includes? text "beta")))))

(deftest directed-render-shows-connection
  (testing "directed edges render visible connector strokes and arrowheads"
    (let [nodes (vals (:nodes (vijual/layout-result render/ascii-dim
                                                    [[:a :b]]
                                                    {}
                                                    true
                                                    {:seed 1
                                                     :anneal-iterations 1})))
          text (render/draw-shapes-string
                render/ascii-dim
                (render/integer-shapes
                 (render/graph-to-shapes render/ascii-dim nodes)))
          shape-types (set (map :type (render/graph-to-shapes render/ascii-dim nodes)))]
      (is (re-find #"[<>^v]" text))
      (is (re-find #"[·|][<>^v][·|]" text))
      (is (str/includes? text "·"))
      (is (str/includes? text "+---+"))
      (is (contains? shape-types :line)))))

(deftest line-joints-render-clearly
  (testing "intersecting line segments render a joint glyph"
    (let [text (render/draw-shapes-string
                render/ascii-dim
                [{:type :line :x 0 :y 1 :width 3 :height 1 :dir :right}
                 {:type :line :x 1 :y 0 :width 1 :height 3 :dir :down}])]
      (is (str/includes? text "+")))))

(deftest middle-arrow-renders-on-link
  (testing "middle arrows are emitted halfway along the connector"
    (let [arrow (first (filter #(= :arrow (:type %))
                               (render/link-shapes
                                render/ascii-dim
                                {:arrow :middle
                                 :legs [{:xpos 0 :ypos 0 :dir :right}
                                        {:xpos 4 :ypos 0 :dir :right}]})))]
      (is (= [2 0] [(:x arrow) (:y arrow)]))
      (is (= :right (:dir arrow)))))

  (testing "middle arrows can point opposite the stored route direction"
    (let [arrow (first (filter #(= :arrow (:type %))
                               (render/link-shapes
                                render/ascii-dim
                                {:arrow :middle
                                 :middle-arrow-reversed? true
                                 :legs [{:xpos 0 :ypos 0 :dir :right}
                                        {:xpos 4 :ypos 0 :dir :right}]})))]
      (is (= :left (:dir arrow))))))
