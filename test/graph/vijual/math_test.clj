(ns graph.vijual.math-test
  (:require [clojure.test :refer [deftest is testing]]
            [graph.vijual.layout :as layout]
            [graph.vijual.math :as math]))

(deftest vector-matrix-arithmetic
  (testing "small vector and matrix primitives"
    (is (= [4 6] (math/v+ [1 2] [3 4])))
    (is (= [-2 -2] (math/v- [1 2] [3 4])))
    (is (= [6 9] (math/v* 3 [2 3])))
    (is (= 7 (math/manhattan [1 2] [5 5])))
    (is (= [8 3] (math/mirror-point [3 8])))
    (is (= [3 8] (math/mirror-point (math/mirror-point [3 8]))))))

(deftest geometry-predicates
  (testing "rectangles and segment orientation"
    (is (math/rect-overlap? {:p [0 0] :size [3 3]}
                            {:p [2 2] :size [3 3]}))
    (is (not (math/rect-overlap? {:p [0 0] :size [3 3]}
                                 {:p [3 3] :size [3 3]})))
    (is (math/orthogonal-segment? {:from [0 0] :to [0 4]}))
    (is (math/orthogonal-segment? {:from [0 0] :to [4 0]}))
    (is (not (math/orthogonal-segment? {:from [0 0] :to [4 4]})))))

(deftest logical-routing-is-orthogonal
  (testing "basic grid cases produce only orthogonal segments"
    (doseq [[from to] [[[0 0] [2 0]]
                       [[0 0] [0 2]]
                       [[0 1] [1 1]]
                       [[1 1] [1 0]]
                       [[0 2] [2 0]]
                       [[0 0] [0 0]]]]
      (let [points (layout/route-logical-points from to)
            segments (math/segment-pairs points)]
        (is (= (layout/logical-center from) (first points)))
        (is (= (layout/logical-center to) (last points)))
        (is (every? math/orthogonal-segment? segments))))))
