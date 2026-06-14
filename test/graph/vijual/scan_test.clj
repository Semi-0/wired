(ns graph.vijual.scan-test
  (:require [clojure.test :refer [deftest is testing]]
            [graph.vijual.scan :as scan]))

(deftest scan-packing
  (testing "scan-add reserves occupied horizontal ranges"
    (let [packed-scan (scan/scan-add [[0 0]] 0 5 4)]
      (is (= 5 (scan/scan-lowest-y packed-scan 1 2)))
      (is (= 0 (scan/scan-lowest-y packed-scan 5 2)))))
  (testing "packed rectangles do not overlap their previous scan"
    (let [rects [{:p [0 0] :size [4 2]}
                 {:p [1 0] :size [2 2]}
                 {:p [5 0] :size [2 2]}]
          packed (:rects (scan/pack-rects 1 rects))]
      (is (= [[0 0] [1 3] [5 0]] (mapv :p packed))))))
