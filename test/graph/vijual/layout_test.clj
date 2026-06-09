(ns graph.vijual.layout-test
  (:require [clojure.test :refer [deftest is testing]]
            [graph.vijual :as vijual]
            [graph.vijual.layout :as layout]
            [graph.vijual.math :as math]
            [graph.vijual.render :as render]))

(def test-dim
  (assoc render/ascii-dim :line-padding 3))

(defn node-rect [node]
  {:p (:p node) :size (:size node)})

(defn no-node-overlaps? [nodes]
  (let [items (vec (vals nodes))]
    (empty?
     (for [left-index (range (count items))
           right-index (range (inc left-index) (count items))
           :let [left (node-rect (items left-index))
                 right (node-rect (items right-index))]
           :when (math/rect-overlap? left right)]
       [left-index right-index]))))

(defn layout-height [result]
  (apply max 0
         (map (fn [{[_ y] :p [_ height] :size}]
                (+ y height))
              (vals (:nodes result)))))

(defn traced-layout [result]
  (:value (last (:trace result))))

(defn trace-stage [result stage]
  (:value (first (filter #(= stage (:stage %)) (:trace result)))))

(defn link-arrows [result]
  (mapcat (comp (partial map :arrow) :links)
          (vals (:nodes result))))

(defn placed-grid-size [placed]
  [(inc (apply max 0 (map (comp first :grid) (vals (:nodes placed)))))
   (inc (apply max 0 (map (comp second :grid) (vals (:nodes placed)))))])

(deftest normalization-keeps-edge-and-label-ids
  (testing "labels can introduce isolated nodes"
    (let [normalized (layout/normalize-graph [[:a :b]] {:solo "solo"})]
      (is (= #{:a :b :solo} (set (:ids normalized))))
      (is (= "solo" (get-in normalized [:labels :solo]))))))

(deftest deterministic-layout
  (testing "same seed and iteration budget produce the same result"
    (let [opts {:seed 42 :anneal-iterations 20}
          left (vijual/layout-result test-dim [[:a :b] [:b :c]] {} true opts)
          right (vijual/layout-result test-dim [[:a :b] [:b :c]] {} true opts)]
      (is (= (:nodes left) (:nodes right)))
      (is (= (:metrics left) (:metrics right)))))
  (testing "force layout is deterministic with the same options"
    (let [edges [[:a :b] [:b :c] [:c :d] [:d :a]]
          opts {:seed 7 :force-iterations 20}
          left (vijual/layout-force-result test-dim edges {} opts)
          right (vijual/layout-force-result test-dim edges {} opts)]
      (is (= (:nodes left) (:nodes right)))
      (is (= (:metrics left) (:metrics right)))))
  (testing "Sugiyama layout is deterministic with the same options"
    (let [edges [[:a :b] [:a :c] [:b :d] [:c :d]]
          opts {:sugiyama-iterations 4}
          left (vijual/layout-sugiyama-result test-dim edges {} opts)
          right (vijual/layout-sugiyama-result test-dim edges {} opts)]
      (is (= (:nodes left) (:nodes right)))
      (is (= (:metrics left) (:metrics right)))))
  (testing "stress layout is deterministic with the same options"
    (let [edges [[:a :b] [:b :c] [:c :d] [:d :a] [:a :c]]
          opts {:stress-iterations 30
                :stress-aspect-ratio 1.0}
          left (vijual/layout-stress-result test-dim edges {} opts)
          right (vijual/layout-stress-result test-dim edges {} opts)]
      (is (= (:nodes left) (:nodes right)))
      (is (= (:metrics left) (:metrics right))))))

(deftest layout-invariants
  (testing "small directed graph keeps nodes, routes, and orthogonal geometry"
    (let [result (vijual/layout-result test-dim
                                       [[:a :b] [:b :c]]
                                       {:isolated "isolated"}
                                       true
                                       {:seed 7 :anneal-iterations 30})
          nodes (:nodes result)]
      (is (= #{:a :b :c :isolated} (set (keys nodes))))
      (is (no-node-overlaps? nodes))
      (is (= 0 (get-in result [:metrics :segment-alignment-failures])))
      (is (= 2 (get-in result [:metrics :route-count])))
      (is (= :middle (get-in nodes [:a :links 0 :arrow])))))

  (testing "shortest-path directed graph also uses middle arrows"
    (let [result (vijual/layout-result test-dim
                                       [[:a :b]]
                                       {}
                                       true
                                       {:routing :shortest-path
                                        :seed 7
                                        :anneal-iterations 30})]
      (is (= [:middle] (link-arrows result))))))

(deftest force-layout-invariants
  (testing "force layout snaps continuous undirected placement to valid ASCII geometry"
    (let [result (vijual/layout-force-result
                  test-dim
                  [[:a :b] [:b :c] [:c :d] [:d :e]
                   [:e :f] [:f :g] [:g :h] [:h :a]]
                  {}
                  {:seed 1 :force-iterations 50})
          nodes (:nodes result)]
      (is (= #{:a :b :c :d :e :f :g :h} (set (keys nodes))))
      (is (no-node-overlaps? nodes))
      (is (= 0 (get-in result [:metrics :segment-alignment-failures])))
      (is (= 8 (get-in result [:metrics :route-count])))))

  (testing "force-directed directed graph uses middle arrows with both routers"
    (doseq [opts [{:seed 1 :force-iterations 20}
                  {:seed 1 :force-iterations 20
                   :routing :shortest-path}]]
      (let [result (vijual/layout-force-directed-result
                    test-dim
                    [[:a :b] [:b :c]]
                    {}
                    opts)]
        (is (= [:middle :middle] (link-arrows result)))))))

(deftest stress-layout-invariants
  (testing "stress layout balances aspect, keeps close nodes, and removes grid overlap"
    (let [edges [[:a :b] [:b :c] [:c :d] [:d :e] [:e :f]
                 [:f :g] [:g :h] [:h :a] [:a :e] [:b :f]]
          result (vijual/layout-stress-result
                  test-dim edges {}
                  {:stress-iterations 60
                   :stress-aspect-ratio 1.0
                   :trace? true})
          placed (trace-stage result :placed)
          grids (map :grid (vals (:nodes placed)))
          [width height] (placed-grid-size placed)
          ratio (/ (double width) (double height))]
      (is (= #{:a :b :c :d :e :f :g :h} (set (keys (:nodes result)))))
      (is (<= 0.5 ratio 2.0))
      (is (= (count grids) (count (distinct grids))))
      (is (no-node-overlaps? (:nodes result)))
      (is (= 0 (get-in result [:metrics :segment-alignment-failures])))
      (is (= 10 (get-in result [:metrics :route-count])))
      (is (every? (get-in result [:metrics :stress-energy])
                  [:graph-distance-error
                   :node-overlap-penalty
                   :edge-length-variance-penalty
                   :aspect-ratio-penalty
                   :center-balance-penalty
                   :crossing-penalty
                   :wiring-overlap-balance-penalty
                   :total]))))

  (testing "stress energy refinement does not increase total energy"
    (let [edges [[:a :b] [:b :c] [:c :d] [:d :a] [:a :c] [:b :d]]
          base (vijual/layout-stress-result
                test-dim edges {}
                {:stress-iterations 30
                 :stress-refine-iterations 0})
          refined (vijual/layout-stress-result
                   test-dim edges {}
                   {:stress-iterations 30
                    :stress-refine-iterations 80})
          base-energy (get-in base [:metrics :stress-energy :total])
          refined-energy (get-in refined [:metrics :stress-energy :total])]
      (is (<= refined-energy base-energy))))

  (testing "stress wiring target prefers overlap spread across route lanes"
    (let [ids [:a :b :c :d]
          edges [[:a :b] [:c :d]]
          concentrated-grid {:a [0 0] :b [2 0]
                             :c [0 0] :d [2 0]}
          distributed-grid {:a [0 0] :b [2 0]
                            :c [0 1] :d [2 1]}
          concentrated (layout/wiring-overlap-balance-penalty
                        ids edges concentrated-grid)
          distributed (layout/wiring-overlap-balance-penalty
                       ids edges distributed-grid)]
      (is (pos? concentrated))
      (is (zero? distributed))
      (is (< distributed concentrated))))

  (testing "stress node spacing expands the automatic grid"
    (let [edges [[:a :b] [:b :c] [:c :d] [:d :e] [:e :f]]
          tight (vijual/layout-stress-result
                 test-dim edges {}
                 {:stress-iterations 30
                  :stress-node-spacing 0.1
                  :trace? true})
          compact (vijual/layout-stress-result
                   test-dim edges {}
                   {:stress-iterations 30
                    :stress-node-spacing 1.0
                    :trace? true})
          spaced (vijual/layout-stress-result
                  test-dim edges {}
                   {:stress-iterations 30
                    :stress-node-spacing 2.0
                    :trace? true})
          tight-size (:stress-grid-size (trace-stage tight :placed))
          compact-size (:stress-grid-size (trace-stage compact :placed))
          spaced-size (:stress-grid-size (trace-stage spaced :placed))]
      (is (= compact-size tight-size))
      (is (no-node-overlaps? (:nodes tight)))
      (is (every? true? (map <= compact-size spaced-size)))
      (is (< (apply * compact-size)
             (apply * spaced-size)))
      (is (no-node-overlaps? (:nodes spaced)))))

  (testing "stress-directed layout keeps directed middle arrows with both routers"
    (doseq [opts [{:stress-iterations 40}
                  {:stress-iterations 40
                   :routing :shortest-path}]]
      (let [result (vijual/layout-stress-directed-result
                    test-dim
                    [[:a :b] [:b :c]]
                    {}
                    opts)]
        (is (no-node-overlaps? (:nodes result)))
        (is (= [:middle :middle] (link-arrows result)))))))

(deftest sugiyama-layout-invariants
  (testing "Sugiyama layout keeps directed layers and valid routed geometry"
    (let [result (vijual/layout-sugiyama-result
                  test-dim
                  [[:input :parse] [:input :validate]
                   [:parse :plan] [:validate :plan]
                   [:plan :execute] [:execute :report]]
                  {}
                  {:sugiyama-iterations 4})
          nodes (:nodes result)]
      (is (= #{:input :parse :validate :plan :execute :report}
             (set (keys nodes))))
      (is (no-node-overlaps? nodes))
      (is (< (get-in nodes [:input :grid 1])
             (get-in nodes [:report :grid 1])))
      (is (= 0 (get-in result [:metrics :segment-alignment-failures])))
      (is (= 6 (get-in result [:metrics :route-count])))))

  (testing "Sugiyama routes leave the source box through bottom center with middle arrows"
    (let [result (vijual/layout-sugiyama-result
                  test-dim [[:a :b]] {}
                  {:trace? true})
          final-layout (traced-layout result)
          route (first (:routes final-layout))
          source-node (get-in final-layout [:nodes :a])
          link (first (get-in result [:nodes :a :links]))]
      (is (= (layout/node-port-point source-node :bottom)
             (first (:points route))))
      (is (= :middle (:arrow link)))
      (is (false? (:middle-arrow-reversed? link)))))

  (testing "directed routes are physically attached to their source nodes"
    (let [result (vijual/layout-sugiyama-result
                  test-dim
                  [[:x :op] [:one :op] [:op :output]]
                  {:x "x" :one "1" :op "+" :output "output"}
                  {:routing :shortest-path
                   :arrow-position :end})
          link-summary (into {}
                             (map (fn [[id node]]
                                    [id (mapv #(select-keys % [:dest :arrow])
                                              (:links node))]))
                             (:nodes result))]
      (is (= [{:dest :op :arrow :end}]
             (:x link-summary)))
      (is (= [{:dest :op :arrow :end}]
             (:one link-summary)))
      (is (= [{:dest :output :arrow :end}]
             (:op link-summary))))))

(deftest shortest-path-routing-invariants
  (testing "shortest-path routing produces orthogonal routed paths"
    (let [result (vijual/layout-sugiyama-result
                  test-dim
                  [[:input :parse] [:input :validate]
                   [:parse :normalize] [:validate :normalize]
                   [:normalize :plan] [:plan :execute]]
                  {}
                  {:routing :shortest-path
                   :sugiyama-iterations 4
                   :trace? true})
          routes (get-in result [:trace 4 :value :routes])]
      (is (= 0 (get-in result [:metrics :segment-alignment-failures])))
      (is (= 6 (get-in result [:metrics :route-count])))
      (is (every? #(= :shortest-path (:routing %)) routes))))

  (testing "shortest-path Sugiyama defaults to compact routing bounds"
    (let [edges [[:a :e] [:b :e] [:c :e] [:d :e]
                 [:e :f] [:e :g] [:e :h] [:e :i]]
          compacted (vijual/layout-sugiyama-result
                     test-dim edges {}
                     {:routing :shortest-path})
          expanded (vijual/layout-sugiyama-result
                    test-dim edges {}
                    {:routing :shortest-path
                     :compact? false})]
      (is (< (layout-height compacted)
             (layout-height expanded)))
      (is (no-node-overlaps? (:nodes compacted)))
      (is (= 0 (get-in compacted [:metrics :segment-alignment-failures])))))

  (testing "shortest-path Sugiyama also starts from the source bottom port"
    (let [result (vijual/layout-sugiyama-result
                  test-dim [[:a :b]] {}
                  {:routing :shortest-path
                   :trace? true})
          final-layout (traced-layout result)
          route (first (:routes final-layout))
          source-node (get-in final-layout [:nodes :a])
          link (first (get-in result [:nodes :a :links]))]
      (is (= (layout/integer-node-port-point source-node :bottom)
             (first (:points route))))
      (is (= :middle (:arrow link)))
      (is (false? (:middle-arrow-reversed? link))))))

(deftest trace-is-optional
  (testing "trace is empty unless requested"
    (is (empty? (:trace (vijual/layout-result test-dim [[:a :b]] {} false
                                              {:seed 1 :anneal-iterations 1}))))
    (is (= [:normalized :placed :routed :expanded :compacted]
           (mapv :stage (:trace (vijual/layout-result test-dim [[:a :b]] {} false
                                                      {:seed 1
                                                       :anneal-iterations 1
                                                       :trace? true})))))))

(deftest edge-cases
  (testing "empty graph, isolated node, self-loop, and duplicate edge inputs"
    (is (empty? (:nodes (vijual/layout-result test-dim [] {} false
                                              {:seed 1 :anneal-iterations 1}))))
    (is (= #{:solo}
           (set (keys (:nodes (vijual/layout-result test-dim [] {:solo "solo"} false
                                                   {:seed 1 :anneal-iterations 1}))))))
    (is (= 1 (get-in (vijual/layout-result test-dim [[:a :a]] {} true
                                           {:seed 1 :anneal-iterations 1})
                     [:metrics :route-count])))
    (is (= 1 (get-in (vijual/layout-result test-dim [[:a :b] [:a :b]] {} false
                                           {:seed 1 :anneal-iterations 1})
                     [:metrics :route-count])))))
