(ns graph.vijual.layout
  (:require [graph.vijual.layout.anneal :as anneal]
            [graph.vijual.layout.force :as force]
            [graph.vijual.layout.stress :as stress]
            [graph.vijual.layout.sugiyama :as sugiyama]
            [graph.vijual.math :as m]
            [graph.vijual.scan :as scan]))

(def default-options
  {:anneal-iterations nil
   :seed nil
   :trace? false})

(defn label-text [text]
  (if (keyword? text)
    (apply str (replace {\- \space} (name text)))
    (str text)))

(defn graph-side [nodes]
  (m/ceil-int (Math/sqrt (max 1 (count nodes)))))

(defn grid-axis-size [nodes axis-index]
  (max 1
       (inc (apply max -1
                   (keep #(get-in % [:grid axis-index])
                         (vals nodes))))))

(defn normalize-graph [edges labels]
  (let [edge-list (vec (mapv vec edges))
        edge-ids (mapcat identity edge-list)
        label-ids (keys labels)
        ids (vec (distinct (concat edge-ids label-ids)))]
    {:ids ids
     :edges edge-list
     :labels (into {} (map (fn [id]
                             [id (label-text (get labels id id))])
                           ids))}))

(defn undirected-edges [edges]
  (vec (distinct (mapcat (fn [[a b]]
                           [[a b] [b a]])
                         edges))))

(defn canonical-edge [grid [a b]]
  (let [[ax ay] (get grid a)
        [bx by] (get grid b)]
    (if (or (< ax bx)
            (and (= ax bx) (<= ay by)))
      [a b]
      [b a])))

(defn tension [edges nodes directed?]
  (reduce + 0
          (map (fn [[a b]]
                 (let [[ax ay] (get-in nodes [a :grid])
                       [bx by] (get-in nodes [b :grid])
                       distance (m/manhattan [ax ay] [bx by])
                       penalty (if (and directed? (> ay by)) 2 1)]
                   (* distance penalty)))
               edges)))

(def default-stress-energy-weights
  stress/default-stress-energy-weights)

(def wiring-overlap-balance-penalty
  stress/wiring-overlap-balance-penalty)

(defn logical-center [[x y]]
  [(inc (* 2 x)) (inc (* 2 y))])

(defn route-logical-points [[x y :as from-grid] [bx by :as to-grid]]
  (let [[sx sy :as start] (logical-center from-grid)
        [ex ey :as end] (logical-center to-grid)]
    (cond
      (= start end)
      [start [(inc sx) sy] [(inc sx) (inc sy)] [sx (inc sy)] start]

      (and (= y by) (not= (inc x) bx))
      [start [sx (+ (* y 2) 2)] [ex (+ (* y 2) 2)] end]

      (= (dec y) by)
      [start [sx (* y 2)] [ex (* y 2)] end]

      (= (inc y) by)
      [start [sx (+ (* y 2) 2)] [ex (* by 2)] end]

      (= x bx)
      [start [(+ (* x 2) 2) sy] [(+ (* x 2) 2) ey] end]

      (= (inc x) bx)
      [start [(+ (* x 2) 2) sy] [(+ (* x 2) 2) ey] end]

      (< by y)
      [start [(+ (* x 2) 2) sy] [(+ (* x 2) 2) (+ (* by 2) 2)]
       [ex (+ (* by 2) 2)] end]

      :else
      [start [(+ (* x 2) 2) sy] [(+ (* x 2) 2) (* by 2)]
       [ex (* by 2)] end])))

(defn route-edges [normalized placed directed?]
  (let [nodes (:nodes placed)
        grid (into {} (map (fn [[id node]] [id (:grid node)]) nodes))
        routed-edges (if directed?
                       (vec (distinct (:edges normalized)))
                       (vec (distinct (map (partial canonical-edge grid)
                                           (:edges normalized)))))]
    (vec (map-indexed (fn [index [a b]]
                        {:id index
                         :source a
                         :dest b
                         :directed? directed?
                         :logical-points (route-logical-points (grid a) (grid b))})
                      routed-edges))))

(defn measure-grid [dim nodes]
  (let [col-count (grid-axis-size nodes 0)
        row-count (grid-axis-size nodes 1)
        by-row (group-by (comp second :grid) (vals nodes))
        by-col (group-by (comp first :grid) (vals nodes))
        width-fn (:width-fn dim)
        height-fn (:height-fn dim)]
    {:row-heights (vec (for [y (range row-count)]
                         (apply max 0 (map #(height-fn (:text %)) (by-row y)))))
     :col-widths (vec (for [x (range col-count)]
                        (apply max 0 (map #(width-fn (:text %)) (by-col x)))))}))

(defn segment-records [routes]
  (mapcat (fn [{:keys [id logical-points]}]
            (map-indexed (fn [segment-index [from to]]
                           (let [dir (m/direction from to)]
                             {:route-id id
                              :segment-index segment-index
                              :from from
                              :to to
                              :dir dir}))
                         (partition 2 1 logical-points)))
          routes))

(defn lane-segment [{:keys [route-id segment-index from dir]}]
  (let [[x y] from]
    (cond
      (and (m/vertical? dir) (even? x))
      {:axis :x :lane (quot x 2) :route-id route-id :segment-index segment-index}

      (and (m/horizontal? dir) (even? y))
      {:axis :y :lane (quot y 2) :route-id route-id :segment-index segment-index}

      :else nil)))

(defn route-lane-data [routes]
  (let [items (keep lane-segment (segment-records routes))
        groups (group-by (juxt :axis :lane) items)
        lane-indexes (into {}
                           (mapcat (fn [[[axis lane] items]]
                                     (map-indexed
                                      (fn [index {:keys [route-id segment-index]}]
                                        [[route-id segment-index axis] index])
                                      (sort-by (juxt :route-id :segment-index) items)))
                                   groups))
        point-lanes (reduce (fn [point-lanes {:keys [route-id segment-index axis]}]
                              (let [lane-index (lane-indexes [route-id segment-index axis])]
                                (-> point-lanes
                                    (assoc [route-id segment-index axis] lane-index)
                                    (assoc [route-id (inc segment-index) axis] lane-index))))
                            {}
                            items)]
    {:counts (reduce (fn [counts [[axis lane] items]]
                       (assoc-in counts [axis lane] (count items)))
                     {:x {} :y {}}
                     groups)
     :point-lanes point-lanes}))

(defn lane-gap [{:keys [line-padding line-wid min-edge-gap]} lane-count]
  (max (or min-edge-gap line-padding)
       (+ line-padding (* (or lane-count 0) (+ line-padding line-wid)))))

(defn spacing-prefixes
  ([dim measurements]
   (spacing-prefixes dim measurements {:x {} :y {}}))
  ([dim measurements lane-counts]
   {:row-tops (m/prefix-sums
               (map-indexed (fn [index height]
                              (+ height (lane-gap dim (get-in lane-counts [:y (inc index)]))))
                            (:row-heights measurements)))
    :col-lefts (m/prefix-sums
                (map-indexed (fn [index width]
                               (+ width (lane-gap dim (get-in lane-counts [:x (inc index)]))))
                             (:col-widths measurements)))}))

(defn node-geometry [dim measurements prefixes node]
  (let [[gx gy] (:grid node)
        width ((:width-fn dim) (:text node))
        height ((:height-fn dim) (:text node))
        col-width (get-in measurements [:col-widths gx] width)
        row-height (get-in measurements [:row-heights gy] height)
        x (+ (get-in prefixes [:col-lefts gx]) (m/half (- col-width width)))
        y (+ (get-in prefixes [:row-tops gy]) (m/half (- row-height height)))]
    (assoc node :p [x y] :size [width height] :width width :height height)))

(defn lane-coordinate [prefixes widths index]
  (let [lane (quot index 2)]
    (if (odd? index)
      (+ (prefixes lane) (m/half (widths lane)))
      (max 0 (- (prefixes lane) 1)))))

(defn expand-point [measurements prefixes [x y]]
  [(lane-coordinate (:col-lefts prefixes) (:col-widths measurements) x)
   (lane-coordinate (:row-tops prefixes) (:row-heights measurements) y)])

(defn expand-layout [dim normalized placed routes]
  (let [measurements (measure-grid dim (:nodes placed))
        lane-data (route-lane-data routes)
        prefixes (spacing-prefixes dim measurements (:counts lane-data))
        nodes (into {} (map (fn [[id node]]
                              [id (node-geometry dim measurements prefixes node)])
                            (:nodes placed)))
        expanded-routes (mapv (fn [route]
                                (assoc route
                                       :points
                                       (mapv (partial expand-point measurements prefixes)
                                             (:logical-points route))))
                              routes)]
    {:nodes nodes
     :routes expanded-routes
     :measurements measurements
     :lane-data lane-data}))

(defn node-rect [node]
  {:id (:id node) :p (:p node) :size (:size node)})

(defn overlap-count [nodes]
  (let [rects (mapv node-rect (vals nodes))]
    (count (for [a rects
                 b rects
                 :when (and (neg? (compare (:id a) (:id b)))
                            (m/rect-overlap? a b))]
             [(:id a) (:id b)]))))

(defn segment-alignment-failures [routes]
  (count (remove m/orthogonal-segment?
                 (mapcat (comp m/segment-pairs :points) routes))))

(defn compact-padding [{:keys [line-padding min-edge-gap]}]
  (max line-padding (or min-edge-gap line-padding)))

(defn compact-vertical [dim layout]
  (let [rects (sort-by (juxt (comp second :p) (comp first :p))
                       (map node-rect (vals (:nodes layout))))
        packed (:rects (scan/pack-rects (compact-padding dim) rects))
        y-by-id (into {} (map (fn [{:keys [id p]}] [id (second p)]) packed))]
    (update layout :nodes
            (fn [nodes]
              (into {} (map (fn [[id node]]
                              [id (assoc-in node [:p 1] (y-by-id id))])
                            nodes))))))

(defn mirror-node [node]
  (let [[w h] (:size node)]
    (assoc node
           :grid (m/mirror-point (:grid node))
           :p (m/mirror-point (:p node))
           :size [h w]
           :width h
           :height w)))

(defn mirror-route [route]
  (update route :points #(mapv m/mirror-point %)))

(defn mirror-layout [layout]
  (-> layout
      (update :nodes #(into {} (map (fn [[id node]]
                                      [id (mirror-node node)])
                                    %)))
      (update :routes #(mapv mirror-route %))))

(defn route-direction [points]
  (let [[from to] (take 2 points)]
    (m/direction from to)))

(defn node-center [{[x y] :p [width height] :size}]
  [(+ x (m/half width)) (+ y (m/half height))])

(defn outside-node-point [{[x y] :p [width height] :size :as node} dir]
  (let [[cx cy] (node-center node)]
    (case dir
      :right [(+ x width) cy]
      :left [(dec x) cy]
      :down [cx (+ y height)]
      :up [cx (dec y)]
      [cx cy])))

(def port-direction
  {:right :right
   :left :left
   :bottom :down
   :top :up})

(defn node-port-point [node port]
  (outside-node-point node (port-direction port)))

(def reverse-dir
  {:right :left
   :left :right
   :down :up
   :up :down})

(def grid-directions
  [{:dir :right :delta [1 0]}
   {:dir :down :delta [0 1]}
   {:dir :left :delta [-1 0]}
   {:dir :up :delta [0 -1]}])

(defn integer-node-rect [{[x y] :p [width height] :size}]
  (let [ix (m/floor-int x)
        iy (m/floor-int y)
        rx (m/floor-int (+ x width))
        by (m/floor-int (+ y height))]
    {:x ix
     :y iy
     :width (max 1 (- rx ix))
     :height (max 1 (- by iy))}))

(defn integer-node-center [node]
  (let [{:keys [x y width height]} (integer-node-rect node)]
    [(m/floor-int (+ x (m/half width)))
     (m/floor-int (+ y (m/half height)))]))

(defn integer-node-port-point [node port]
  (let [{:keys [x y width height]} (integer-node-rect node)
        [cx cy] (integer-node-center node)]
    (case port
      :right [(+ x width) cy]
      :left [(dec x) cy]
      :bottom [cx (+ y height)]
      :top [cx (dec y)]
      [cx cy])))

(defn rect-cells [{:keys [x y width height]}]
  (for [px (range x (+ x width))
        py (range y (+ y height))]
    [px py]))

(defn occupied-cells [nodes]
  (set (mapcat (comp rect-cells integer-node-rect) (vals nodes))))

(defn route-bounds [nodes]
  (let [rects (map integer-node-rect (vals nodes))
        max-x (apply max 0 (map (fn [{:keys [x width]}] (+ x width)) rects))
        max-y (apply max 0 (map (fn [{:keys [y height]}] (+ y height)) rects))]
    {:min-x 0
     :min-y 0
     :max-x (+ max-x 2)
     :max-y (+ max-y 2)}))

(defn in-bounds? [{:keys [min-x max-x min-y max-y]} [x y]]
  (and (<= min-x x max-x)
       (<= min-y y max-y)))

(defn route-port-candidates
  ([bounds node]
   (route-port-candidates bounds node [:right :left :bottom :top]))
  ([bounds node ports]
   (let [ports (if (sequential? ports) ports [ports])]
     (vec (filter #(in-bounds? bounds %)
                  (map (partial integer-node-port-point node) ports))))))

(defn path-cells [points]
  (mapcat (fn [[[x1 y1] [x2 y2]]]
            (cond
              (= x1 x2)
              (map (fn [y] [x1 y])
                   (range (min y1 y2) (inc (max y1 y2))))

              (= y1 y2)
              (map (fn [x] [x y1])
                   (range (min x1 x2) (inc (max x1 x2))))

              :else
          []))
          (partition 2 1 points)))

(declare physical-route-points)

(defn reconstruct-states [previous state]
  (loop [state state
         states ()]
    (if state
      (recur (previous state) (cons state states))
      (vec states))))

(defn compress-path [points]
  (let [points (vec (dedupe points))]
    (if (< (count points) 3)
      points
      (let [middle (keep-indexed
                    (fn [index point]
                      (let [prev-point (points index)
                            next-point (points (+ index 2))]
                        (when (not= (m/direction prev-point point)
                                    (m/direction point next-point))
                          point)))
                    (subvec points 1 (dec (count points))))]
        (vec (concat [(first points)] middle [(last points)]))))))

(defn shortest-path
  [bounds occupied used source-points dest-points
   {:keys [shortest-turn-cost shortest-used-cost]}]
  (let [turn-cost (or shortest-turn-cost 3)
        used-cost (or shortest-used-cost 2)
        dest-set (set dest-points)
        queue (java.util.PriorityQueue.
               11
               (reify java.util.Comparator
                 (compare [_ left right]
                   (compare (:cost left) (:cost right)))))
        add-state! (fn [state cost]
                     (.add queue {:state state :cost cost}))]
    (doseq [point source-points]
      (add-state! [point nil] 0))
    (loop [dist (into {} (map (fn [point] [[point nil] 0]) source-points))
           previous {}
           visited #{}]
      (if-let [{:keys [state cost]} (.poll queue)]
        (if (visited state)
          (recur dist previous visited)
          (let [[point dir] state]
            (if (dest-set point)
              (compress-path (mapv first (reconstruct-states previous state)))
              (let [[dist previous]
                    (reduce (fn [[dist previous] {:keys [dir delta]}]
                              (let [next-point (m/v+ point delta)
                                    next-state [next-point dir]]
                                (if (or (not (in-bounds? bounds next-point))
                                        (occupied next-point))
                                  [dist previous]
                                  (let [next-cost (+ cost
                                                     1
                                                     (if (or (nil? (second state))
                                                             (= (second state) dir))
                                                       0
                                                       turn-cost)
                                                     (if (used next-point)
                                                       used-cost
                                                       0))
                                        old-cost (get dist next-state ##Inf)]
                                    (if (< next-cost old-cost)
                                      (do
                                        (add-state! next-state next-cost)
                                        [(assoc dist next-state next-cost)
                                         (assoc previous next-state state)])
                                      [dist previous])))))
                            [dist previous]
                            grid-directions)]
                (recur dist previous (conj visited state))))))
        nil))))

(defn shortest-path-route [dim nodes lane-data bounds occupied used route]
  (let [{:keys [source dest source-port]} route
        source-node (nodes source)
        dest-node (nodes dest)]
    (if (= source dest)
      [(physical-route-points dim nodes lane-data route) used]
      (let [source-points (remove occupied
                                  (route-port-candidates bounds source-node
                                                         (or source-port
                                                             [:right :left :bottom :top])))
            dest-points (remove occupied (route-port-candidates bounds dest-node))
            points (shortest-path bounds occupied used source-points dest-points dim)]
        (if (seq points)
          [(assoc route
                  :points points
                  :arrow-points points
                  :routing :shortest-path)
           (into used (path-cells points))]
          [(physical-route-points dim nodes lane-data route) used])))))

(defn shortest-path-routes [dim nodes routes lane-data]
  (let [bounds (route-bounds nodes)
        occupied (occupied-cells nodes)]
    (first
     (reduce (fn [[routed used] route]
               (let [[next-route next-used]
                     (shortest-path-route dim nodes lane-data bounds occupied used route)]
                 [(conj routed next-route) next-used]))
             [[] #{}]
             routes))))

(defn axis-lane-coordinate [{:keys [line-padding line-wid]} nodes axis lane lane-index]
  (let [axis-index (if (= axis :x) 0 1)
        size-index (if (= axis :x) 0 1)
        axis-nodes (vals nodes)
        starts (keep (fn [node]
                       (if (= lane (get-in node [:grid axis-index]))
                         (get-in node [:p axis-index])
                         nil))
                     axis-nodes)
        previous-ends (keep (fn [node]
                              (if (< (get-in node [:grid axis-index]) lane)
                                (+ (get-in node [:p axis-index])
                                   (get-in node [:size size-index]))
                                nil))
                            axis-nodes)]
    (if (seq previous-ends)
      (+ (apply max previous-ends)
         line-padding
         (* (or lane-index 0) (+ line-padding line-wid)))
      (if (seq starts)
        (max 0 (- (apply min starts)
                  line-wid
                  (* (inc (or lane-index 0)) (+ line-padding line-wid))))
        0))))

(defn route-axis-coordinate
  [dim nodes lane-data source-node dest-node axis source-logical dest-logical
   route-id point-index value]
  (let [coord-index (if (= axis :x) 0 1)
        point-lanes (:point-lanes lane-data)]
    (cond
      (and (= value source-logical) (= value dest-logical))
      (get (node-center source-node) coord-index)

      (= value source-logical)
      (get (node-center source-node) coord-index)

      (= value dest-logical)
      (get (node-center dest-node) coord-index)

      :else
      (axis-lane-coordinate dim nodes axis (quot value 2)
                            (get point-lanes [route-id point-index axis])))))

(defn orthogonal-points? [[ax ay] [bx by]]
  (or (= ax bx) (= ay by)))

(defn outward-from-port? [port [px py] [nx ny]]
  (case port
    :bottom (or (= py ny) (>= ny py))
    :top (or (= py ny) (<= ny py))
    :right (or (= px nx) (>= nx px))
    :left (or (= px nx) (<= nx px))
    true))

(defn port-stub-distance [{:keys [line-padding]}]
  (max 1 (or line-padding 1)))

(defn port-stub-point [dim [x y] port]
  (let [distance (port-stub-distance dim)]
    (case port
      :bottom [x (+ y distance)]
      :top [x (- y distance)]
      :right [(+ x distance) y]
      :left [(- x distance) y]
      [x y])))

(defn port-elbow-point [port stub-point next-point]
  (if (contains? #{:bottom :top} port)
    [(first next-point) (second stub-point)]
    [(first stub-point) (second next-point)]))

(defn force-source-port [dim source-node source-port points]
  (if-not source-port
    points
    (let [source-point (node-port-point source-node source-port)]
      (if (= source-point (first points))
        points
        (let [tail (vec (rest points))
              next-point (first tail)]
          (cond
            (nil? next-point)
            [source-point]

            (and (orthogonal-points? source-point next-point)
                 (outward-from-port? source-port source-point next-point))
            (vec (dedupe (cons source-point tail)))

            :else
            (let [stub-point (port-stub-point dim source-point source-port)
                  elbow-point (port-elbow-point source-port stub-point next-point)]
              (vec (dedupe (concat [source-point stub-point elbow-point]
                                   tail))))))))))

(defn physical-route-points [dim nodes lane-data
                             {:keys [id source dest logical-points source-port] :as route}]
  (let [source-node (nodes source)
        dest-node (nodes dest)
        [source-logical-x source-logical-y] (first logical-points)
        [dest-logical-x dest-logical-y] (last logical-points)
        first-dir (m/direction (first logical-points) (second logical-points))
        last-dir (m/direction (last (butlast logical-points)) (last logical-points))
        physical-point (fn [point-index [x y]]
                         [(route-axis-coordinate dim nodes lane-data source-node dest-node
                                                 :x source-logical-x dest-logical-x
                                                 id point-index x)
                          (route-axis-coordinate dim nodes lane-data source-node dest-node
                                                 :y source-logical-y dest-logical-y
                                                 id point-index y)])
        inner-points (mapv (fn [point-index logical-point]
                             (physical-point point-index logical-point))
                           (range 1 (dec (count logical-points)))
                           (-> logical-points butlast rest))
        points (vec (dedupe
                     (concat [(outside-node-point source-node first-dir)]
                             inner-points
                             [(outside-node-point dest-node (reverse-dir last-dir))])))]
    (assoc route :points
           (force-source-port dim source-node source-port points))))

(defn physical-routes [dim nodes routes lane-data]
  (mapv (partial physical-route-points dim nodes lane-data) routes))

(defn route-style [opts]
  (select-keys opts [:source-port :dest-port :arrow-position]))

(defn attach-routes [nodes routes directed-edges]
  (let [directed-set (set directed-edges)
        links-by-source (group-by :source routes)]
    (into {}
          (map (fn [[id node]]
                 [id (assoc node
                            :xpos (first (:p node))
                            :ypos (second (:p node))
                            :links (vec
                                    (map (fn [{:keys [dest points source directed?
                                                       arrow-position]}]
                                           (let [first-dir (m/direction (first points)
                                                                        (second points))
                                                 last-dir (m/direction (last (butlast points))
                                                                       (last points))
                                                 route-forward? (directed-set [source dest])
                                                 arrow (cond
                                                         (not directed?) nil
                                                         (= arrow-position :middle) :middle
                                                         route-forward? :end
                                                         :else :start)]
                                             {:dest dest
                                              :arrow arrow
                                              :middle-arrow-reversed?
                                              (and directed?
                                                   (= arrow-position :middle)
                                                   (not route-forward?))
                                              :start-arrow-dir (reverse-dir first-dir)
                                              :end-arrow-dir last-dir
                                              :legs (vec
                                                     (map (fn [point next-point]
                                                            (let [dir (m/direction point next-point)]
                                                              {:xpos (first point)
                                                               :ypos (second point)
                                                               :dir dir}))
                                                          points
                                                          (concat (next points) [(last points)])))}))
                                         (links-by-source id))))])
               nodes))))

(defn metrics [placed layout]
  (cond-> {:tension (:tension placed)
           :node-overlaps (overlap-count (:nodes layout))
           :segment-alignment-failures (segment-alignment-failures (:routes layout))
           :route-count (count (:routes layout))}
    (:stress-energy placed)
    (assoc :stress-energy (:stress-energy placed))))

(defn trace-step [trace? name value]
  (if trace?
    [{:stage name :value value}]
    []))

(defn layout-result-with-placement [dim edges nodes directed? opts placement-fn]
  (let [opts (merge default-options
                    (if directed? {:arrow-position :middle} {})
                    opts)
        normalized (normalize-graph edges nodes)
        placed (placement-fn normalized directed? opts)
        routed (mapv #(merge (route-style opts) %)
                     (route-edges normalized placed directed?))
        expanded (expand-layout dim normalized placed routed)
        compacted (if (false? (:compact? opts))
                    expanded
                    (->> expanded
                         (compact-vertical dim)
                         mirror-layout
                         (compact-vertical dim)
                         mirror-layout))
        final-routes (if (= :shortest-path (:routing opts))
                       (shortest-path-routes dim (:nodes compacted) (:routes compacted)
                                             (:lane-data compacted))
                       (physical-routes dim (:nodes compacted) (:routes compacted)
                                        (:lane-data compacted)))
        final-layout (assoc compacted :routes final-routes)
        attached (attach-routes (:nodes final-layout) (:routes final-layout) (:edges normalized))]
    {:nodes attached
     :metrics (metrics placed (assoc final-layout :nodes attached))
     :trace (vec (concat (trace-step (:trace? opts) :normalized normalized)
                         (trace-step (:trace? opts) :placed placed)
                         (trace-step (:trace? opts) :routed routed)
                         (trace-step (:trace? opts) :expanded expanded)
                         (trace-step (:trace? opts) :compacted final-layout)))}))

(defn layout-result
  ([dim edges nodes directed?]
   (layout-result dim edges nodes directed? {}))
  ([dim edges nodes directed? opts]
   (layout-result-with-placement dim edges nodes directed? opts anneal/anneal-placement)))

(defn layout-force-result
  "Layout an undirected graph with a light continuous force pass, then snap to the ASCII grid."
  ([dim edges nodes]
   (layout-force-result dim edges nodes {}))
  ([dim edges nodes opts]
   (let [opts (merge default-options {:compact? false} opts)
         placement (fn [normalized directed? placement-opts]
                     (force/force-placement normalized directed? placement-opts))]
     (layout-result-with-placement dim edges nodes false opts placement))))

(defn layout-force-directed-result
  "Layout with undirected force placement, but keep directed edge rendering semantics."
  ([dim edges nodes]
   (layout-force-directed-result dim edges nodes {}))
  ([dim edges nodes opts]
   (let [opts (merge default-options {:compact? false} opts)
         placement (fn [normalized directed? placement-opts]
                     (force/force-placement normalized directed? placement-opts))]
     (layout-result-with-placement dim edges nodes true opts placement))))

(defn layout-stress-result
  "Layout an undirected graph with stress majorization, aspect-ratio constrained snapping, and grid overlap removal."
  ([dim edges nodes]
   (layout-stress-result dim edges nodes {}))
  ([dim edges nodes opts]
   (let [opts (merge default-options {:compact? false} opts)
         placement (fn [normalized directed? placement-opts]
                     (stress/stress-placement normalized directed? placement-opts))]
     (layout-result-with-placement dim edges nodes false opts placement))))

(defn layout-stress-directed-result
  "Layout with stress majorization placement, but keep directed edge rendering semantics."
  ([dim edges nodes]
   (layout-stress-directed-result dim edges nodes {}))
  ([dim edges nodes opts]
   (let [opts (merge default-options {:compact? false} opts)
         placement (fn [normalized directed? placement-opts]
                     (stress/stress-placement normalized directed? placement-opts))]
     (layout-result-with-placement dim edges nodes true opts placement))))

(defn layout-sugiyama-result
  "Layout a directed graph with Sugiyama-style layering and barycentric order sweeps."
  ([dim edges nodes]
   (layout-sugiyama-result dim edges nodes {}))
  ([dim edges nodes opts]
   (let [opts (merge default-options
                     {:compact? (= :shortest-path (:routing opts))
                      :source-port :bottom}
                     opts)
         placement (fn [normalized directed? placement-opts]
                     (sugiyama/sugiyama-placement normalized directed? placement-opts))]
     (layout-result-with-placement dim edges nodes true opts placement))))

(defn layout-graph
  ([dim edges nodes directed?]
   (vals (:nodes (layout-result dim edges nodes directed? {}))))
  ([dim edges nodes directed? opts]
   (vals (:nodes (layout-result dim edges nodes directed? opts)))))

(defn layout-force-graph
  ([dim edges nodes]
   (vals (:nodes (layout-force-result dim edges nodes {}))))
  ([dim edges nodes opts]
   (vals (:nodes (layout-force-result dim edges nodes opts)))))

(defn layout-force-directed-graph
  ([dim edges nodes]
   (vals (:nodes (layout-force-directed-result dim edges nodes {}))))
  ([dim edges nodes opts]
   (vals (:nodes (layout-force-directed-result dim edges nodes opts)))))

(defn layout-stress-graph
  ([dim edges nodes]
   (vals (:nodes (layout-stress-result dim edges nodes {}))))
  ([dim edges nodes opts]
   (vals (:nodes (layout-stress-result dim edges nodes opts)))))

(defn layout-stress-directed-graph
  ([dim edges nodes]
   (vals (:nodes (layout-stress-directed-result dim edges nodes {}))))
  ([dim edges nodes opts]
   (vals (:nodes (layout-stress-directed-result dim edges nodes opts)))))

(defn layout-sugiyama-graph
  ([dim edges nodes]
   (vals (:nodes (layout-sugiyama-result dim edges nodes {}))))
  ([dim edges nodes opts]
   (vals (:nodes (layout-sugiyama-result dim edges nodes opts)))))
