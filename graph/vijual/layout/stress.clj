(ns graph.vijual.layout.stress
  (:require [graph.vijual.math :as m]))

(defn layout-fn [name]
  (deref (requiring-resolve (symbol "graph.vijual.layout" (str name)))))

(defn graph-side [& args]
  (apply (layout-fn 'graph-side) args))

(defn undirected-edges [& args]
  (apply (layout-fn 'undirected-edges) args))

(defn tension [& args]
  (apply (layout-fn 'tension) args))

(defn route-logical-points [& args]
  (apply (layout-fn 'route-logical-points) args))

(defn segment-records [& args]
  (apply (layout-fn 'segment-records) args))

(defn lane-segment [& args]
  (apply (layout-fn 'lane-segment) args))

(defn ceil-div [n divisor]
  (quot (+ n divisor -1) divisor))

(defn stress-aspect-ratio [opts]
  (max 0.25 (double (or (:stress-aspect-ratio opts) 1.15))))

(defn stress-node-spacing [opts]
  (max 0.1 (double (or (:stress-node-spacing opts)
                       (:stress-edge-length opts)
                       1.0))))

(defn stress-grid-scale
  "Grid expansion uses spacing only when >= 1.0 so tight edge lengths do not shrink the canvas."
  [opts]
  (max 1.0 (stress-node-spacing opts)))

(defn aspect-score [node-count target-aspect [width height]]
  (let [ratio (/ (double width) (double height))
        empty-cells (- (* width height) node-count)]
    [(Math/abs (Math/log (/ ratio target-aspect)))
     empty-cells
     width]))

(defn expand-grid-size [[width height] spacing]
  [(max 1 (m/ceil-int (* width spacing)))
   (max 1 (m/ceil-int (* height spacing)))])

(defn stress-grid-size [node-count opts]
  (let [node-count (max 1 node-count)
        requested-width (:stress-grid-width opts)
        requested-height (:stress-grid-height opts)
        spacing (stress-grid-scale opts)]
    (cond
      (and requested-width requested-height)
      (let [width (max 1 requested-width)
            height (max 1 requested-height)]
        (if (>= (* width height) node-count)
          [width height]
          [width (ceil-div node-count width)]))

      requested-width
      (let [width (max 1 requested-width)]
        [width (ceil-div node-count width)])

      requested-height
      (let [height (max 1 requested-height)]
        [(ceil-div node-count height) height])

      :else
      (let [target (stress-aspect-ratio opts)]
        (expand-grid-size
         (first
          (sort-by (partial aspect-score node-count target)
                   (for [width (range 1 (inc node-count))
                         :let [height (ceil-div node-count width)]]
                     [width height])))
         spacing)))))

(defn jitter [^java.util.Random rng]
  (- (.nextDouble rng) 0.5))

(defn ellipse-positions [ids [width height] rng]
  (let [n (count ids)
        center-x (/ (dec width) 2.0)
        center-y (/ (dec height) 2.0)
        radius-x (max 0.5 (* 0.45 (max 1 (dec width))))
        radius-y (max 0.5 (* 0.45 (max 1 (dec height))))]
    (into {}
          (map-indexed
           (fn [index id]
             (let [angle (if (pos? n)
                           (* 2.0 Math/PI (/ index n))
                           0.0)
                   x (+ center-x (* radius-x (Math/cos angle)) (* 0.01 (jitter rng)))
                   y (+ center-y (* radius-y (Math/sin angle)) (* 0.01 (jitter rng)))]
               [id [x y]]))
           ids))))

(defn undirected-adjacency [ids edges]
  (let [base (zipmap ids (repeat #{}))]
    (reduce (fn [adj [a b]]
              (if (= a b)
                adj
                (-> adj
                    (update a (fnil conj #{}) b)
                    (update b (fnil conj #{}) a))))
            base
            edges)))

(defn graph-distances-from [adj start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         distances {start 0}]
    (if (empty? queue)
      distances
      (let [id (peek queue)
            queue (pop queue)
            next-distance (inc (distances id))
            neighbors (remove distances (adj id))]
        (recur (reduce conj queue neighbors)
               (reduce (fn [distances neighbor]
                         (assoc distances neighbor next-distance))
                       distances
                       neighbors))))))

(defn graph-distance-maps [ids edges]
  (let [adj (undirected-adjacency ids edges)]
    (into {} (map (fn [id] [id (graph-distances-from adj id)]) ids))))

(defn finite-graph-distances [ids distance-maps]
  (for [left-index (range (count ids))
        right-index (range (inc left-index) (count ids))
        :let [distance (get-in distance-maps [(ids left-index) (ids right-index)])]
        :when distance]
    distance))

(defn stress-pairs [ids edges opts]
  (let [distance-maps (graph-distance-maps ids edges)
        finite-distances (seq (finite-graph-distances ids distance-maps))
        diameter (if finite-distances
                   (apply max 1 finite-distances)
                   1)
        disconnected-distance (+ diameter 2)
        edge-length (stress-node-spacing opts)
        disconnected-weight (double (or (:stress-disconnected-weight opts) 0.02))]
    (vec
     (for [left-index (range (count ids))
           right-index (range (inc left-index) (count ids))
           :let [a (ids left-index)
                 b (ids right-index)
                 graph-distance (get-in distance-maps [a b])
                 desired-distance (* edge-length
                                     (double (or graph-distance
                                                 disconnected-distance)))
                 weight (if graph-distance
                          (/ 1.0 (* graph-distance graph-distance))
                          disconnected-weight)]]
       {:a a
        :b b
        :distance desired-distance
        :weight weight}))))

(defn stress-iterations [node-count requested]
  (if requested
    requested
    (min 250 (max 40 (* 6 node-count)))))

(defn stress-accumulate [acc id point weight]
  (-> acc
      (update-in [id :sum] #(m/v+ (or % [0.0 0.0])
                                  (m/v* weight point)))
      (update-in [id :weight] (fnil + 0.0) weight)))

(defn stress-majorization-step [ids pairs positions]
  (let [acc (reduce
             (fn [acc {:keys [a b distance weight]}]
               (let [[ax ay] (positions a)
                     [bx by] (positions b)
                     dx (- ax bx)
                     dy (- ay by)
                     length (max 1.0e-6
                                 (Math/sqrt (+ (* dx dx) (* dy dy))))
                     ux (/ dx length)
                     uy (/ dy length)
                     target-a [(+ bx (* distance ux))
                               (+ by (* distance uy))]
                     target-b [(- ax (* distance ux))
                               (- ay (* distance uy))]]
                 (-> acc
                     (stress-accumulate a target-a weight)
                     (stress-accumulate b target-b weight))))
             {}
             pairs)]
    (into {}
          (map (fn [id]
                 (let [{[sx sy] :sum weight :weight} (acc id)]
                   [id (if (pos? (or weight 0.0))
                         [(/ sx weight) (/ sy weight)]
                         (positions id))]))
               ids))))

(defn normalize-coordinate-to-size [min-value max-value size value]
  (if (= min-value max-value)
    (/ (dec size) 2.0)
    (* (dec size) (/ (- value min-value) (- max-value min-value)))))

(defn normalize-positions-to-rect [positions [width height]]
  (let [xs (map first (vals positions))
        ys (map second (vals positions))
        min-x (apply min 0 xs)
        max-x (apply max 0 xs)
        min-y (apply min 0 ys)
        max-y (apply max 0 ys)]
    (into {}
          (map (fn [[id [x y]]]
                 [id [(normalize-coordinate-to-size min-x max-x width x)
                      (normalize-coordinate-to-size min-y max-y height y)]])
               positions))))

(defn stress-majorization-positions [ids edges opts]
  (let [rng (java.util.Random. (long (or (:seed opts) 0)))
        grid-size (stress-grid-size (count ids) opts)
        pairs (stress-pairs ids edges opts)
        iterations (stress-iterations (count ids) (:stress-iterations opts))]
    (loop [positions (normalize-positions-to-rect
                      (ellipse-positions ids grid-size rng)
                      grid-size)
           iteration 0]
      (if (or (zero? iterations) (>= iteration iterations) (< (count ids) 2))
        positions
        (recur (normalize-positions-to-rect
                (stress-majorization-step ids pairs positions)
                grid-size)
               (inc iteration))))))

(defn rect-grid-cells [[width height]]
  (vec (for [y (range height)
             x (range width)]
         [x y])))

(defn squared-distance [[ax ay] [bx by]]
  (let [dx (- ax bx)
        dy (- ay by)]
    (+ (* dx dx) (* dy dy))))

(defn nearest-cell [position cells]
  (first (sort-by (fn [[x y :as cell]]
                    [(squared-distance position cell) y x])
                  cells)))

(defn snap-order-rect [ids positions [width height]]
  (let [center [(/ (dec width) 2.0) (/ (dec height) 2.0)]
        index-by-id (into {} (map-indexed (fn [index id] [id index]) ids))]
    (sort-by (fn [id]
               [(- (squared-distance (positions id) center))
                (index-by-id id)])
             ids)))

(defn snap-to-rect-grid [ids positions grid-size]
  (let [normalized (normalize-positions-to-rect positions grid-size)]
    (loop [pending (snap-order-rect ids normalized grid-size)
           cells (rect-grid-cells grid-size)
           snapped {}]
      (if-let [[id & more] (seq pending)]
        (let [cell (nearest-cell (normalized id) cells)]
          (recur more
                 (vec (remove #{cell} cells))
                 (assoc snapped id cell)))
        snapped))))

(def default-stress-energy-weights
  {:graph-distance-error 1.0
   :node-overlap-penalty 1000.0
   :edge-length-variance-penalty 0.5
   :aspect-ratio-penalty 20.0
   :center-balance-penalty 10.0
   :crossing-penalty 8.0
   :wiring-overlap-balance-penalty 3.0})

(defn stress-energy-weights [opts]
  (merge default-stress-energy-weights (:stress-energy-weights opts)))

(defn euclidean-distance [[ax ay] [bx by]]
  (let [dx (- ax bx)
        dy (- ay by)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn mean [xs]
  (when-let [items (seq xs)]
    (/ (reduce + items) (double (count items)))))

(defn variance [xs]
  (if-let [avg (mean xs)]
    (mean (map (fn [x]
                 (let [delta (- x avg)]
                   (* delta delta)))
               xs))
    0.0))

(defn graph-distance-error [pairs grid]
  (reduce + 0.0
          (map (fn [{:keys [a b distance weight]}]
                 (let [actual (euclidean-distance (grid a) (grid b))
                       delta (- actual distance)]
                   (* weight delta delta)))
               pairs)))

(defn node-overlap-penalty [ids grid]
  (reduce + 0.0
          (map (fn [count]
                 (let [extra (max 0 (dec count))]
                   (* extra extra)))
               (vals (frequencies (map grid ids))))))

(defn indexed-undirected-edges [ids edges]
  (let [index-by-id (into {} (map-indexed (fn [index id] [id index]) ids))]
    (vec (distinct
          (keep (fn [[a b]]
                  (when (and (not= a b)
                             (contains? index-by-id a)
                             (contains? index-by-id b))
                    (if (<= (index-by-id a) (index-by-id b))
                      [a b]
                      [b a])))
                edges)))))

(defn edge-length-variance-penalty [ids edges grid]
  (variance (map (fn [[a b]]
                   (euclidean-distance (grid a) (grid b)))
                 (indexed-undirected-edges ids edges))))

(defn occupied-grid-bounds [ids grid]
  (let [points (map grid ids)
        xs (map first points)
        ys (map second points)
        min-x (apply min 0 xs)
        max-x (apply max 0 xs)
        min-y (apply min 0 ys)
        max-y (apply max 0 ys)]
    {:min-x min-x
     :max-x max-x
     :min-y min-y
     :max-y max-y
     :width (inc (- max-x min-x))
     :height (inc (- max-y min-y))}))

(defn aspect-ratio-penalty [ids grid opts]
  (let [{:keys [width height]} (occupied-grid-bounds ids grid)
        ratio (/ (double width) (double height))
        target (stress-aspect-ratio opts)]
    (let [delta (Math/log (/ ratio target))]
      (* delta delta))))

(defn center-balance-penalty [ids grid [width height]]
  (if-let [points (seq (map grid ids))]
    (let [avg-x (mean (map first points))
          avg-y (mean (map second points))
          target-x (/ (dec width) 2.0)
          target-y (/ (dec height) 2.0)
          dx (/ (- avg-x target-x) (max 1.0 width))
          dy (/ (- avg-y target-y) (max 1.0 height))]
      (+ (* dx dx) (* dy dy)))
    0.0))

(defn cross-product [[ax ay] [bx by] [cx cy]]
  (- (* (- bx ax) (- cy ay))
     (* (- by ay) (- cx ax))))

(defn proper-segment-cross? [a b c d]
  (let [ab-c (cross-product a b c)
        ab-d (cross-product a b d)
        cd-a (cross-product c d a)
        cd-b (cross-product c d b)]
    (and (not (zero? ab-c))
         (not (zero? ab-d))
         (not (zero? cd-a))
         (not (zero? cd-b))
         (neg? (* ab-c ab-d))
         (neg? (* cd-a cd-b)))))

(defn crossing-penalty [ids edges grid]
  (let [edges (indexed-undirected-edges ids edges)]
    (count
     (for [left-index (range (count edges))
           right-index (range (inc left-index) (count edges))
           :let [[a b] (edges left-index)
                 [c d] (edges right-index)]
           :when (and (not-any? #{a b} [c d])
                      (proper-segment-cross? (grid a) (grid b)
                                             (grid c) (grid d)))]
      [left-index right-index]))))

(defn stress-logical-routes [edges grid]
  (map-indexed (fn [index [a b]]
                 {:id index
                  :source a
                  :dest b
                  :logical-points (route-logical-points (grid a) (grid b))})
               (distinct edges)))

(defn wiring-lane-loads [edges grid]
  (frequencies
   (map (juxt :axis :lane)
        (keep lane-segment
              (segment-records (stress-logical-routes edges grid))))))

(defn wiring-overlap-balance-penalty [_ids edges grid]
  (reduce + 0.0
          (map (fn [load]
                 (let [extra (max 0 (dec load))]
                   (* extra extra)))
               (vals (wiring-lane-loads edges grid)))))

(defn weighted-stress-energy-total [components opts]
  (let [weights (stress-energy-weights opts)]
    (reduce-kv (fn [total component value]
                 (+ total (* (double (get weights component 1.0))
                             (double value))))
               0.0
               components)))

(defn stress-energy [ids edges grid-size grid opts]
  (let [pairs (stress-pairs ids edges opts)
        components {:graph-distance-error (graph-distance-error pairs grid)
                    :node-overlap-penalty (node-overlap-penalty ids grid)
                    :edge-length-variance-penalty
                    (edge-length-variance-penalty ids edges grid)
                    :aspect-ratio-penalty (aspect-ratio-penalty ids grid opts)
                    :center-balance-penalty
                    (center-balance-penalty ids grid grid-size)
                    :crossing-penalty (crossing-penalty ids edges grid)
                    :wiring-overlap-balance-penalty
                    (wiring-overlap-balance-penalty ids edges grid)}]
    (assoc components :total (weighted-stress-energy-total components opts))))

(defn stress-refine-iterations [node-count requested]
  (if requested
    requested
    (min 2000 (max 100 (* 20 node-count)))))

(defn swap-grid-cells [grid a b]
  (let [a-cell (grid a)
        b-cell (grid b)]
    (assoc grid a b-cell b a-cell)))

(defn move-grid-cell [grid id cell]
  (assoc grid id cell))

(defn empty-grid-cells [grid-size grid]
  (let [occupied (set (vals grid))]
    (vec (remove occupied (rect-grid-cells grid-size)))))

(defn stress-candidate-grid [ids grid grid-size ^java.util.Random rng]
  (let [empty-cells (empty-grid-cells grid-size grid)]
    (if (and (seq empty-cells)
             (< (.nextDouble rng) 0.5))
      (move-grid-cell grid
                      (nth ids (.nextInt rng (count ids)))
                      (nth empty-cells (.nextInt rng (count empty-cells))))
      (if (< (count ids) 2)
        grid
        (let [a (nth ids (.nextInt rng (count ids)))
              b (nth ids (.nextInt rng (count ids)))]
          (if (= a b)
            grid
            (swap-grid-cells grid a b)))))))

(defn refine-stress-grid [ids edges grid-size grid opts]
  (let [rng (java.util.Random. (long (+ 7919 (or (:seed opts) 0))))
        iterations (stress-refine-iterations (count ids)
                                             (:stress-refine-iterations opts))]
    (loop [grid grid
           energy (stress-energy ids edges grid-size grid opts)
           iteration 0]
      (if (or (zero? iterations) (>= iteration iterations) (< (count ids) 2))
        {:grid grid :energy energy}
        (let [candidate (stress-candidate-grid ids grid grid-size rng)
              candidate-energy (stress-energy ids edges grid-size candidate opts)]
          (if (<= (:total candidate-energy) (:total energy))
            (recur candidate candidate-energy (inc iteration))
            (recur grid energy (inc iteration))))))))

(defn stress-placement [normalized _directed? opts]
  (let [ids (:ids normalized)
        grid-size (stress-grid-size (count ids) opts)
        positions (stress-majorization-positions ids (:edges normalized) opts)
        snapped-grid (snap-to-rect-grid ids positions grid-size)
        {:keys [grid energy]} (refine-stress-grid ids (:edges normalized)
                                                  grid-size snapped-grid opts)
        nodes (into {}
                    (map (fn [id]
                           [id {:id id
                                :text (get-in normalized [:labels id])
                                :grid (grid id)
                                :stress-position (positions id)}])
                         ids))]
    {:nodes nodes
     :tension (tension (undirected-edges (:edges normalized)) nodes false)
     :stress? true
     :stress-grid-size grid-size
     :stress-energy energy}))
