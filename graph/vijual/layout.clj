(ns graph.vijual.layout
  (:require [graph.vijual.math :as m]
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

(defn incoming-edges [edges]
  (group-by second edges))

(defn depth-sort [ids edges]
  (let [incoming (incoming-edges edges)
        original-index (into {} (map-indexed (fn [index id] [id index]) ids))]
    (letfn [(depth [id visited]
              (if (visited id)
                0
                (if-let [parents (seq (incoming id))]
                  (inc (apply max (map #(depth (first %) (conj visited id))
                                       parents)))
                  0)))]
      (mapv first
            (sort-by (fn [[id]]
                       [(depth id #{}) (original-index id)])
                     (map vector ids))))))

(defn shuffle-with [^java.util.Random rng xs]
  (let [items (java.util.ArrayList. xs)]
    (java.util.Collections/shuffle items rng)
    (vec items)))

(defn placement-order [{:keys [ids edges directed?]} rng]
  (if directed?
    (depth-sort ids edges)
    (shuffle-with rng ids)))

(defn initial-placement [{:keys [ids labels seed] :as normalized}]
  (let [rng (java.util.Random. (long (or seed (System/nanoTime))))
        side (graph-side ids)
        ordered (placement-order normalized rng)]
    {:rng rng
     :nodes (into {}
                  (map-indexed (fn [index id]
                                 [id {:id id
                                      :text (labels id)
                                      :grid [(mod index side)
                                             (quot index side)]}])
                               ordered))}))

(defn tension [edges nodes directed?]
  (reduce + 0
          (map (fn [[a b]]
                 (let [[ax ay] (get-in nodes [a :grid])
                       [bx by] (get-in nodes [b :grid])
                       distance (m/manhattan [ax ay] [bx by])
                       penalty (if (and directed? (> ay by)) 2 1)]
                   (* distance penalty)))
               edges)))

(defn swap-grids [nodes a b]
  (let [agrid (get-in nodes [a :grid])
        bgrid (get-in nodes [b :grid])]
    (-> nodes
        (assoc-in [a :grid] bgrid)
        (assoc-in [b :grid] agrid))))

(defn anneal-iterations [node-count requested]
  (if requested
    requested
    (min 50000 (max 100 (* node-count node-count)))))

(defn anneal-placement [normalized directed? opts]
  (let [{:keys [rng nodes]} (initial-placement (assoc (merge normalized opts)
                                                       :directed? directed?))
        ids (:ids normalized)
        score-edges (if directed? (:edges normalized) (undirected-edges (:edges normalized)))
        iterations (anneal-iterations (count ids) (:anneal-iterations opts))]
    (loop [nodes nodes
           score (tension score-edges nodes directed?)
           n iterations]
      (if (or (zero? n) (< (count ids) 2))
        {:nodes nodes :tension score}
        (let [a (nth ids (.nextInt rng (count ids)))
              b (nth ids (.nextInt rng (count ids)))
              candidate (swap-grids nodes a b)
              next-score (tension score-edges candidate directed?)]
          (if ((if directed? < <=) next-score score)
            (recur candidate next-score (dec n))
            (recur nodes score (dec n))))))))

(defn force-iterations [node-count requested]
  (if requested
    requested
    (min 1000 (max 100 (* 20 node-count)))))

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

(defn jitter [^java.util.Random rng]
  (- (.nextDouble rng) 0.5))

(defn circle-positions [ids side rng]
  (let [n (count ids)
        center (/ (dec side) 2.0)
        radius (max 0.5 (* 0.45 (max 1 (dec side))))]
    (into {}
          (map-indexed
           (fn [index id]
             (let [angle (if (pos? n)
                           (* 2.0 Math/PI (/ index n))
                           0.0)
                   x (+ center (* radius (Math/cos angle)) (* 0.01 (jitter rng)))
                   y (+ center (* radius (Math/sin angle)) (* 0.01 (jitter rng)))]
               [id [x y]]))
           ids))))

(defn displacement-add [displacements id delta]
  (update displacements id #(m/v+ (or % [0.0 0.0]) delta)))

(defn clamp-double [lower upper value]
  (max lower (min upper value)))

(defn force-step [ids edges positions iteration iterations]
  (let [n (max 1 (count ids))
        side (double (graph-side ids))
        area (* side side)
        k (Math/sqrt (/ area n))
        temperature (* side (/ (- iterations iteration) iterations))
        displacements (zipmap ids (repeat [0.0 0.0]))
        repulsed (reduce
                  (fn [displacements [a b]]
                    (let [[ax ay] (positions a)
                          [bx by] (positions b)
                          dx (- ax bx)
                          dy (- ay by)
                          distance (max 0.01 (Math/sqrt (+ (* dx dx) (* dy dy))))
                          force (/ (* k k) distance)
                          delta [(* (/ dx distance) force)
                                 (* (/ dy distance) force)]]
                      (-> displacements
                          (displacement-add a delta)
                          (displacement-add b (m/v* -1 delta)))))
                  displacements
                  (for [left-index (range n)
                        right-index (range (inc left-index) n)]
                    [(ids left-index) (ids right-index)]))
        attracted (reduce
                   (fn [displacements [a b]]
                     (let [[ax ay] (positions a)
                           [bx by] (positions b)
                           dx (- ax bx)
                           dy (- ay by)
                           distance (max 0.01 (Math/sqrt (+ (* dx dx) (* dy dy))))
                           force (/ (* distance distance) k)
                           delta [(* (/ dx distance) force)
                                  (* (/ dy distance) force)]]
                       (-> displacements
                           (displacement-add a (m/v* -1 delta))
                           (displacement-add b delta))))
                   repulsed
                   edges)]
    (into {}
          (map (fn [id]
                 (let [[x y] (positions id)
                       [dx dy] (attracted id)
                       length (max 0.01 (Math/sqrt (+ (* dx dx) (* dy dy))))
                       step (min length temperature)
                       nx (+ x (* (/ dx length) step))
                       ny (+ y (* (/ dy length) step))]
                   [id [(clamp-double (- side) (* 2 side) nx)
                        (clamp-double (- side) (* 2 side) ny)]]))
               ids))))

(defn force-directed-positions [ids edges opts]
  (let [rng (java.util.Random. (long (or (:seed opts) 0)))
        iterations (force-iterations (count ids) (:force-iterations opts))
        force-edges (indexed-undirected-edges ids edges)]
    (loop [positions (circle-positions ids (graph-side ids) rng)
           iteration 0]
      (if (or (zero? iterations) (>= iteration iterations) (< (count ids) 2))
        positions
        (recur (force-step ids force-edges positions iteration iterations)
               (inc iteration))))))

(defn normalize-coordinate [min-value max-value side value]
  (if (= min-value max-value)
    (/ (dec side) 2.0)
    (* (dec side) (/ (- value min-value) (- max-value min-value)))))

(defn normalize-positions [positions side]
  (let [xs (map first (vals positions))
        ys (map second (vals positions))
        min-x (apply min 0 xs)
        max-x (apply max 0 xs)
        min-y (apply min 0 ys)
        max-y (apply max 0 ys)]
    (into {}
          (map (fn [[id [x y]]]
                 [id [(normalize-coordinate min-x max-x side x)
                      (normalize-coordinate min-y max-y side y)]])
               positions))))

(defn squared-distance [[ax ay] [bx by]]
  (let [dx (- ax bx)
        dy (- ay by)]
    (+ (* dx dx) (* dy dy))))

(defn grid-cells [side]
  (vec (for [y (range side)
             x (range side)]
         [x y])))

(defn snap-order [ids positions side]
  (let [center [(/ (dec side) 2.0) (/ (dec side) 2.0)]
        index-by-id (into {} (map-indexed (fn [index id] [id index]) ids))]
    (sort-by (fn [id]
               [(- (squared-distance (positions id) center))
                (index-by-id id)])
             ids)))

(defn nearest-cell [position cells]
  (first (sort-by (fn [[x y :as cell]]
                    [(squared-distance position cell) y x])
                  cells)))

(defn snap-to-grid [ids positions]
  (let [side (graph-side ids)
        normalized (normalize-positions positions side)]
    (loop [pending (snap-order ids normalized side)
           cells (grid-cells side)
           snapped {}]
      (if-let [[id & more] (seq pending)]
        (let [cell (nearest-cell (normalized id) cells)]
          (recur more
                 (vec (remove #{cell} cells))
                 (assoc snapped id cell)))
        snapped))))

(defn force-placement [normalized _directed? opts]
  (let [ids (:ids normalized)
        positions (force-directed-positions ids (:edges normalized) opts)
        grid (snap-to-grid ids positions)
        nodes (into {}
                    (map (fn [id]
                           [id {:id id
                                :text (get-in normalized [:labels id])
                                :grid (grid id)
                                :force-position (positions id)}])
                         ids))]
    {:nodes nodes
     :tension (tension (undirected-edges (:edges normalized)) nodes false)
     :force? true}))

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

(declare route-logical-points segment-records lane-segment)

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

(defn directed-edges [edges]
  (vec (remove (fn [[a b]] (= a b)) edges)))

(defn sugiyama-layer-map [ids edges]
  (let [incoming (incoming-edges (directed-edges edges))]
    (letfn [(layer-depth [id visiting]
              (if (visiting id)
                0
                (if-let [parents (seq (incoming id))]
                  (inc (apply max (map #(layer-depth (first %)
                                                       (conj visiting id))
                                       parents)))
                  0)))]
      (into {} (map (fn [id] [id (layer-depth id #{})]) ids)))))

(defn sugiyama-initial-layers [ids edges]
  (let [layer-by-id (sugiyama-layer-map ids edges)
        max-layer (apply max 0 (vals layer-by-id))
        original-index (into {} (map-indexed (fn [index id] [id index]) ids))
        grouped (group-by layer-by-id ids)]
    (mapv (fn [layer]
            (vec (sort-by original-index (grouped layer))))
          (range (inc max-layer)))))

(defn layer-order-index [layers]
  (into {}
        (mapcat (fn [layer]
                  (map-indexed (fn [index id] [id index]) layer))
                layers)))

(defn average [xs]
  (when-let [items (seq xs)]
    (/ (reduce + items) (count items))))

(defn reorder-layer [layer neighbors order-index original-index]
  (vec (sort-by (fn [id]
                  [(or (average (keep order-index (neighbors id)))
                       (original-index id))
                   (original-index id)])
                layer)))

(defn sweep-layers-down [layers incoming original-index]
  (loop [index 1
         layers layers]
    (if (>= index (count layers))
      layers
      (let [order-index (layer-order-index layers)
            layer (reorder-layer (layers index) incoming order-index original-index)]
        (recur (inc index) (assoc layers index layer))))))

(defn sweep-layers-up [layers outgoing original-index]
  (loop [index (- (count layers) 2)
         layers layers]
    (if (neg? index)
      layers
      (let [order-index (layer-order-index layers)
            layer (reorder-layer (layers index) outgoing order-index original-index)]
        (recur (dec index) (assoc layers index layer))))))

(defn sugiyama-order-iterations [requested]
  (or requested 8))

(defn sugiyama-ordered-layers [ids edges opts]
  (let [edges (directed-edges edges)
        incoming (update-vals (group-by second edges) #(mapv first %))
        outgoing (update-vals (group-by first edges) #(mapv second %))
        original-index (into {} (map-indexed (fn [index id] [id index]) ids))
        iterations (sugiyama-order-iterations (:sugiyama-iterations opts))]
    (loop [layers (sugiyama-initial-layers ids edges)
           n iterations]
      (if (zero? n)
        layers
        (recur (-> layers
                   (sweep-layers-down incoming original-index)
                   (sweep-layers-up outgoing original-index))
               (dec n))))))

(defn sugiyama-grid [layers]
  (let [width (apply max 1 (map count layers))]
    (into {}
          (mapcat
           (fn [layer-y layer]
             (let [offset (quot (- width (count layer)) 2)]
               (map-indexed (fn [index id]
                              [id [(+ offset index) layer-y]])
                            layer)))
           (range)
           layers))))

(defn sugiyama-placement [normalized _directed? opts]
  (let [ids (:ids normalized)
        layers (sugiyama-ordered-layers ids (:edges normalized) opts)
        grid (sugiyama-grid layers)
        nodes (into {}
                    (map (fn [id]
                           [id {:id id
                                :text (get-in normalized [:labels id])
                                :grid (grid id)
                                :layer (second (grid id))}])
                         ids))]
    {:nodes nodes
     :tension (tension (:edges normalized) nodes true)
     :layers layers
     :sugiyama? true}))

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
   (layout-result-with-placement dim edges nodes directed? opts anneal-placement)))

(defn layout-force-result
  "Layout an undirected graph with a light continuous force pass, then snap to the ASCII grid."
  ([dim edges nodes]
   (layout-force-result dim edges nodes {}))
  ([dim edges nodes opts]
   (let [opts (merge default-options {:compact? false} opts)
         placement (fn [normalized directed? placement-opts]
                     (force-placement normalized directed? placement-opts))]
     (layout-result-with-placement dim edges nodes false opts placement))))

(defn layout-force-directed-result
  "Layout with undirected force placement, but keep directed edge rendering semantics."
  ([dim edges nodes]
   (layout-force-directed-result dim edges nodes {}))
  ([dim edges nodes opts]
   (let [opts (merge default-options {:compact? false} opts)
         placement (fn [normalized directed? placement-opts]
                     (force-placement normalized directed? placement-opts))]
     (layout-result-with-placement dim edges nodes true opts placement))))

(defn layout-stress-result
  "Layout an undirected graph with stress majorization, aspect-ratio constrained snapping, and grid overlap removal."
  ([dim edges nodes]
   (layout-stress-result dim edges nodes {}))
  ([dim edges nodes opts]
   (let [opts (merge default-options {:compact? false} opts)
         placement (fn [normalized directed? placement-opts]
                     (stress-placement normalized directed? placement-opts))]
     (layout-result-with-placement dim edges nodes false opts placement))))

(defn layout-stress-directed-result
  "Layout with stress majorization placement, but keep directed edge rendering semantics."
  ([dim edges nodes]
   (layout-stress-directed-result dim edges nodes {}))
  ([dim edges nodes opts]
   (let [opts (merge default-options {:compact? false} opts)
         placement (fn [normalized directed? placement-opts]
                     (stress-placement normalized directed? placement-opts))]
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
                     (sugiyama-placement normalized directed? placement-opts))]
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
