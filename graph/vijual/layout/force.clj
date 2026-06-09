(ns graph.vijual.layout.force
  (:require [graph.vijual.math :as m]))

(defn layout-fn [name]
  (deref (requiring-resolve (symbol "graph.vijual.layout" (str name)))))

(defn graph-side [& args]
  (apply (layout-fn 'graph-side) args))

(defn undirected-edges [& args]
  (apply (layout-fn 'undirected-edges) args))

(defn tension [& args]
  (apply (layout-fn 'tension) args))

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
