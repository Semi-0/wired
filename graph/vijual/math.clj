(ns graph.vijual.math)

(def mirror-matrix [[0 1] [1 0]])

(defn half [x]
  (/ x 2))

(defn floor-int [x]
  (int (Math/floor (double x))))

(defn ceil-int [x]
  (int (Math/ceil (double x))))

(defn positions [pred coll]
  (keep-indexed (fn [index item]
                  (if (pred item)
                    index
                    nil))
                coll))

(defn v+ [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn v- [[ax ay] [bx by]]
  [(- ax bx) (- ay by)])

(defn v* [k [x y]]
  [(* k x) (* k y)])

(defn manhattan [[ax ay] [bx by]]
  (+ (abs (- ax bx)) (abs (- ay by))))

(defn mat-v [[[aa ab] [ba bb]] [x y]]
  [(+ (* aa x) (* ab y))
   (+ (* ba x) (* bb y))])

(defn mirror-point [point]
  (mat-v mirror-matrix point))

(defn direction [[ax ay] [bx by]]
  (cond
    (< ax bx) :right
    (> ax bx) :left
    (< ay by) :down
    (> ay by) :up
    :else nil))

(defn horizontal? [dir]
  (contains? #{:left :right} dir))

(defn vertical? [dir]
  (contains? #{:up :down} dir))

(defn orthogonal-segment? [{:keys [from to]}]
  (let [[x1 y1] from
        [x2 y2] to]
    (or (= x1 x2) (= y1 y2))))

(defn rect-overlap?
  [{[ax ay] :p [aw ah] :size}
   {[bx by] :p [bw bh] :size}]
  (not (or (<= (+ ax aw) bx)
           (<= (+ bx bw) ax)
           (<= (+ ay ah) by)
           (<= (+ by bh) ay))))

(defn prefix-sums [xs]
  (vec (cons 0 (reductions + xs))))

(defn segment-pairs [points]
  (map (fn [from to]
         {:from from :to to})
       points
       (next points)))
