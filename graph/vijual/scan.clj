(ns graph.vijual.scan)

(defn scan-add
  "Add a height reservation covering [x, x + width)."
  [scan x y width]
  (let [xend (+ x width)]
    (letfn [(add [bars current-y]
              (if-let [[[bar-x bar-y] & more] (seq bars)]
                (if (<= bar-x xend)
                  (add more bar-y)
                  (cons [xend current-y] bars))
                (list [xend current-y])))
            (advance [bars current-y]
              (if-let [[[bar-x bar-y :as bar] & more] (seq bars)]
                (if (> x bar-x)
                  (cons bar (advance more bar-y))
                  (cons [x y] (add bars current-y)))
                (list [x y] [xend current-y])))]
      (vec (advance scan nil)))))

(defn scan-lowest-y
  "Return the lowest free y for a rectangle covering [x, x + width)."
  [scan x width]
  (loop [bars scan
         current-y nil
         best-y nil]
    (if-let [[[bar-x bar-y] & more] (seq bars)]
      (cond
        (<= (+ x width) bar-x)
        (if best-y
          (max best-y current-y)
          current-y)

        (< x bar-x)
        (recur more
               bar-y
               (if best-y
                 (max best-y bar-y current-y)
                 (if current-y
                   (max bar-y current-y)
                   bar-y)))

        :else
        (recur more bar-y best-y))
      (if best-y
        (max best-y current-y)
        current-y))))

(defn pack-rects
  "Pack rects upward, preserving x/width and returning {:rects ... :scan ...}."
  [padding rects]
  (loop [pending rects
         placed []
         scan [[0 0]]]
    (if-let [[{:keys [p size] :as rect} & more] (seq pending)]
      (let [[x _] p
            [width height] size
            y (or (scan-lowest-y scan x width) 0)
            next-rect (assoc rect :p [x y])
            next-scan (scan-add scan x (+ y height padding) width)]
        (recur more (conj placed next-rect) next-scan))
      {:rects placed :scan scan})))
