(ns graph.vijual.render
  (:require [graph.vijual.math :as m])
  (:import (java.awt Color)
           (java.awt.image BufferedImage)))

(def ascii-wrap-threshold 10)
(def image-wrap-threshold 20)

(defn fill
  ([c n]
   (apply str (repeat (max 0 n) c)))
  ([c]
   (fill c 1)))

(defn wrap [text width]
  (let [text (str text)]
    (lazy-seq
     (if (<= (count text) width)
       [text]
       (let [spaces (m/positions #(= \space %) (take width text))]
         (if (seq spaces)
           (cons (apply str (take (last spaces) text))
                 (wrap (apply str (drop (inc (last spaces)) text)) width))
           (cons (apply str (take width text))
                 (wrap (apply str (drop width text)) width))))))))

(defn center-lines [lines width height]
  (let [lines (vec lines)
        pad-top (quot (max 0 (- height (count lines))) 2)]
    (vec (concat (repeat pad-top "")
                 (map (fn [line]
                        (str (fill \space (quot (max 0 (- width (count line))) 2))
                             line))
                      lines)))))

(def ascii-dim
  {:width-fn (fn [text]
               (+ (min ascii-wrap-threshold (count (str text))) 4))
   :height-fn (fn [text]
                (+ (count (wrap text ascii-wrap-threshold)) 2))
   :wrap-fn (fn
              ([text]
               (vec (map #(str " " %) (wrap text ascii-wrap-threshold))))
              ([text width height]
               (vec (map #(str " " %)
                         (center-lines (wrap text (- width 4))
                                       (- width 4)
                                       (- height 3))))))
   :node-padding 1
   :row-padding 8
   :line-wid 1
   :line-padding 1
   :min-edge-gap 3
   :horizontal-line-char \·
   :vertical-line-char \|})

(def image-dim
  {:width-fn (fn [text]
               (* (+ (min image-wrap-threshold (count (str text))) 4) 7))
   :height-fn (fn [text]
                (* (+ (count (wrap text image-wrap-threshold)) 2) 9))
   :wrap-fn (fn
              ([text]
               (vec (map #(str " " %) (wrap text image-wrap-threshold))))
              ([text width height]
               (vec (map #(str " " %)
                         (center-lines (wrap text (quot (- width 4) 7))
                                       (quot (- width 4) 7)
                                       (quot (- height 3) 9))))))
   :node-padding 10
   :row-padding 80
   :line-wid 3
   :line-padding 10})

(defn integer-shapes [shapes]
  (mapv (fn [{:keys [x y width height] :as shape}]
          (let [ix (m/floor-int x)
                iy (m/floor-int y)
                rx (m/floor-int (+ x width))
                by (m/floor-int (+ y height))]
            (assoc shape
                   :x ix
                   :y iy
                   :width (max 1 (- rx ix))
                   :height (max 1 (- by iy)))))
        shapes))

(defn shapes-width [shapes]
  (apply max 1 (map (fn [{:keys [x width]}] (+ x width)) shapes)))

(defn shapes-height [shapes]
  (apply max 1 (map (fn [{:keys [y height]}] (+ y height)) shapes)))

(defn rect-relation [ypos {:keys [y height]}]
  (let [bottom (dec (+ y height))]
    (cond
      (or (= ypos y) (= ypos bottom)) :on
      (and (> ypos y) (< ypos bottom)) :in
      :else nil)))

(defn line-fill-char
  [{:keys [horizontal-line-char vertical-line-char]} dir]
  (if (m/vertical? dir)
    (or vertical-line-char \|)
    (or horizontal-line-char \-)))

(defn with-line-fill [dim {:keys [dir] :as shape}]
  (assoc shape :line-char (line-fill-char dim dir)))

(defn segment-shape [line-wid from to]
  (let [[x1 y1] from
        [x2 y2] to]
    (if (= y1 y2)
      {:type :line :x (min x1 x2) :y y1
       :width (+ (abs (- x2 x1)) line-wid)
       :height line-wid
       :dir :right}
      {:type :line :x x1 :y (min y1 y2)
       :width line-wid
       :height (+ (abs (- y2 y1)) line-wid)
       :dir :down})))

(defn arrow-shape [line-wid point dir arrow?]
  {:type (if arrow? :arrow :cap)
   :x (first point)
   :y (second point)
   :width line-wid
   :height line-wid
   :on-top true
   :dir dir})

(def reverse-dir
  {:right :left
   :left :right
   :down :up
   :up :down})

(defn segment-distance [from to]
  (m/manhattan from to))

(defn point-at-distance [[ax ay :as from] [bx by :as to] distance]
  (let [dir (m/direction from to)
        distance (max 0 (min (segment-distance from to) distance))]
    (case dir
      :right [(+ ax distance) ay]
      :left [(- ax distance) ay]
      :down [ax (+ ay distance)]
      :up [ax (- ay distance)]
      from)))

(defn middle-arrow-shape [line-wid points reversed?]
  (let [segments (vec (partition 2 1 points))
        total (reduce + (map (fn [[from to]]
                               (segment-distance from to))
                             segments))
        target (/ total 2)]
    (when (pos? total)
      (loop [segments segments
             distance 0]
        (let [[[from to] & more] segments
              length (segment-distance from to)
              next-distance (+ distance length)]
          (if (or (empty? more) (<= target next-distance))
            (let [dir (m/direction from to)]
              (arrow-shape line-wid
                           (point-at-distance from to (- target distance))
                           (if reversed? (reverse-dir dir) dir)
                           true))
            (recur more next-distance)))))))

(defn link-shapes
  [{:keys [line-wid] :as dim}
   {:keys [arrow legs start-arrow-dir end-arrow-dir middle-arrow-reversed?]}]
  (let [points (mapv (fn [{:keys [xpos ypos]}] [xpos ypos]) legs)
        dirs (mapv :dir legs)]
    (if (< (count points) 2)
      []
      (let [middle-arrow (when (= arrow :middle)
                           (middle-arrow-shape line-wid points middle-arrow-reversed?))]
        (concat [(with-line-fill
                   dim
                   (arrow-shape line-wid (first points)
                                (or start-arrow-dir (reverse-dir (first dirs)))
                                (= arrow :start)))]
                [(with-line-fill
                   dim
                   (arrow-shape line-wid (last points)
                                (or end-arrow-dir (nth dirs (- (count dirs) 2) nil))
                                (= arrow :end)))]
                (when middle-arrow [middle-arrow])
                (map (fn [[from to]]
                       (with-line-fill
                         dim
                         (assoc (segment-shape line-wid from to) :on-top true)))
                     (partition 2 1 points)))))))

(defn graph-to-shapes [{:keys [wrap-fn] :as dim} nodes]
  (mapcat (fn [{:keys [text xpos ypos width height links]}]
            (cons {:type :rect
                   :text (wrap-fn text width height)
                   :x xpos :y ypos
                   :width width :height height}
                  (mapcat (partial link-shapes dim) links)))
          nodes))

(defn shape-text [{:keys [type y width height text dir line-char]} ypos]
  (let [line-offset (- ypos y)]
    (cond
      (= type :rect)
      (if (or (zero? line-offset) (= line-offset (dec height)))
        (str "+" (fill \- (- width 2)) "+")
        (let [body (get text (dec line-offset) "")]
          (str "|" body (fill \space (- width (count body) 2)) "|")))

      (= type :arrow)
      (case dir
        :right ">"
        :left "<"
        :up "^"
        :down "v"
        "*")

      (or (= type :cap) (= type :line))
      (fill (or line-char (if (m/vertical? dir) \| \-)) width)

      :else
      (fill (or line-char (if (m/vertical? dir) \| \-)) width))))

(defn shape-priority [{:keys [type]}]
  (case type
    :line 0
    :cap 1
    :arrow 2
    :rect 3
    0))

(defn line-char? [ch]
  (contains? #{\- \| \+ \·} ch))

(defn merge-line-char [current incoming]
  (cond
    (= current \space) incoming
    (= current incoming) incoming
    (and (line-char? current) (line-char? incoming)) \+
    :else incoming))

(defn write-char! [chars priorities pos priority ch]
  (let [current-priority (aget priorities pos)
        current (aget chars pos)]
    (cond
      (and (line-char? current)
           (line-char? ch)
           (<= priority current-priority))
      (aset-char chars pos (merge-line-char current ch))

      (>= priority current-priority)
      (do
        (aset-char chars pos
                   (if (and (line-char? current) (line-char? ch))
                     (merge-line-char current ch)
                     ch))
        (aset-int priorities pos priority)))))

(defn row-string [width ypos shapes]
  (let [chars (char-array (repeat width \space))
        priorities (int-array (repeat width -1))]
    (doseq [{:keys [x] :as shape} (sort-by shape-priority
                                            (filter #(rect-relation ypos %) shapes))]
      (let [text (shape-text shape ypos)
            priority (shape-priority shape)]
        (doseq [[index ch] (map-indexed vector text)]
          (let [pos (+ x index)]
            (when (< -1 pos width)
              (write-char! chars priorities pos priority ch))))))
    (apply str chars)))

(defn draw-shapes [_ shapes]
  (let [shapes (vec (sort-by shape-priority shapes))
        width (shapes-width shapes)
        height (shapes-height shapes)]
    (doseq [ypos (range height)]
      (println (row-string width ypos shapes)))))

(defn draw-shapes-string [dim shapes]
  (with-out-str (draw-shapes dim shapes)))

(def arrow-size 5)

(defn draw-shapes-image [{:keys [line-wid]} shapes]
  (let [shapes (vec shapes)
        img (BufferedImage. (shapes-width shapes)
                            (shapes-height shapes)
                            BufferedImage/TYPE_4BYTE_ABGR)
        graphics (.createGraphics img)]
    (doseq [{:keys [type x y width height text]} shapes]
      (if (or (= type :rect) (= type :line))
        (do (.setColor graphics (Color. 0 0 0))
            (.fillRect graphics x y width height)
            (if (= type :rect)
              (do (.setColor graphics (Color. 240 240 240))
                  (.fillRect graphics (+ x line-wid) (+ y line-wid)
                             (max 0 (- width (* 2 line-wid)))
                             (max 0 (- height (* 2 line-wid))))
                  (.setColor graphics (Color. 0 0 0))
                  (doseq [[line n] (map vector text (range))]
                    (.drawString graphics line (+ x 2) (+ y 9 (* n 10)))))
              nil))
        nil))
    (doseq [{:keys [type x y width height dir]} shapes]
      (if (= type :arrow)
        (do (.setColor graphics (Color. 0 0 0))
            (case dir
              :left (.fillPolygon graphics
                                  (int-array [x (+ x arrow-size) (+ x arrow-size)])
                                  (int-array [(+ y (m/half height))
                                              (- (+ y (m/half height)) arrow-size)
                                              (+ (+ y (m/half height)) arrow-size)])
                                  3)
              :right (.fillPolygon graphics
                                   (int-array [(+ x width)
                                               (- (+ x width) arrow-size)
                                               (- (+ x width) arrow-size)])
                                   (int-array [(+ y (m/half height))
                                               (- (+ y (m/half height)) arrow-size)
                                               (+ (+ y (m/half height)) arrow-size)])
                                   3)
              :up (.fillPolygon graphics
                                (int-array [(+ x (m/half width))
                                            (- (+ x (m/half width)) arrow-size)
                                            (+ (+ x (m/half width)) arrow-size)])
                                (int-array [y (+ y arrow-size) (+ y arrow-size)])
                                3)
              :down (.fillPolygon graphics
                                  (int-array [(+ x (m/half width))
                                              (- (+ x (m/half width)) arrow-size)
                                              (+ (+ x (m/half width)) arrow-size)])
                                  (int-array [(+ y height)
                                              (- (+ y height) arrow-size)
                                              (- (+ y height) arrow-size)])
                                  3)
              nil))
        nil))
    img))
