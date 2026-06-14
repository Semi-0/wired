(ns graph.vijual.layout.sugiyama)

(defn layout-fn [name]
  (deref (requiring-resolve (symbol "graph.vijual.layout" (str name)))))

(defn tension [& args]
  (apply (layout-fn 'tension) args))

(defn incoming-edges [edges]
  (group-by second edges))

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
