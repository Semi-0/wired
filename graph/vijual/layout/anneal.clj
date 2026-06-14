(ns graph.vijual.layout.anneal)

(defn layout-fn [name]
  (deref (requiring-resolve (symbol "graph.vijual.layout" (str name)))))

(defn graph-side [& args]
  (apply (layout-fn 'graph-side) args))

(defn undirected-edges [& args]
  (apply (layout-fn 'undirected-edges) args))

(defn tension [& args]
  (apply (layout-fn 'tension) args))

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
