(ns graph.vijual-clojure-lang-demo
  "Draw clojure.lang type hierarchy with vijual directed layout/routing combinations."
  (:require [clojure.reflect :as reflect]
            [clojure.string :as str]
            [graph.vijual :as v])
  (:import (java.util Locale)))

(defn clojure-lang-name? [sym]
  (str/starts-with? (str sym) "clojure.lang."))

(defn simple-name [sym]
  (symbol (let [s (str sym)]
            (if (str/includes? s ".")
              (subs s (inc (str/last-index-of s ".")))
              s))))

(defn ancestors-of [klass]
  (let [{:keys [bases interfaces]} (reflect/type-reflect klass :ancestors true)]
    (distinct (concat bases interfaces))))

(defn build-graph [seeds]
  (let [visited (atom {})
        edges (atom #{})]
    (letfn [(walk [klass]
              (when-not (@visited klass)
                (swap! visited assoc klass true)
                (doseq [parent (ancestors-of klass)
                        :when (clojure-lang-name? parent)]
                  (swap! edges conj [klass parent])
                  (walk parent))))]
      (doseq [s seeds] (walk s)))
    {:ids (vec (keys @visited))
     :edges (vec @edges)}))

(def seeds
  '[clojure.lang.RT
    clojure.lang.AFunction
    clojure.lang.AFn
    clojure.lang.IPersistentCollection
    clojure.lang.IPersistentVector
    clojure.lang.IPersistentMap
    clojure.lang.IPersistentSet
    clojure.lang.IPersistentList
    clojure.lang.ISeq
    clojure.lang.IFn
    clojure.lang.IRef
    clojure.lang.IType
    clojure.lang.IObj
    clojure.lang.IPending
    clojure.lang.LazySeq
    clojure.lang.PersistentVector
    clojure.lang.PersistentArrayMap
    clojure.lang.PersistentHashMap
    clojure.lang.PersistentHashSet
    clojure.lang.PersistentList
    clojure.lang.Var
    clojure.lang.Symbol
    clojure.lang.Keyword
    clojure.lang.Namespace
    clojure.lang.Ref
    clojure.lang.Atom
    clojure.lang.Agent
    clojure.lang.MultiFn
    clojure.lang.Compiler
    clojure.lang.Numbers
    clojure.lang.BigInt
    clojure.lang.Ratio
    clojure.lang.MapEntry
    clojure.lang.ChunkedCons
    clojure.lang.Reduced])

(def placement-strategies
  [{:title "Current directed layout"
    :layout (fn [edges nodes opts]
              (v/layout-result v/ascii-dim edges nodes true opts))}
   {:title "Force-directed snapped layout"
    :layout (fn [edges nodes opts]
              (v/layout-force-directed-result v/ascii-dim edges nodes opts))}
   {:title "Stress-majorized balanced layout"
    :layout (fn [edges nodes opts]
              (v/layout-stress-directed-result v/ascii-dim edges nodes opts))}
   {:title "Sugiyama layered layout"
    :layout (fn [edges nodes opts]
              (v/layout-sugiyama-result v/ascii-dim edges nodes opts))}])

(def routing-strategies
  [{:title "Current orthogonal routing"
    :opts {}}
   {:title "Shortest-path routing"
    :opts {:routing :shortest-path}}])

(def demo-opts
  {:force-iterations 100
   :stress-iterations 120
   :sugiyama-iterations 8})

(def stress-energy-components
  [:graph-distance-error
   :node-overlap-penalty
   :edge-length-variance-penalty
   :aspect-ratio-penalty
   :center-balance-penalty
   :crossing-penalty
   :total])

(defn format-number [n]
  (String/format Locale/ROOT "%.4f" (object-array [(double n)])))

(defn print-stress-energy [result]
  (when-let [energy (get-in result [:metrics :stress-energy])]
    (println "energy =")
    (doseq [component stress-energy-components]
      (println (str "  " (name component) " = "
                    (format-number (get energy component 0.0)))))
    (println)))

(defn draw-layout-result [result]
  (v/draw-shapes v/ascii-dim
                 (v/integer-shapes
                  (v/graph-to-shapes v/ascii-dim
                                     (vals (:nodes result))))))

(defn draw-section [edges nodes routing placement]
  (println)
  (println (str "=== " (:title routing) " / " (:title placement) " ==="))
  (let [result ((:layout placement) edges nodes (merge demo-opts (:opts routing)))]
    (print-stress-energy result)
    (draw-layout-result result)))

(defn symbol-graph [build-result]
  (let [sym simple-name
        {:keys [ids edges]} build-result
        nodes (into {} (map (fn [id] [(sym id) (name (sym id))]) ids))
        sym-edges (mapv (fn [[a b]] [(sym a) (sym b)]) edges)]
    {:nodes nodes :edges sym-edges}))

(defn full-class-graph []
  (symbol-graph (build-graph seeds)))

(defn sorted-node-ids [nodes]
  (sort (keys nodes)))

(defn subgraph-by-node-count [nodes edges node-count]
  (let [keep (set (take node-count (sorted-node-ids nodes)))]
    {:nodes (select-keys nodes keep)
     :edges (vec (filter (fn [[a b]] (and (keep a) (keep b))) edges))}))

(defn progressive-node-counts
  "Node counts from `start` in steps of `step` up to `total` (always includes total)."
  [total {:keys [start step] :or {start 10 step 10}}]
  (let [steps (range start (inc total) step)]
    (if (= (last steps) total)
      (vec steps)
      (conj (vec steps) total))))

(defn draw-stress-progressive
  "Draw stress-directed layout for increasing subgraph sizes."
  [nodes edges {:keys [spacing start step stress-iterations] :or {spacing 0.1 start 10 step 10 stress-iterations 80}}]
  (let [total (count nodes)
        opts {:stress-node-spacing (double spacing)
              :stress-iterations stress-iterations}]
    (println (str "=== stress-directed progressive (spacing=" spacing
                  ", " total " types max) ==="))
    (doseq [n (progressive-node-counts total {:start start :step step})]
      (let [{:keys [nodes edges]} (subgraph-by-node-count nodes edges n)]
        (println)
        (println (str "--- " n " nodes, " (count edges) " edges ---"))
        (v/draw-stress-directed-graph edges nodes opts)))))

(defn -main [& args]
  (when (and (seq args) (= (first args) "stress-progressive"))
    (let [spacing (if (> (count args) 1) (Double/parseDouble (second args)) 0.1)
          {:keys [nodes edges]} (full-class-graph)]
      (draw-stress-progressive nodes edges {:spacing spacing})
      (System/exit 0)))
  (let [{:keys [ids edges]} (build-graph seeds)
        sym (fn [c] (simple-name c))
        nodes (into {} (map (fn [id] [(sym id) (name (sym id))]) ids))
        sym-edges (mapv (fn [[a b]] [(sym a) (sym b)]) edges)]
    (println (str "=== clojure.lang type hierarchy: "
                  (count ids) " types, "
                  (count edges) " edges (subtype -> supertype) ==="))
    (println "=== 4 placement strategies x 2 routing strategies ===")
    (doseq [routing routing-strategies
            placement placement-strategies]
      (draw-section sym-edges nodes routing placement))))
