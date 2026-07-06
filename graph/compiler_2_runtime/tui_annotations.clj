(ns graph.compiler-2-runtime.tui-annotations
  "Display annotations for projected TUI block values."
  (:require [clojure.string :as str]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.cells.value :as value]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.tms :as tms]
            [propagators.network :as net]
            [propagators.semantic-trace :as semantic-trace]))

(defn- behavior-projection?
  [v]
  (and (contains? (obj/public-slot-keys v) behavior/base-layer)
       (contains? (obj/public-slot-keys v) behavior/summary-layer)))

(defn project-value
  [v]
  (cond
    (value/unusable? v)
    v

    (semantic-trace/semantic-trace-graph? v)
    v

    (behavior-projection? v)
    (project-value (behavior/base-value v))

    (behavior/behavior-value? v)
    (let [current (behavior/strongest-value v)]
      (if (value/unusable? current)
        current
        (project-value current)))

    (tms/distributed-value? v)
    (let [projected (tms/strongest-distributed-value v)]
      (if (value/unusable? projected)
        projected
        (tms/distributed-base-value projected)))

    (net/network? v)
    (or (semantic-repl/display-cell-value v) "network")

    :else
    v))

(defn- sorted-pr
  [xs]
  (->> xs (sort-by pr-str) vec))

(defn- behavior-latest-time
  [v]
  (cond
    (behavior/behavior-value? v)
    (let [current (behavior/strongest-value v)]
      (when-not (value/unusable? current)
        (behavior/summary-latest-time current)))

    (behavior-projection? v)
    (behavior/summary-latest-time v)))

(defn- behavior-annotation
  [v]
  (let [identities (behavior/identity-set v)]
    (when (seq identities)
      {:kind :behavior
       :identities (sorted-pr identities)
       :latest-time (behavior-latest-time v)})))

(declare value-annotations)

(defn- behavior-base-value
  [v]
  (cond
    (behavior/behavior-value? v)
    (let [current (behavior/strongest-value v)]
      (when-not (value/unusable? current)
        (behavior/base-value current)))

    (behavior-projection? v)
    (behavior/base-value v)))

(defn- behavior-annotations
  [v]
  (let [self (behavior-annotation v)
        base (behavior-base-value v)
        nested (when base (value-annotations base))
        nested-behavior (first (filter #(= :behavior (:kind %)) nested))
        redundant-wrapper? (and self
                                nested-behavior
                                (= (:identities self)
                                   (:identities nested-behavior)))]
    (cond-> []
      (and self (not redundant-wrapper?)) (conj self)
      (seq nested) (into nested))))

(defn- tms-annotation
  [v]
  (let [projected (tms/strongest-distributed-value v)]
    (when-not (value/unusable? projected)
      (let [view (tms/tms-view (tms/distributed-slots projected))
            entry (get (tms/propositions view) tms/distributed-proposition)]
        {:kind :tms
         :status (:tms/status entry)
         :claims (sorted-pr (:tms/claims entry))
         :active-premises (sorted-pr (tms/active-premises view))}))))

(defn- tms-annotations
  [v]
  (let [projected (tms/strongest-distributed-value v)
        base (when-not (value/unusable? projected)
               (tms/distributed-base-value projected))
        annotation (tms-annotation v)]
    (cond-> []
      annotation (conj annotation)
      base (into (value-annotations base)))))

(defn value-annotations
  [v]
  (cond
    (value/unusable? v) []
    (behavior/behavior-value? v) (behavior-annotations v)
    (behavior-projection? v) (behavior-annotations v)
    (tms/distributed-value? v) (tms-annotations v)
    :else []))

(defn- annotation-token
  [x]
  (cond
    (keyword? x) (name x)
    (symbol? x) (name x)
    (string? x) x
    :else (pr-str x)))

(defn- behavior-annotation-label
  [{:keys [identities latest-time]}]
  (str "[be:"
       (str/join "," (map annotation-token identities))
       (when (some? latest-time)
         (str " @" latest-time))
       "]"))

(defn- tms-annotation-label
  [{:keys [status claims active-premises]}]
  (str "[tms:"
       (or (some-> status name) "unknown")
       (when (seq claims)
         (str " claims=" (str/join "," (map annotation-token claims))))
       (when (seq active-premises)
         (str " premises=" (str/join "," (map annotation-token active-premises))))
       "]"))

(defn format-annotations
  [annotations]
  (when (seq annotations)
    (str/join
     " "
     (map (fn [{:keys [kind] :as annotation}]
            (case kind
              :behavior (behavior-annotation-label annotation)
              :tms (tms-annotation-label annotation)
              (pr-str annotation)))
          annotations))))

(defn annotate-block-view
  [block-view raw-value]
  (let [annotations (value-annotations raw-value)]
    (cond-> block-view
      (seq annotations)
      (assoc :annotations annotations
             :annotation (format-annotations annotations)))))
