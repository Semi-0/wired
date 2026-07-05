(ns graph.compiler-2-runtime.graph-projection
  "Runtime semantic graph projection helpers."
  (:require [clojure.set :as set]
            [graph.compiler-2-runtime.state :as state]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.cells.cell :as cell]
            [propagators.cells.value :as value]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.compound-object :as obj]
            [propagators.network :as net]
            [propagators.semantic-trace :as semantic-trace]))

(def labels state/labels)
(def stable-node-id state/stable-node-id)

(defn display-widget-value
  [v]
  (when-not (value/unusable? v)
    (semantic-repl/display-cell-value v)))

(defn widget-node-id
  [widget-id]
  (stable-node-id :xr :widget widget-id))

(defn graph-cell-node-id
  [graph cell-id]
  (or (first (sort-by pr-str (get-in graph [:node-aliases cell-id])))
      (stable-node-id :xr :widget-cell cell-id)))

(defn graph-cell-node-ids
  [graph cell-id]
  (let [aliases (get-in graph [:node-aliases cell-id])]
    (if (seq aliases)
      aliases
      #{})))

(defn ensure-graph-cell-node
  [graph label cell-id]
  (let [node-id (graph-cell-node-id graph cell-id)]
    (-> graph
        (assoc-in [:nodes node-id] (or label "cell"))
        (update-in [:node-aliases cell-id] (fnil conj #{}) node-id))))

(defn graph-cell-value?
  [v]
  (cond
    (value/unusable? v)
    false

    (semantic-trace/semantic-trace-graph? v)
    false

    (and (contains? (obj/public-slot-keys v) behavior/base-layer)
         (contains? (obj/public-slot-keys v) behavior/summary-layer))
    true

    (behavior/behavior-value? v)
    true

    (net/network? v)
    false

    :else
    true))

(defn assoc-graph-cell-value
  [state cell-id]
  (let [label (get (labels state) cell-id)
        graph0 (ensure-graph-cell-node (:graph state) label cell-id)
        node-id (graph-cell-node-id graph0 cell-id)
        strongest (net/network-cell-strongest (:program/net state) cell-id)
        graph1 (if (graph-cell-value? strongest)
                 (assoc-in graph0 [:values node-id] strongest)
                 (update graph0 :values dissoc node-id))]
    (assoc state
           :graph graph1
           :program/graph graph1)))

(defn assoc-graph-cell-values
  [state cell-ids]
  (reduce assoc-graph-cell-value state cell-ids))

(defn program-strongest-snapshot
  [program-net]
  (into {}
        (keep (fn [[id entry]]
                (when (cell/cell? entry)
                  [id (cell/cell-strongest entry)])))
        (net/net-env program-net)))

(def ^:private missing-cell ::missing-cell)

(defn changed-cell-ids
  [before after]
  (->> (set/union (set (keys before)) (set (keys after)))
       (filter (fn [id]
                 (not (value/cell-value-equal?
                       (get before id missing-cell)
                       (get after id missing-cell)))))
       (sort-by pr-str)
       vec))

(defn record-runtime-transaction
  [state before-program-net]
  (let [before (if before-program-net
                 (program-strongest-snapshot before-program-net)
                 {})
        after (program-strongest-snapshot (:program/net state))
        graph (:graph state)
        changed (changed-cell-ids before after)
        pairs (keep (fn [cell-id]
                      (let [node-ids (seq (graph-cell-node-ids graph cell-id))]
                        (when node-ids
                          [cell-id node-ids])))
                    changed)
        cells (mapv first pairs)
        nodes (->> pairs
                   (mapcat second)
                   distinct
                   vec)]
    (assoc (assoc-graph-cell-values state cells)
           :runtime/changed-cells cells
           :runtime/changed-node-ids nodes)))

(defn widget-channel-view
  [state {:keys [widget/channel widget/view-cell widget/event-cell widget/view-value]}]
  (let [labels (labels state)]
    {:channel (str channel)
     :view-cell view-cell
     :view-cell-id (pr-str view-cell)
     :view-label (get labels view-cell)
     :event-cell event-cell
     :event-cell-id (pr-str event-cell)
     :event-label (get labels event-cell)
     :current (display-widget-value view-value)}))

(defn widget-ui
  [widget-type widget-id channels]
  {:kind "widget"
   :type (name widget-type)
   :widget-id (str widget-id)
   :channels (mapv #(select-keys %
                                 [:channel
                                  :view-label
                                  :event-label
                                  :current])
                   channels)})

(defn add-widget-to-graph
  [state widget-type widget-id channels]
  (let [node-id (widget-node-id widget-id)
        graph0 (:graph state)
        graph1 (assoc-in graph0 [:nodes node-id] (str widget-id))
        graph2 (assoc-in graph1 [:node-ui node-id] (widget-ui widget-type
                                                              widget-id
                                                              channels))
        graph3 (reduce
                (fn [graph {:keys [view-cell event-cell view-label event-label]}]
                  (let [view-node (graph-cell-node-id graph view-cell)
                        event-node (graph-cell-node-id graph event-cell)]
                    (-> graph
                        (ensure-graph-cell-node view-label view-cell)
                        (ensure-graph-cell-node event-label event-cell)
                        (update :edges (fnil into [])
                                [[view-node node-id]
                                 [node-id event-node]]))))
                graph2
                channels)]
    (assoc state
           :graph graph3
           :program/graph graph3)))

(defn record-widget-register
  [state request]
  (let [{:widget/keys [type id channels]} (:boundary/payload request)
        widget-id (str id)
        channels* (mapv #(widget-channel-view state %) channels)
        channel-map (into {}
                          (map (fn [channel]
                                 [(:channel channel) channel]))
                          channels*)]
    (-> state
        (assoc-in [:xr :widgets widget-id]
                  {:id widget-id
                   :type (name type)
                   :channels channel-map
                   :epoch (:boundary/epoch request)})
        (update-in [:xr :effects] (fnil conj []) request)
        (add-widget-to-graph type widget-id channels*))))
