(ns graph.compiler-2-runtime.operators.tui.common
  "Shared TUI operator target lookup and effect request helpers."
  (:require [graph.compiler-2-runtime.boundary :as boundary]
            [graph.compiler-2-runtime.ids :as runtime-ids]
            [propagators.datastructures.compound-object :as obj]
            [propagators.ids :as ids]
            [propagators.network :as net]))

(defn effect-slot-key [effect-id]
  (runtime-ids/effect-slot-key effect-id))

(defn declared-slot-parent-id
  [network block-id slot-key]
  (some->> (get (obj/accessor-declarations-for network block-id) slot-key)
           keys
           (sort-by pr-str)
           first))

(defn instance-block-head-id
  [network instance-id]
  (let [instance-value (net/network-cell-strongest network instance-id)
        instance-id (if (ids/node-id? instance-value)
                      instance-value
                      instance-id)]
    (when-let [blocks-id (declared-slot-parent-id network
                                                  instance-id
                                                  :instance/blocks)]
      (net/network-cell-strongest network blocks-id))))

(defn block-at-slot-id
  [network instance-id index-id slot-key]
  (let [wanted-index (net/network-cell-strongest network index-id)
        first-block-id (instance-block-head-id network instance-id)]
    (loop [block-id first-block-id
           seen #{}]
      (when (and (ids/node-id? block-id)
                 (not (contains? seen block-id)))
        (let [index-cell-id (declared-slot-parent-id network
                                                     block-id
                                                     :block/index)
              slot-cell-id (declared-slot-parent-id network block-id slot-key)
              next-cell-id (declared-slot-parent-id network block-id :cdr)
              block-index (when index-cell-id
                            (net/network-cell-strongest network index-cell-id))]
          (if (= wanted-index block-index)
            slot-cell-id
            (recur (when next-cell-id
                     (net/network-cell-strongest network next-cell-id))
                   (conj seen block-id))))))))

(defn block-at-text-id
  [network instance-id index-id]
  (block-at-slot-id network instance-id index-id :block/text))

(defn block-at-display-id
  [network instance-id index-id]
  (block-at-slot-id network instance-id index-id :block/display))

(defn tui-write-effect-request
  [effect-id text-id payload epoch]
  (boundary/tui-write-effect-request effect-id text-id payload epoch))

(defn tui-display-effect-request
  [effect-id display-id payload tick]
  (boundary/tui-display-effect-request effect-id display-id payload tick))

(defn trace-target-value
  [label source-id]
  {:trace/target true
   :trace/symbol label
   :node source-id
   :label label})
