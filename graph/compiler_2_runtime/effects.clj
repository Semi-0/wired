(ns graph.compiler-2-runtime.effects
  "Boundary effect delivery for compiler-2 runtime."
  (:require [graph.compiler-2-runtime.block-model :as block-model]
            [graph.compiler-2-runtime.boundary :as boundary]
            [graph.compiler-2-runtime.graph-projection :as graphp]
            [graph.compiler-2-runtime.state :as state]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.cells.value :as value]
            [propagators.core :as core]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.compound-object :as obj]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.semantic-trace :as semantic-trace]))

(def boundary-outbox-id state/boundary-outbox-id)
(def receipt-slot-key state/receipt-slot-key)
(def xr-receipt state/xr-receipt)
(def record-widget-register graphp/record-widget-register)
(def block-by-text-id block-model/block-by-text-id)
(def block-by-display-id block-model/block-by-display-id)
(def update-block block-model/update-block)

(defn assoc-block-current-text
  [state block payload tick]
  (update-block state
                (:client-id block)
                (:index block)
                #(assoc % :text-current payload
                          :text-effect-tick tick)))

(defn write-block-value
  [state block epoch payload]
  (let [epoch (long (or epoch 0))
        current-tick (long (or (:text-effect-tick block) Long/MIN_VALUE))]
    (if (< epoch current-tick)
      state
      (let [state (assoc-block-current-text state block payload epoch)
        current (net/network-cell-strongest (:network state) (:text-id block))]
        (if (or (= current payload)
                (and (not (value/unusable? current))
                     (not= current payload)))
          state
          (let [[tasks n1] (core/eval-cells [(message (:text-id block) payload)]
                                            (:network state))
                n2 (core/run-tasks tasks n1)]
            (assoc state :network n2)))))))

(defn outbox-effects
  [program-net]
  (if-not (contains? (net/net-env program-net) (boundary-outbox-id))
    []
    (let [outbox (net/network-cell-strongest program-net (boundary-outbox-id))]
      (if (value/unusable? outbox)
        []
        (->> (obj/public-slot-keys outbox)
             (keep (fn [slot-key]
                     (let [request (obj/slot-value outbox slot-key)]
                       (when (:boundary/effect request)
                         request))))
             vec)))))

(defn receipt-message
  [request status]
  (message (:boundary/receipt-id request)
           (obj/compound-object
            {(receipt-slot-key (:boundary/id request))
             (xr-receipt request status)})))

(defn record-xr-launch
  [state request]
  (if (get-in state [:xr :launched (:boundary/id request)])
    state
    (let [receipt (receipt-message request :delivered)
          [tasks program-net] (core/eval-cells [receipt] (:program/net state))]
      (-> state
          (assoc :program/net program-net)
          (update-in [:xr :launched]
                     (fnil assoc {})
                     (:boundary/id request)
                     {:request request
                      :receipt (:value receipt)})
          (update-in [:xr :effects]
                     (fnil conj [])
                     request)
          (assoc :program/pending-after-effects tasks)))))

(defn record-trace-subscription
  [state boundary-request]
  (let [{trace-request :request target-id :target-id}
        (:boundary/payload boundary-request)
        subscription-id (:boundary/id boundary-request)]
    (assoc-in state
              [:trace/subscriptions subscription-id]
              {:id subscription-id
               :request trace-request
               :target-id target-id
               :created-epoch (:boundary/epoch boundary-request)})))

(defn record-tui-write
  [state request]
  (let [text-id (get-in request [:boundary/target :text-id])
        payload (:boundary/payload request)]
    (if-let [block (block-by-text-id state text-id)]
      (if (some? (:order block))
        state
        (-> state
            (write-block-value block (:boundary/epoch request) payload)
            (update-in [:tui :effects] (fnil conj []) request)))
      state)))

(defn display-behavior-update
  [display-id tick payload]
  (behavior/retained-value [:tui/display display-id]
                           tick
                           payload
                           #{[:tui/display display-id tick]}))

(defn write-block-display-value
  [state block tick payload]
  (let [update (display-behavior-update (:display-id block) tick payload)
        [tasks n1] (core/eval-cells [(message (:display-id block) update)]
                                    (:network state))
        n2 (core/run-tasks tasks n1)]
    (assoc state :network n2)))

(defn record-tui-display
  [state request]
  (let [display-id (get-in request [:boundary/target :display-id])
        payload (:boundary/payload request)
        tick (or (:boundary/tick request)
                 (:boundary/epoch request)
                 0)]
    (if-let [block (block-by-display-id state display-id)]
      (-> state
          (write-block-display-value block tick payload)
          (update-in [:tui :effects] (fnil conj []) request))
      state)))

(defn graph-score
  [request]
  (let [payload (:boundary/payload request)
        graph (if (semantic-trace/semantic-trace-graph? payload)
                payload
                (:graph payload))]
    (+ (count (:nodes graph))
       (count (:edges graph))
       (count (:values graph)))))

(defn delivery-key
  [request]
  (case [(:boundary/port request) (:boundary/kind request)]
    [:tui :tui/write-display]
    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/target request)
     (:boundary/epoch request)]

    [:tui :tui/write-block]
    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/target request)
     (:boundary/epoch request)]

    [(:boundary/port request)
     (:boundary/kind request)
     (:boundary/receipt-id request)
     (:boundary/epoch request)]))

(defn prefer-latest-boundary-request?
  [request]
  (= [(:boundary/port request) (:boundary/kind request)]
     [:xr :xr/launch-trace]))

(defn richer-boundary-request
  [old request]
  (if (> (graph-score old) (graph-score request))
    old
    request))

(defn usable-boundary-request
  [old request]
  (let [old-unusable? (value/unusable? (:boundary/payload old))
        request-unusable? (value/unusable? (:boundary/payload request))]
    (cond
      (and old-unusable? (not request-unusable?)) request
      (and request-unusable? (not old-unusable?)) old
      :else nil)))

(defn better-boundary-request
  [old request]
  (let [usable (when old
                 (usable-boundary-request old request))]
    (cond
      (nil? old)
      request

      usable
      usable

      (prefer-latest-boundary-request? request)
      (richer-boundary-request old request)

      (>= (graph-score old) (graph-score request))
      old

      :else
      request)))

(defn collapse-boundary-effects
  [requests]
  (->> requests
       (reduce (fn [acc request]
                 (update acc
                         (delivery-key request)
                         #(better-boundary-request % request)))
               {})
       vals))

(defn perform-boundary-effects
  [state]
  (reduce (fn [s request]
            (case [(:boundary/port request) (:boundary/kind request)]
              [:xr :xr/launch-trace] (record-xr-launch s request)
              [:xr :xr/trace-subscribe] (record-trace-subscription s request)
              [:xr :xr/widget-register] (record-widget-register s request)
              [:tui :tui/write-block] (record-tui-write s request)
              [:tui :tui/write-display] (record-tui-display s request)
              s))
          state
          (collapse-boundary-effects (outbox-effects (:program/net state)))))

(defn refresh-program-graph
  [state]
  (if (and (:compiled state) (:program/net state))
    (let [fresh (semantic-repl/compiled-semantic-graph (:compiled state)
                                                       (:program/net state))
          graph (semantic-trace/graph-union (:graph state) fresh)]
      (assoc state
             :graph graph
             :program/graph graph
             :compiled-network (:program/net state)))
    state))

(defn run-runtime-cycle
  "Apply already-committed model state through propagation effects.

  Public for tests; command handlers should keep using the higher-level TUI/XR
  operations unless they are deliberately testing the runtime cycle boundary."
  [state]
  (perform-boundary-effects (refresh-program-graph state)))
