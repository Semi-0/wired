(ns propagators.compiler-2.runtime.operators.xr
  "XR compiler-2 runtime operators."
  (:require [propagators.compiler-2.runtime.boundary :as boundary]
            [propagators.compiler-2.runtime.ids :as runtime-ids]
            [propagators.cells.value :as value]
            [propagators.compiler-2.model.operator-value :as operator-value]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.event :as event]
            [propagators.datastructures.tms :as tms]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.semantic-trace :as semantic-trace]))

(defn- effect-slot-key [effect-id]
  (runtime-ids/effect-slot-key effect-id))

(defn- xr-effect-request
  [effect-id trace-graph receipt-id epoch]
  (boundary/xr-effect-request effect-id trace-graph receipt-id epoch))

(defn- xr-receipt
  [request status]
  (boundary/xr-receipt request status))

(defn- program-epoch
  [network]
  (or (:program/epoch (net/net-dict-or-empty network))
      0))

(defn- trace-graph-value
  [v]
  (cond
    (value/unusable? v)
    nil

    (semantic-trace/semantic-trace-graph? v)
    v

    (event/event-projection? v)
    (let [graphs (->> (obj/public-slot-keys v)
                      (keep #(trace-graph-value (obj/slot-value v %)))
                      distinct
                      vec)]
      (when (= 1 (count graphs))
        (first graphs)))

    (or (event/event-content? v)
        (event/event-fact? v))
    (trace-graph-value (event/strongest-value v))

    (tms/distributed-value? v)
    (let [projected (tms/strongest-distributed-value v)]
      (when-not (value/unusable? projected)
        (trace-graph-value (tms/distributed-base-value projected))))

    :else
    (let [base (or (behavior/base-value (behavior/strongest-value v))
                   (behavior/base-value v))]
      (when base
        (trace-graph-value base)))))

(defn- xr-launch-messages
  [network outbox-id trace-id receipt-id]
  (let [trace-graph (when trace-id
                      (trace-graph-value
                       (net/network-cell-strongest network trace-id)))
        epoch (program-epoch network)
        effect-id [:xr/launch-trace trace-id receipt-id epoch (hash trace-graph)]]
    (if (or (value/unusable? trace-graph)
            (not (semantic-trace/semantic-trace-graph? trace-graph)))
      []
      [(message outbox-id
                (obj/compound-object
                 {(effect-slot-key effect-id)
                  (xr-effect-request effect-id
                                     trace-graph
                                     receipt-id
                                     epoch)}))])))

(defn xr-io-operator [outbox-id]
  (operator-value/operator-closure
   {:name 'xr-io
    :output-selector (fn [arg-ids fallback-id]
                       (or (second (vec arg-ids)) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[trace-id maybe-receipt-id] (vec arg-ids)
                      receipt-id (or maybe-receipt-id out-id)]
                  (when-not (and trace-id receipt-id (#{1 2} (count arg-ids)))
                    (throw (ex-info "xr-io expects trace graph and optional receipt output"
                                    {:arg-ids arg-ids})))
                  (xr-launch-messages network outbox-id trace-id receipt-id)))}))

(defn io-xr-operator [outbox-id]
  (operator-value/operator-closure
   {:name 'io:xr
    :output-selector (fn [_arg-ids fallback-id]
                       fallback-id)
    :activate (fn [network _context-id arg-ids out-id]
                (let [[trace-id] (vec arg-ids)
                      receipt-id out-id]
                  (when-not (and trace-id receipt-id (= 1 (count arg-ids)))
                    (throw (ex-info "io:xr expects trace graph and returns receipt cell"
                                    {:arg-ids arg-ids})))
                  (xr-launch-messages network outbox-id trace-id receipt-id)))}))
