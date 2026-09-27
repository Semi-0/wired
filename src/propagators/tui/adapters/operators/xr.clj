(ns propagators.tui.adapters.operators.xr
  "XR compiler-2 runtime operators."
  (:require [propagators.runtime.boundary :as boundary]
            [propagators.runtime.ids :as runtime-ids]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.infra.datastructures.behavior :as behavior]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.datastructures.tms :as tms]
            [propagators.infra.experimental.visualization.data :as collection-data]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.semantic-trace :as semantic-trace]
            [propagators.infra.visualizer :as visualizer]))

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

(defn- boundary-value
  [accepted? v]
  (cond
    (value/unusable? v)
    nil

    (accepted? v)
    v

    (event/event-projection? v)
    (let [values (->> (obj/public-slot-keys v)
                      (keep #(boundary-value accepted? (obj/slot-value v %)))
                      distinct
                      vec)]
      (when (= 1 (count values))
        (first values)))

    (or (event/event-content? v)
        (event/event-fact? v))
    (boundary-value accepted? (event/strongest-value v))

    (tms/distributed-value? v)
    (let [projected (tms/strongest-distributed-value v)]
      (when-not (value/unusable? projected)
        (boundary-value accepted? (tms/distributed-base-value projected))))

    :else
    (let [base (or (behavior/base-value (behavior/strongest-value v))
                   (behavior/base-value v))]
      (when base
        (boundary-value accepted? base)))))

(defn- trace-graph-value
  [v]
  (boundary-value semantic-trace/semantic-trace-graph? v))

(defn- view-declaration-value
  [v]
  (boundary-value visualizer/view-declaration? v))

(defn- xr-launch-messages
  [network outbox-id trace-id receipt-id]
  (let [source-value (when trace-id
                       (net/network-cell-strongest network trace-id))
        trace-graph (trace-graph-value source-value)
        view (if (collection-data/collection? source-value)
               (visualizer/collection-declaration trace-id trace-id)
               (view-declaration-value source-value))
        epoch (program-epoch network)
        trace-effect-id [:xr/launch-trace trace-id receipt-id epoch (hash trace-graph)]
        view-effect-id [:xr/present-view trace-id receipt-id epoch (hash view)]]
    (cond
      (semantic-trace/semantic-trace-graph? trace-graph)
      [(message outbox-id
                (obj/compound-object
                 {(effect-slot-key trace-effect-id)
                  (xr-effect-request trace-effect-id trace-graph receipt-id epoch)}))]

      (visualizer/view-declaration? view)
      [(message outbox-id
                (obj/compound-object
                 {(effect-slot-key view-effect-id)
                  (boundary/xr-view-request view-effect-id view receipt-id epoch)}))]

      :else
      [])))

(defn xr-io-operator [outbox-id]
  (operator-value/operator-closure
   {:name 'xr-io
    :output-selector (fn [arg-ids fallback-id]
                       (or (second (vec arg-ids)) fallback-id))
    :activate (fn [network _context-id arg-ids out-id]
                (let [[trace-id maybe-receipt-id] (vec arg-ids)
                      receipt-id (or maybe-receipt-id out-id)]
                  (when-not (and trace-id receipt-id (#{1 2} (count arg-ids)))
                    (throw (ex-info "xr-io expects a graph or view and optional receipt output"
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
