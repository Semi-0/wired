(ns graph.compiler-2-runtime.operators.xr
  "XR compiler-2 runtime operators."
  (:require [graph.compiler-2-runtime.boundary :as boundary]
            [graph.compiler-2-runtime.ids :as runtime-ids]
            [propagators.cells.value :as value]
            [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.datastructures.compound-object :as obj]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]
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

(defn- p:xr-io-request
  [trace-id outbox-id receipt-id]
  (prop/construct-propagator
   (fn [_inputs _outputs network]
     (let [trace-graph (net/network-cell-strongest network trace-id)
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
   [trace-id]
   [outbox-id]))

(defn xr-io-operator [outbox-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[trace-id maybe-receipt-id] (vec arg-ids)
            receipt-id (or maybe-receipt-id out-id)]
        (when-not (and trace-id receipt-id (#{1 2} (count arg-ids)))
          (throw (ex-info "xr-io expects trace graph and optional receipt output"
                          {:arg-ids arg-ids})))
        (let [network* (-> network
                           (nb/ensure-cell outbox-id)
                           (nb/ensure-cell receipt-id))
              [prop-id n] ((p:xr-io-request trace-id outbox-id receipt-id)
                           network*)]
          [n [prop-id] receipt-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (second (vec arg-ids)) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[trace-id maybe-receipt-id] (vec arg-ids)
             receipt-id (or maybe-receipt-id out-id)
             trace-graph (net/network-cell-strongest current-net trace-id)
             epoch (program-epoch current-net)
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
                                          epoch)}))])))}))

(defn io-xr-operator [outbox-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[trace-id] (vec arg-ids)
            receipt-id out-id]
        (when-not (and trace-id receipt-id (= 1 (count arg-ids)))
          (throw (ex-info "io:xr expects trace graph and returns receipt cell"
                          {:arg-ids arg-ids})))
        (let [network* (-> network
                           (nb/ensure-cell outbox-id)
                           (nb/ensure-cell receipt-id))
              [prop-id n] ((p:xr-io-request trace-id outbox-id receipt-id)
                           network*)]
          [n [prop-id] receipt-id])))
    {compiler-helpers/output-selector-key
     (fn [_arg-ids fallback-id]
       fallback-id)
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[trace-id] (vec arg-ids)
             receipt-id out-id
             trace-graph (when trace-id
                           (net/network-cell-strongest current-net trace-id))
             epoch (program-epoch current-net)
             effect-id [:xr/launch-trace trace-id receipt-id epoch (hash trace-graph)]]
         (when-not (and trace-id receipt-id (= 1 (count arg-ids)))
           (throw (ex-info "io:xr expects trace graph and returns receipt cell"
                           {:arg-ids arg-ids})))
         (if (or (value/unusable? trace-graph)
                 (not (semantic-trace/semantic-trace-graph? trace-graph)))
           []
           [(message outbox-id
                     (obj/compound-object
                      {(effect-slot-key effect-id)
                       (xr-effect-request effect-id
                                          trace-graph
                                          receipt-id
                                          epoch)}))])))}))
