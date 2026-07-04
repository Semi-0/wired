(ns graph.compiler-2-runtime.widget
  "Compiler-2 runtime widget IO operators for XR/browser projections."
  (:require [graph.compiler-2-runtime.ids :as runtime-ids]
            [propagators.cells.value :as value]
            [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.datastructures.compound-object :as obj]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]))

(defn- widget-register-request
  [effect-id widget-type widget-id channels epoch]
  {:boundary/effect true
   :boundary/id effect-id
   :boundary/port :xr
   :boundary/kind :xr/widget-register
   :boundary/payload {:widget/type widget-type
                      :widget/id widget-id
                      :widget/channels channels}
   :boundary/epoch epoch})

(defn- channel-value
  [network channel]
  (assoc channel
         :widget/view-value
         (net/network-cell-strongest network (:widget/view-cell channel))))

(defn- register-messages
  [network outbox-id out-id widget-type widget-id-id channels]
  (let [widget-id (net/network-cell-strongest network widget-id-id)
        epoch (or (:program/epoch (net/net-dict-or-empty network)) 0)]
    (if (value/unusable? widget-id)
      []
      (let [channels* (mapv #(channel-value network %) channels)
            descriptor {:widget/type widget-type
                        :widget/id widget-id
                        :widget/channels channels*}
            effect-id [:xr/widget-register widget-type widget-id epoch
                       (hash (mapv #(select-keys %
                                                 [:widget/channel
                                                  :widget/view-cell
                                                  :widget/event-cell
                                                  :widget/view-value])
                                    channels*))]]
        [(message outbox-id
                  (obj/compound-object
                   {(runtime-ids/effect-slot-key effect-id)
                    (widget-register-request effect-id
                                             widget-type
                                             widget-id
                                             channels*
                                             epoch)}))
         (message out-id descriptor)]))))

(defn slider-io-operator
  [outbox-id]
  (with-meta
    (fn [network arg-ids fallback-id]
      (let [[widget-id-id view-id event-id explicit-out-id] (vec arg-ids)
            out-id (or explicit-out-id fallback-id)]
        (when-not (and widget-id-id view-id event-id out-id
                       (<= 3 (count arg-ids) 4))
          (throw (ex-info "slider-io expects widget-id, view, event source, and optional output"
                          {:arg-ids arg-ids})))
        [(-> network
             (nb/ensure-cell outbox-id)
             (nb/ensure-cell out-id))
         []
         out-id]))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 3 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [network _context-id arg-ids fallback-id]
       (let [[widget-id-id view-id event-id explicit-out-id] (vec arg-ids)
             out-id (or explicit-out-id fallback-id)]
         (when-not (and widget-id-id view-id event-id out-id
                        (<= 3 (count arg-ids) 4))
           (throw (ex-info "slider-io expects widget-id, view, event source, and optional output"
                           {:arg-ids arg-ids})))
         (register-messages network
                            outbox-id
                            out-id
                            :slider
                            widget-id-id
                            [{:widget/channel "value"
                              :widget/view-cell view-id
                              :widget/event-cell event-id}])))}))

(defn- panel-channels
  [arg-ids]
  (let [triples (partition 3 arg-ids)]
    (mapv (fn [[channel-id view-id event-id]]
            {:widget/channel-id channel-id
             :widget/view-cell view-id
             :widget/event-cell event-id})
          triples)))

(defn- resolve-panel-channel
  [network channel]
  (let [channel-name (net/network-cell-strongest network (:widget/channel-id channel))]
    (when-not (value/unusable? channel-name)
      (-> channel
          (dissoc :widget/channel-id)
          (assoc :widget/channel (str channel-name))))))

(defn slider-panel-io-operator
  [outbox-id]
  (with-meta
    (fn [network arg-ids fallback-id]
      (let [arg-ids (vec arg-ids)
            explicit-out-id (when (= 2 (mod (count arg-ids) 3))
                              (peek arg-ids))
            widget-id-id (first arg-ids)
            channel-args (subvec arg-ids 1 (- (count arg-ids)
                                               (if explicit-out-id 1 0)))
            out-id (or explicit-out-id fallback-id)]
        (when-not (and widget-id-id out-id
                       (pos? (count channel-args))
                       (zero? (mod (count channel-args) 3)))
          (throw (ex-info "slider-panel-io expects panel-id plus channel/view/event triples and optional output"
                          {:arg-ids arg-ids})))
        [(-> network
             (nb/ensure-cell outbox-id)
             (nb/ensure-cell out-id))
         []
         out-id]))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (let [arg-ids (vec arg-ids)]
         (if (= 2 (mod (count arg-ids) 3))
           (peek arg-ids)
           fallback-id)))
     compiler-helpers/application-activate-key
     (fn [network _context-id arg-ids fallback-id]
       (let [arg-ids (vec arg-ids)
             explicit-out-id (when (= 2 (mod (count arg-ids) 3))
                               (peek arg-ids))
             widget-id-id (first arg-ids)
             channel-args (subvec arg-ids 1 (- (count arg-ids)
                                                (if explicit-out-id 1 0)))
             out-id (or explicit-out-id fallback-id)
             channels (vec (keep #(resolve-panel-channel network %)
                                  (panel-channels channel-args)))]
         (when-not (and widget-id-id out-id
                        (pos? (count channel-args))
                        (zero? (mod (count channel-args) 3)))
           (throw (ex-info "slider-panel-io expects panel-id plus channel/view/event triples and optional output"
                           {:arg-ids arg-ids})))
         (if (not= (count channels) (count (panel-channels channel-args)))
           []
           (register-messages network
                              outbox-id
                              out-id
                              :slider-panel
                              widget-id-id
                              channels))))}))
