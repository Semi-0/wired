(ns graph.compiler-2-runtime.operators.tui.effects
  "TUI block write/display effect compiler-2 operators."
  (:require [graph.compiler-2-runtime.operators.tui.common :as common]
            [propagators.cells.value :as value]
            [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.datastructures.compound-object :as obj]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.stdlib.prop :as stdlib-prop]))

(def effect-slot-key common/effect-slot-key)
(def block-at-text-id common/block-at-text-id)
(def block-at-display-id common/block-at-display-id)
(def p:tui-write-request common/p:tui-write-request)
(def p:tui-display-request common/p:tui-display-request)
(def tui-write-effect-request common/tui-write-effect-request)
(def tui-display-effect-request common/tui-display-effect-request)

(defn block-at-operator [outbox-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[instance-id index-id maybe-out] (vec arg-ids)
            target-id (or maybe-out out-id)
            text-id (block-at-text-id network instance-id index-id)]
        (when-not (and (#{2 3} (count arg-ids)) text-id)
          (throw (ex-info "block-at expects a known instance and index"
                          {:arg-ids arg-ids
                           :index (net/network-cell-strongest network
                                                             index-id)})))
        (let [network (nb/ensure-cell network outbox-id)
              [read-prop n1] ((stdlib-prop/id text-id target-id) network)
              [write-prop n2] ((p:tui-write-request target-id
                                                     text-id
                                                     outbox-id)
                               n1)]
          [n2 [read-prop write-prop] target-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 2 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[instance-id index-id maybe-out] (vec arg-ids)
             target-id (or maybe-out out-id)
             text-id (block-at-text-id current-net instance-id index-id)
             source-value (when text-id
                            (net/network-cell-strongest current-net
                                                        text-id))
             target-value (net/network-cell-strongest current-net target-id)
             write-message (when (and text-id
                                      (not (value/nothing? target-value)))
                             (let [epoch (or (:program/epoch
                                              (net/net-dict-or-empty current-net))
                                             0)
                                   effect-id [:tui/write-block
                                              text-id
                                              epoch
                                              (hash target-value)]]
                               (message outbox-id
                                        (obj/compound-object
                                         {(effect-slot-key effect-id)
                                          (tui-write-effect-request effect-id
                                                                    text-id
                                                                    target-value
                                                                    epoch)}))))]
         (when-not (#{2 3} (count arg-ids))
           (throw (ex-info "block-at expects instance, index, and optional output"
                           {:arg-ids arg-ids})))
         (cond-> []
           text-id
           (conj (message target-id source-value))

           write-message
           (conj write-message))))}))

(defn be-block-at-operator [outbox-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[instance-id index-id source-id] (vec arg-ids)
            display-id (block-at-display-id network instance-id index-id)]
        (when-not (and (= 3 (count arg-ids)) display-id)
          (throw (ex-info "be:block-at expects a known instance, index, and source"
                          {:arg-ids arg-ids
                           :index (net/network-cell-strongest network
                                                             index-id)})))
        (let [network (nb/ensure-cell network outbox-id)
              [write-prop n] ((p:tui-display-request source-id
                                                     display-id
                                                     outbox-id)
                              network)]
          [n [write-prop] source-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 2 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[instance-id index-id source-id] (vec arg-ids)
             source-id (or source-id out-id)
             display-id (when (and instance-id index-id)
                          (block-at-display-id current-net
                                               instance-id
                                               index-id))
             target-value (when source-id
                            (net/network-cell-strongest current-net source-id))
             tick (or (:runtime/commit-tick (net/net-dict-or-empty current-net))
                      (:program/epoch (net/net-dict-or-empty current-net))
                      0)
             write-message (when (and display-id
                                      source-id
                                      (not (value/nothing? target-value)))
                             (let [effect-id [:tui/write-display
                                              display-id
                                              tick
                                              (hash target-value)]]
                               (message outbox-id
                                        (obj/compound-object
                                         {(effect-slot-key effect-id)
                                          (tui-display-effect-request effect-id
                                                                      display-id
                                                                      target-value
                                                                      tick)}))))]
         (when-not (= 3 (count arg-ids))
           (throw (ex-info "be:block-at expects instance, index, and source"
                           {:arg-ids arg-ids})))
         (cond-> []
           write-message
           (conj write-message))))}))

(defn be-block-target-operator [outbox-id instance-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[index-id maybe-source-id] (vec arg-ids)
            source-id (or maybe-source-id out-id)
            display-id (and instance-id
                            index-id
                            (block-at-display-id network instance-id index-id))]
        (when-not (and (#{1 2} (count arg-ids)) display-id source-id)
          (throw (ex-info "be:block expects a known block index and optional source"
                          {:arg-ids arg-ids
                           :index (when index-id
                                    (net/network-cell-strongest network
                                                              index-id))})))
        (let [network (nb/ensure-cell network outbox-id)
              [write-prop n] ((p:tui-display-request source-id
                                                     display-id
                                                     outbox-id)
                              network)]
          [n [write-prop] source-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 1 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[index-id maybe-source-id] (vec arg-ids)
             source-id (or maybe-source-id out-id)
             display-id (when (and instance-id index-id)
                          (block-at-display-id current-net
                                               instance-id
                                               index-id))
             target-value (when source-id
                            (net/network-cell-strongest current-net source-id))
             tick (or (:runtime/commit-tick (net/net-dict-or-empty current-net))
                      (:program/epoch (net/net-dict-or-empty current-net))
                      0)
             write-message (when (and display-id
                                      source-id
                                      (not (value/nothing? target-value)))
                             (let [effect-id [:tui/write-display
                                              display-id
                                              tick
                                              (hash target-value)]]
                               (message outbox-id
                                        (obj/compound-object
                                         {(effect-slot-key effect-id)
                                          (tui-display-effect-request effect-id
                                                                      display-id
                                                                      target-value
                                                                      tick)}))))]
         (when-not (#{1 2} (count arg-ids))
           (throw (ex-info "be:block expects index and optional source"
                           {:arg-ids arg-ids})))
         (cond-> []
           write-message
           (conj write-message))))}))
