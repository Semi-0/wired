(ns graph.compiler-2-runtime.operators.tui.targets
  "TUI target and instance compiler-2 operators."
  (:require [graph.compiler-2-runtime.operators.tui.common :as common]
            [propagators.cells.value :as value]
            [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.datastructures.compound-object :as obj]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]
            [propagators.stdlib.prop :as stdlib-prop]))

(def effect-slot-key common/effect-slot-key)
(def block-at-text-id common/block-at-text-id)
(def p:tui-write-request common/p:tui-write-request)
(def tui-write-effect-request common/tui-write-effect-request)
(def trace-target-value common/trace-target-value)

(defn block-target-operator
  [outbox-id instance-id]
  (with-meta
    (fn [network arg-ids out-id]
      (let [[index-id maybe-out] (vec arg-ids)
            target-id (or maybe-out out-id)
            text-id (and instance-id
                         index-id
                         (block-at-text-id network instance-id index-id))]
        (when-not (and (#{1 2} (count arg-ids)) text-id)
          (throw (ex-info "block expects a known block index"
                          {:arg-ids arg-ids
                           :index (when index-id
                                    (net/network-cell-strongest network
                                                              index-id))})))
        (let [network (nb/ensure-cell network outbox-id)
              [read-prop n1] ((stdlib-prop/id text-id target-id) network)
              [write-prop n2] ((p:tui-write-request target-id
                                                     text-id
                                                     outbox-id)
                               n1)]
          [n2 [read-prop write-prop] target-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 1 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[index-id maybe-out] (vec arg-ids)
             target-id (or maybe-out out-id)
             text-id (and instance-id
                          index-id
                          (block-at-text-id current-net instance-id index-id))
             source-value (when text-id
                            (net/network-cell-strongest current-net text-id))
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
         (when-not (#{1 2} (count arg-ids))
           (throw (ex-info "block expects index and optional output"
                           {:arg-ids arg-ids})))
         (cond-> []
           text-id
           (conj (message target-id source-value))

           write-message
           (conj write-message))))}))

(defn trace-target-operator []
  (with-meta
    (fn [network arg-ids out-id]
      (let [[label-id source-id maybe-out] (vec arg-ids)
            target-id (or maybe-out out-id)]
        (when-not (and label-id source-id (#{2 3} (count arg-ids)))
          (throw (ex-info "trace-target expects label, source, and optional output"
                          {:arg-ids arg-ids})))
        (let [[prop-id n]
              ((prop/construct-propagator
                (fn [_inputs _outputs current-net]
                  (let [label (net/network-cell-strongest current-net label-id)]
                    (if (value/unusable? label)
                      []
                      [(message target-id
                                (trace-target-value label source-id))])))
                [label-id]
                [target-id])
               network)]
          [n [prop-id] target-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 2 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[label-id source-id maybe-out] (vec arg-ids)
             target-id (or maybe-out out-id)
             label (net/network-cell-strongest current-net label-id)]
         (when-not (and label-id source-id (#{2 3} (count arg-ids)))
           (throw (ex-info "trace-target expects label, source, and optional output"
                           {:arg-ids arg-ids})))
         (if (value/unusable? label)
           []
           [(message target-id (trace-target-value label source-id))])))}))

(defn instance-operator []
  (with-meta
    (fn [network arg-ids out-id]
      (let [[instance-id maybe-out] (vec arg-ids)
            target-id (or maybe-out out-id)]
        (when-not (#{1 2} (count arg-ids))
          (throw (ex-info "instance expects instance and optional output"
                          {:arg-ids arg-ids})))
        (let [[read-prop n1] ((stdlib-prop/id instance-id target-id) network)
              [write-prop n2] ((stdlib-prop/id target-id instance-id) n1)]
          [n2 [read-prop write-prop] target-id])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (nth (vec arg-ids) 1 nil) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids out-id]
       (let [[instance-id maybe-out] (vec arg-ids)
             target-id (or maybe-out out-id)
             source-value (net/network-cell-strongest current-net instance-id)
             target-value (net/network-cell-strongest current-net target-id)]
         (when-not (#{1 2} (count arg-ids))
           (throw (ex-info "instance expects instance and optional output"
                           {:arg-ids arg-ids})))
         (cond-> []
           (not (value/unusable? source-value))
           (conj (message target-id source-value))

           (not (value/unusable? target-value))
           (conj (message instance-id target-value)))))}))
