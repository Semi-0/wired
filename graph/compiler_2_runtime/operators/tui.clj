(ns graph.compiler-2-runtime.operators.tui
  "TUI/block compiler-2 runtime operators."
  (:require [graph.compiler-2-runtime.boundary :as boundary]
            [graph.compiler-2-runtime.ids :as runtime-ids]
            [propagators.cells.value :as value]
            [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.datastructures.compound-object :as obj]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]
            [propagators.stdlib.prop :as stdlib-prop]))

(defn- effect-slot-key [effect-id]
  (runtime-ids/effect-slot-key effect-id))

(defn- declared-slot-parent-id
  [network block-id slot-key]
  (some->> (get (obj/accessor-declarations-for network block-id) slot-key)
           keys
           (sort-by pr-str)
           first))

(defn- instance-block-head-id
  [network instance-id]
  (let [instance-value (net/network-cell-strongest network instance-id)
        instance-id (if (ids/node-id? instance-value)
                      instance-value
                      instance-id)]
    (when-let [blocks-id (declared-slot-parent-id network
                                                  instance-id
                                                  :instance/blocks)]
      (net/network-cell-strongest network blocks-id))))

(defn- block-at-slot-id
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

(defn- block-at-text-id
  [network instance-id index-id]
  (block-at-slot-id network instance-id index-id :block/text))

(defn- block-at-display-id
  [network instance-id index-id]
  (block-at-slot-id network instance-id index-id :block/display))

(declare p:tui-write-request tui-write-effect-request)

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

(defn- tui-write-effect-request
  [effect-id text-id payload epoch]
  (boundary/tui-write-effect-request effect-id text-id payload epoch))

(defn- tui-display-effect-request
  [effect-id display-id payload tick]
  (boundary/tui-display-effect-request effect-id display-id payload tick))

(defn- p:tui-write-request
  [source-id text-id outbox-id]
  (prop/construct-propagator
   (fn [_inputs _outputs network]
     (let [payload (net/network-cell-strongest network source-id)
           epoch (or (:program/epoch (net/net-dict-or-empty network)) 0)
           effect-id [:tui/write-block text-id epoch (hash payload)]]
       (if (value/nothing? payload)
         []
         [(message outbox-id
                   (obj/compound-object
                    {(effect-slot-key effect-id)
                     (tui-write-effect-request effect-id
                                               text-id
                                               payload
                                               epoch)}))])))
   [source-id]
   [outbox-id]))

(defn- p:tui-display-request
  [source-id display-id outbox-id]
  (prop/construct-propagator
   (fn [_inputs _outputs network]
     (let [payload (net/network-cell-strongest network source-id)
           dict (net/net-dict-or-empty network)
           tick (or (:runtime/commit-tick dict)
                    (:program/epoch dict)
                    0)
           effect-id [:tui/write-display display-id tick (hash payload)]]
       (if (value/nothing? payload)
         []
         [(message outbox-id
                   (obj/compound-object
                    {(effect-slot-key effect-id)
                     (tui-display-effect-request effect-id
                                                 display-id
                                                 payload
                                                 tick)}))])))
   [source-id]
   [outbox-id]))

(defn- trace-target-value
  [label source-id]
  {:trace/target true
   :trace/symbol label
   :node source-id
   :label label})

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

(def ^:private jp->en
  {"猫" "cat"
   "犬" "dog"
   "水" "water"})

(def ^:private en->jp
  (into {} (map (fn [[jp en]] [en jp]) jp->en)))

(defn- translate-messages
  [network jp-id en-id]
  (let [jp (net/network-cell-strongest network jp-id)
        en (net/network-cell-strongest network en-id)
        en' (when (string? jp) (get jp->en jp))
        jp' (when (string? en) (get en->jp en))]
    (cond-> []
      en' (conj (message en-id en'))
      jp' (conj (message jp-id jp')))))

(defn translate-operator []
  (with-meta
    (fn [network arg-ids out-id]
      (let [[jp-id en-id] (vec arg-ids)]
        (when-not (and jp-id en-id (= 2 (count arg-ids)))
          (throw (ex-info "translate expects Japanese and English cells"
                          {:arg-ids arg-ids})))
        (let [[prop-id n]
              ((prop/construct-propagator
                (prop/concrete-propagator
                 (fn [_inputs _outputs current-net]
                   (translate-messages current-net jp-id en-id)))
                [jp-id en-id]
                [jp-id en-id])
               network)]
          [n [prop-id] (or out-id en-id)])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (second (vec arg-ids)) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids _out-id]
       (let [[jp-id en-id] (vec arg-ids)]
         (when-not (and jp-id en-id (= 2 (count arg-ids)))
           (throw (ex-info "translate expects Japanese and English cells"
                           {:arg-ids arg-ids})))
         (translate-messages current-net jp-id en-id)))}))
