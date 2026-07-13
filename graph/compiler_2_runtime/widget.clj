(ns graph.compiler-2-runtime.widget
  "Compiler-2 runtime widget IO operators for XR/browser projections."
  (:require [graph.compiler-2-runtime.ids :as runtime-ids]
            [propagators.cells.value :as value]
            [propagators.compiler-2.language.ast :as ast]
            [propagators.compiler-2.compiler.dispatch :as compiler-dispatch]
            [propagators.compiler-2.model.env :as cenv]
            [propagators.compiler-2.compiler.basis :as compiler-helpers]
            [propagators.compiler-2.model.operator-value :as operator-value]
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

(defn- compile-form
  [state form role]
  (let [compile* (compiler-dispatch/state-compiler state)]
    (let [[state' binding] (compile* (compiler-helpers/child state role) form)]
      [(assoc state' :path (:path state)) binding])))

(defn- ast-symbol-name
  [form]
  (when (= :symbol (ast/type form))
    (name (ast/name form))))

(defn- ast-symbol
  [form]
  (when (= :symbol (ast/type form))
    (ast/name form)))

(defn- ast-literal-value
  [form]
  (when (= :literal (ast/type form))
    (ast/value form)))

(defn- add-literal-cell
  [state role v]
  (compiler-helpers/new-cell state role v))

(defn- install-widget-registration
  [state outbox-id out-id return-id widget-type widget-id-id channels]
  (let [input-ids (vec (distinct
                        (concat [widget-id-id]
                                (mapcat (juxt :widget/view-cell
                                               :widget/event-cell)
                                        channels))))
        network0 (reduce nb/ensure-cell
                         (:net state)
                         (concat [outbox-id out-id return-id]
                                 input-ids))
        [prop-id network']
        ((prop/construct-propagator
          (fn [_inputs _outputs network]
            (register-messages network
                               outbox-id
                               out-id
                               widget-type
                               widget-id-id
                               channels))
          input-ids
          [out-id])
         network0)]
    [(-> state
         (assoc :net network')
         (compiler-helpers/add-props [prop-id]))
     (cenv/cell-binding return-id)]))

(defn- io-slider-plan
  [operand-forms]
  (let [args (vec operand-forms)]
    (case (count args)
      1 {:widget-label (or (ast-symbol-name (first args))
                           "slider")
         :cell-form (first args)}
      2 {:widget-label (or (some-> (ast-literal-value (first args)) str)
                           (ast-symbol-name (first args))
                           "slider")
         :cell-form (second args)}
      (throw (ex-info "io:slider expects value-cell or widget-id plus value-cell"
                      {:operand-forms operand-forms})))))

(defn io-slider-operator
  [outbox-id]
  (operator-value/operator-closure
   {:name 'io:slider
    :direct-installer
    (fn [state operand-forms out-id]
      (let [{:keys [widget-label cell-form]} (io-slider-plan operand-forms)
            [state' widget-binding] (add-literal-cell state
                                                      [:io-slider widget-label :id]
                                                      widget-label)
            [state'' cell-binding] (compile-form state' cell-form :io-slider-cell)
            cell-id (cenv/binding-id cell-binding)]
        (when-not cell-id
          (throw (ex-info "io:slider value expression must compile to a cell"
                          {:cell-form cell-form
                           :binding cell-binding})))
        (install-widget-registration state''
                                     outbox-id
                                     out-id
                                     cell-id
                                     :slider
                                     (cenv/binding-id widget-binding)
                                     [{:widget/channel "value"
                                       :widget/view-cell cell-id
                                       :widget/event-cell cell-id}])))}))

(defn- require-slider-panel-cells
  [name cell-forms]
  (when-not (seq cell-forms)
    (throw (ex-info (str name " expects at least one cell")
                    {:cell-forms cell-forms})))
  cell-forms)

(defn- io-slider-panel-plan
  [operand-forms named?]
  (let [args (vec operand-forms)]
    (if named?
      (do
        (when (< (count args) 2)
          (throw (ex-info "io:slider-panel-name expects panel-id and cells"
                          {:operand-forms operand-forms})))
        {:panel-label (or (some-> (ast-literal-value (first args)) str)
                          (ast-symbol-name (first args))
                          "slider-panel-0")
         :cell-forms (require-slider-panel-cells
                      "io:slider-panel-name"
                      (subvec args 1))})
      {:panel-label "slider-panel-0"
       :cell-forms (require-slider-panel-cells "io:slider-panel" args)})))

(defn- suffix-symbol
  [sym suffix]
  (symbol (namespace sym) (str (name sym) suffix)))

(defn- event-source-id
  [state cell-form fallback-id]
  (or (when-let [sym (ast-symbol cell-form)]
        (some-> (cenv/lookup (:env state) (suffix-symbol sym "-events"))
                cenv/binding-id))
      fallback-id))

(defn- io-slider-panel-operator*
  [outbox-id named?]
  (operator-value/operator-closure
   {:name (if named? 'io:slider-panel-name 'io:slider-panel)
    :direct-installer
    (fn [state operand-forms out-id]
      (let [{:keys [panel-label cell-forms]} (io-slider-panel-plan operand-forms
                                                                    named?)
            [state' widget-binding] (add-literal-cell state
                                                      [:io-slider-panel panel-label :id]
                                                      panel-label)
            [state'' channels]
            (reduce
             (fn [[state channels] [idx cell-form]]
               (let [channel-name (or (ast-symbol-name cell-form)
                                      (str "value" idx))
                     [state' cell-binding] (compile-form state
                                                         cell-form
                                                         [:io-slider-panel-cell idx])
                     cell-id (cenv/binding-id cell-binding)]
                 (when-not cell-id
                   (throw (ex-info "io:slider-panel cell expression must compile to a cell"
                                   {:cell-form cell-form
                                    :binding cell-binding})))
                 [state'
                  (conj channels
                        {:widget/channel channel-name
                         :widget/view-cell cell-id
                         :widget/event-cell (event-source-id state'
                                                             cell-form
                                                             cell-id)})]))
             [state' []]
             (map-indexed vector cell-forms))]
        (install-widget-registration state''
                                     outbox-id
                                     out-id
                                     out-id
                                     :slider-panel
                                     (cenv/binding-id widget-binding)
                                     channels)))}))

(defn io-slider-panel-operator
  [outbox-id]
  (io-slider-panel-operator* outbox-id false))

(defn io-slider-panel-name-operator
  [outbox-id]
  (io-slider-panel-operator* outbox-id true))

(defn slider-io-operator
  [outbox-id]
  (operator-value/operator-closure
   {:name 'slider-io
    :output-selector (fn [arg-ids fallback-id]
                       (or (nth (vec arg-ids) 3 nil) fallback-id))
    :activate (fn [network _context-id arg-ids fallback-id]
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
  (operator-value/operator-closure
   {:name 'slider-panel-io
    :output-selector (fn [arg-ids fallback-id]
                       (let [arg-ids (vec arg-ids)]
                         (if (= 2 (mod (count arg-ids) 3))
                           (peek arg-ids)
                           fallback-id)))
    :activate (fn [network _context-id arg-ids fallback-id]
                (let [arg-ids (vec arg-ids)
                      explicit-out-id (when (= 2 (mod (count arg-ids) 3))
                                        (peek arg-ids))
                      widget-id-id (first arg-ids)
                      channel-args (subvec arg-ids 1 (- (count arg-ids)
                                                         (if explicit-out-id 1 0)))
                      out-id (or explicit-out-id fallback-id)
                      raw-channels (panel-channels channel-args)
                      channels (vec (keep #(resolve-panel-channel network %)
                                           raw-channels))]
                  (when-not (and widget-id-id out-id
                                 (pos? (count channel-args))
                                 (zero? (mod (count channel-args) 3)))
                    (throw (ex-info "slider-panel-io expects panel-id plus channel/view/event triples and optional output"
                                    {:arg-ids arg-ids})))
                  (if (not= (count channels) (count raw-channels))
                    []
                    (register-messages network
                                       outbox-id
                                       out-id
                                       :slider-panel
                                       widget-id-id
                                       channels))))}))
