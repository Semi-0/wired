(ns propagators.tui.adapters.bridge.widget
  "Compiler-2 runtime widget IO operators for XR/browser projections."
  (:require [propagators.runtime.ids :as runtime-ids]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.language.ast :as ast]
            [propagators.compiler.common.cps :as cps]
            [propagators.compiler.model.env :as cenv]
            [propagators.compiler.compiler.basis :as compiler-helpers]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]))

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

(defn slider-compiler-operands
  [outbox-id compile-k state operand-forms result-id continuation]
  (let [{:keys [widget-label cell-form]} (io-slider-plan operand-forms)
        [declared widget-binding]
        (add-literal-cell state [:io-slider widget-label :id] widget-label)]
    (cps/call
     compile-k (compiler-helpers/child declared :io-slider-cell) cell-form
     (fn [compiled cell-binding]
       (let [cell-id (cenv/binding-id cell-binding)]
         (if cell-id
           (let [[installed result]
                 (install-widget-registration
                  (assoc compiled :path (:path state)) outbox-id result-id cell-id
                  :slider (cenv/binding-id widget-binding)
                  [{:widget/channel "value" :widget/view-cell cell-id
                    :widget/event-cell cell-id}])]
             (cps/continue continuation installed result))
           (throw (ex-info "io:slider value expression must compile to a cell"
                           {:cell-form cell-form :binding cell-binding}))))))))

(defn io-slider-operator [outbox-id]
  (operator-value/operator-closure
   {:name 'io:slider
    :compiler-operands (partial slider-compiler-operands outbox-id)}))

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
        (cenv/resolve-binding-id (:net state)
                                 (:env state)
                                 (suffix-symbol sym "-events")))
      fallback-id))

(defn- widget-channel [state position form binding]
  (let [cell-id (cenv/binding-id binding)]
    (if cell-id
      {:widget/channel (or (ast-symbol-name form) (str "value" position))
       :widget/view-cell cell-id
       :widget/event-cell (event-source-id state form cell-id)}
      (throw (ex-info "io:slider-panel cell expression must compile to a cell"
                      {:cell-form form :binding binding})))))

(defn slider-panel-compiler-operands
  [outbox-id named? compile-k state operand-forms result-id continuation]
  (let [{:keys [panel-label cell-forms]} (io-slider-panel-plan operand-forms named?)
        [declared widget-binding]
        (add-literal-cell state [:io-slider-panel panel-label :id] panel-label)]
    (cps/compile-args
     compile-k declared cell-forms
     (fn [compiled bindings]
       (let [channels (mapv (partial widget-channel compiled)
                             (range) cell-forms bindings)
             [installed result]
             (install-widget-registration compiled outbox-id result-id result-id
                                          :slider-panel (cenv/binding-id widget-binding)
                                          channels)]
         (cps/continue continuation installed result))))))

(defn- io-slider-panel-operator* [outbox-id named?]
  (operator-value/operator-closure
   {:name (if named? 'io:slider-panel-name 'io:slider-panel)
    :compiler-operands (partial slider-panel-compiler-operands outbox-id named?)}))

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
