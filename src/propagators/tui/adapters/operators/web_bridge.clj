(ns propagators.tui.adapters.operators.web-bridge
  "Compiler-2 primitive bridge operators for independent web clients."
  (:require [propagators.runtime.session.state :as state]
            [propagators.tui.adapters.bridge.web :as bridge]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.common.cps :as cps]
            [propagators.compiler.language.ast :as ast]
            [propagators.compiler.model.env :as cenv]
            [propagators.compiler.compiler.basis :as h]
            [propagators.compiler.model.operator-value :as operator-value]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]))

(defn- strongest
  [network id]
  (if (and id (contains? (net/net-env network) id))
    (net/network-cell-strongest network id)
    value/nothing))

(defn- add-prop
  [state inputs outputs activate prop-key]
  (let [prop-id (apply state/stable-node-id :web :bridge prop-key)
        n0 (reduce nb/ensure-cell (:net state) (concat inputs outputs))
        [installed-id n1] ((prop/construct-propagator prop-id
                                                      activate
                                                      inputs
                                                      outputs)
                           n0)]
    [(-> state
         (assoc :net n1)
         (h/add-props [installed-id]))
     installed-id]))

(defn- base-value
  [v]
  (cond
    (event/event-projection? v)
    (let [facts (event/projection-facts v)]
      (if (= 1 (count facts))
        (event/event-value (first facts))
        v))

    :else
    v))

(defn- install-static-relation
  [state from-id to-id prop-key]
  (first
   (add-prop state
             [from-id]
             [to-id]
             (fn [_inputs _outputs network]
               (let [from (strongest network from-id)]
                 (cond-> []
                   (not (value/unusable? from))
                   (conj (message to-id from)))))
             prop-key)))

(defn runtime-clients-compiler-operands
  [compile-k state operand-forms result-id continuation]
  (let [forms (vec operand-forms)]
    (if (= 1 (count forms))
      (cps/call
       compile-k (h/child state :runtime-clients) (first forms)
       (fn [compiled target-binding]
         (let [target-id (or (cenv/binding-id target-binding) result-id)
               source-id (bridge/client-list-source-id)]
           (cps/continue continuation
                         (install-static-relation
                          (assoc compiled :path (:path state)) source-id target-id
                          [:clients source-id target-id])
                         (cenv/cell-binding target-id)))))
      (throw (ex-info "runtime:clients expects one target cell"
                      {:operand-forms forms})))))

(defn runtime-clients-operator []
  (operator-value/operator-closure
   {:name 'runtime:clients :compiler-operands runtime-clients-compiler-operands}))

(defn- declare-runtime-client-pipe [state from-id to-id pipe-id target-id]
  (first
   (add-prop state [from-id to-id pipe-id] [target-id]
             (fn [_ _ network]
               (let [from (base-value (strongest network from-id))
                     to (base-value (strongest network to-id))
                     pipe (base-value (strongest network pipe-id))]
                 (if (some value/unusable? [from to pipe])
                   []
                   [(message target-id (bridge/client-handle to))])))
             [:client-pipe from-id to-id pipe-id target-id])))

(defn runtime-client-pipe-compiler-operands
  [compile-k state operand-forms result-id continuation]
  (let [forms (vec operand-forms)
        arity (count forms)]
    (if (contains? #{2 3 4} arity)
      (cps/compile-args
       compile-k state
       (if (= 2 arity) (conj forms (ast/lit bridge/default-pipe)) forms)
       (fn [compiled bindings]
         (let [[from-id to-id pipe-id explicit-output]
               (mapv cenv/binding-id bindings)
               target-id (or explicit-output result-id)]
           (cps/continue continuation
                         (declare-runtime-client-pipe compiled from-id to-id pipe-id target-id)
                         (cenv/cell-binding target-id)))))
      (throw (ex-info "runtime:client-pipe expects from-client, to-client, optional pipe, and optional output"
                      {:operand-forms forms})))))

(defn runtime-client-pipe-operator []
  (operator-value/operator-closure
   {:name 'runtime:client-pipe :compiler-operands runtime-client-pipe-compiler-operands}))
