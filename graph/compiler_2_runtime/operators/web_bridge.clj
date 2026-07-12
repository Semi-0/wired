(ns graph.compiler-2-runtime.operators.web-bridge
  "Compiler-2 primitive bridge operators for independent web clients."
  (:require [graph.compiler-2-runtime.state :as state]
            [graph.compiler-2-runtime.web-bridge :as bridge]
            [propagators.cells.value :as value]
            [propagators.compiler-2.env :as cenv]
            [propagators.compiler-2.helpers :as h]
            [propagators.compiler-2.operator-value :as operator-value]
            [propagators.datastructures.event :as event]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]))

(defn- strongest
  [network id]
  (if (and id (contains? (net/net-env network) id))
    (net/network-cell-strongest network id)
    value/nothing))

(defn- compile-form
  [state form role]
  (let [compile* (requiring-resolve 'propagators.compiler-2.core/g:compile)
        [state' binding] (compile* form
                                   (:env state)
                                   (h/child state role))]
    [(assoc state' :path (:path state)) binding]))

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

(defn- one-arg-target
  [state operand-forms fallback-id role]
  (let [[form] (vec operand-forms)
        [state' binding] (compile-form state form role)
        target-id (or (cenv/binding-id binding) fallback-id)]
    (when-not (and target-id (= 1 (count operand-forms)))
      (throw (ex-info "expected one target cell"
                      {:operand-forms operand-forms})))
    [state' target-id]))

(defn runtime-clients-operator []
  (operator-value/operator-closure
   {:name 'runtime:clients
    :direct-installer
    (fn [state operand-forms out-id]
      (let [[state' target-id] (one-arg-target state
                                               operand-forms
                                               out-id
                                               :runtime-clients)
            source-id (bridge/client-list-source-id)]
        [(install-static-relation state'
                                  source-id
                                  target-id
                                  [:clients source-id target-id])
         (cenv/cell-binding target-id)]))}))

(defn- compile-route-output
  [state operand-forms fallback-id]
  (let [forms (vec operand-forms)]
    (if-let [out-form (nth forms 3 nil)]
      (let [[state' binding] (compile-form state out-form :runtime-client-pipe-out)]
        [state' (cenv/binding-id binding)])
      [state fallback-id])))

(defn- route-messages
  [from to pipe out-id]
  [(message out-id (bridge/client-handle to))])

(defn runtime-client-pipe-operator []
  (operator-value/operator-closure
   {:name 'runtime:client-pipe
    :direct-installer
    (fn [state operand-forms out-id]
      (let [forms (vec operand-forms)]
        (when-not (#{2 3 4} (count forms))
          (throw (ex-info "runtime:client-pipe expects from-client, to-client, optional pipe, and optional output"
                          {:operand-forms operand-forms})))
        (let [[state1 from-binding] (compile-form state (nth forms 0) :runtime-client-pipe-from)
              [state2 to-binding] (compile-form state1 (nth forms 1) :runtime-client-pipe-to)
              [state3 pipe-binding] (if-let [pipe-form (nth forms 2 nil)]
                                      (compile-form state2 pipe-form :runtime-client-pipe-name)
                                      (compile-form state2 bridge/default-pipe :runtime-client-pipe-name))
              [state4 target-id] (compile-route-output state3 forms out-id)
              from-id (cenv/binding-id from-binding)
              to-id (cenv/binding-id to-binding)
              pipe-id (cenv/binding-id pipe-binding)]
          [(first (add-prop state4
                            [from-id to-id pipe-id]
                            [target-id]
                            (fn [_inputs _outputs network]
                              (let [from (base-value (strongest network from-id))
                                    to (base-value (strongest network to-id))
                                    pipe (base-value (strongest network pipe-id))]
                                (if (or (value/unusable? from)
                                        (value/unusable? to)
                                        (value/unusable? pipe))
                                  []
                                  (route-messages from to pipe target-id))))
                            [:client-pipe from-id to-id pipe-id target-id]))
           (cenv/cell-binding target-id)])))}))
