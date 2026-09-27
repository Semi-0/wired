(ns propagators.tui.experimental.visualization-composition-test
  (:require [clojure.test :refer [deftest is testing]]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.model.env :as env]
            [propagators.runtime.session.file-loader :as loader]
            [propagators.infra.core :as core]
            [propagators.infra.datastructures.dependency :as dependency]
            [propagators.infra.datastructures.tms.core :as tms]
            [propagators.runtime.experimental.visualization.layered-primitives :as primitives]
            [propagators.infra.ids :as ids]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]
            [propagators.infra.stdlib.arithmetic.base :as base]))

(def item-source [[:outer] :item])
(def captured-source [[:outer] :captured])
(def item-support :support/item)
(def captured-support :support/captured)

(defn session [source]
  (loader/load-session-from-source
   source {:extensions [primitives/session-extension]}))

(defn binding-id [state symbol]
  (env/resolve-binding-id (:program/net state) (:program/env state) symbol))

(defn seed [state symbol value]
  (let [id (binding-id state symbol)
        [tasks patched] (core/eval-cell id (message id value) (:program/net state))]
    (assoc state :program/net (nb/run-propagators patched tasks))))

(defn observe [state symbol]
  (let [network (:program/net state)
        id (binding-id state symbol)
        strongest (net/network-cell-strongest network id)
        payload (tms/distributed-base-value strongest)]
    {:value (dependency/unwrap payload)
     :sources (dependency/sources payload)
     :supports (set (map tms/support-source
                         (tms/distributed-supports (net/network-cell-content network id))))}))

(defn callback-source [execution]
  (case execution
    :direct "(def-cells seed captured out) (-> (tracked+ seed captured) out)"
    :compiler-direct "(def-cells seed captured out) (-> (+ seed captured) out)"
    :higher-order (slurp "examples/lain/visualization_combinators/tracked_callback.lain")
    :chained
    "(def-cells seed captured out)
     (def-net transform [item] [out]
       (-> (tracked+ (tracked+ item captured) captured) out))
     (def-net apply-transform [f item] [out] (f item out))
     (apply-transform transform seed out)"
    (throw (ex-info "Unknown probe execution" {:execution execution}))))

(defn run-callback
  ([item captured] (run-callback item captured :higher-order))
  ([item captured execution]
   (-> @(session (callback-source execution))
       (seed 'seed item)
       (seed 'captured captured)
       (observe 'out))))

(defn tracked-inputs []
  [(tms/distributed-input-update
    :item-claim (dependency/dependency-value 4 #{item-source})
    :item-premise 0 item-support)
   (tms/distributed-input-update
    :captured-claim (dependency/dependency-value 3 #{captured-source})
    :captured-premise 0 captured-support)])

(deftest higher-order-plain-and-layered-inputs
  (is (= {:value 7 :sources #{} :supports #{}} (run-callback 4 3)))
  (let [result (run-callback (dependency/dependency-value 4 #{item-source})
                             (dependency/dependency-value 3 #{captured-source}))]
    (is (= 7 (:value result)))
    (is (= #{item-source captured-source} (:sources result)))))

(deftest higher-order-network-preserves-tms-supports
  (let [result (run-callback
                (tms/distributed-input-update :item-claim 4 :item-premise 0 item-support)
                (tms/distributed-input-update :captured-claim 3 :captured-premise 0 captured-support))]
    (is (= 7 (:value result)))
    (is (= #{item-support captured-support} (:supports result)))))

(deftest network-preserves-dependencies-inside-tms
  (let [[item captured] (tracked-inputs)]
    (doseq [[execution expected] [[:direct 7] [:higher-order 7] [:chained 10]]]
      (testing (name execution)
        (let [result (run-callback item captured execution)]
          (is (= expected (:value result)))
          (is (= #{item-source captured-source} (:sources result)))
          (is (= #{item-support captured-support} (:supports result))))))))

(deftest compiler-baseline-remains-unchanged
  (let [[item captured] (tracked-inputs)
        result (run-callback item captured :compiler-direct)]
    (is (= 7 (:value result)))
    ;; Characterize the old limitation; the extension does not replace +.
    (is (= #{} (:sources result)))
    (is (= #{item-support captured-support} (:supports result)))))

(deftest late-inputs-and-repeat-execution
  (let [[item captured] (tracked-inputs)
        initial @(session (callback-source :higher-order))
        pending (seed initial 'seed item)
        complete (seed pending 'captured captured)
        network (:program/net complete)
        propagators (keep (fn [[id entry]] (when (prop/prop? entry) id))
                          (net/net-env network))
        repeated (assoc complete :program/net (nb/run-propagators network propagators))]
    (is (value/nothing? (:value (observe pending 'out))))
    (is (= 7 (:value (observe complete 'out))))
    (is (= (observe complete 'out) (observe repeated 'out)))
    (is (= (set (keys (net/net-env network)))
           (set (keys (net/net-env (:program/net repeated))))))
    (is (= item (net/network-cell-content (:program/net repeated) (binding-id complete 'seed))))
    (is (= captured (net/network-cell-content (:program/net repeated)
                                             (binding-id complete 'captured))))))

(deftest late-callback-definition
  (let [[item captured] (tracked-inputs)
        running (session
                 "(def-cells seed captured callback out)
                  (def-net apply-transform [f item] [out] (f item out))
                  (apply-transform callback seed out)")]
    (swap! running #(-> % (seed 'seed item) (seed 'captured captured)))
    (is (value/nothing? (:value (observe @running 'out))))
    (loader/load-source! running
                        "(-> (network [item] [out] (-> (tracked+ item captured) out)) callback)")
    (is (= {:value 7 :sources #{item-source captured-source}
            :supports #{item-support captured-support}}
           (observe @running 'out)))))

(deftest unusable-inputs-are-not-computed
  (is (value/nothing? (:value (run-callback value/nothing 3))))
  ;; Compiler forward sync does not forward raw unusable strongest values.
  ;; This is not evidence of end-to-end contradictory-predicate transport.
  (is (value/nothing? (:value (run-callback value/contradiction 3)))))

(deftest concrete-primitive-boundary-blocks-contradiction
  (let [[left right output] (repeatedly 3 ids/new-node-id)
        prepared (nb/install-cells [left right output])
        [installed tasks _] (primitives/install-call prepared [left right] output base/plus-closure)
        initial (nb/run-propagators installed tasks)
        [left-tasks left-net] (core/eval-cell left (message left value/contradiction) initial)
        after-left (nb/run-propagators left-net left-tasks)
        [right-tasks right-net] (core/eval-cell right (message right 3) after-left)
        complete (nb/run-propagators right-net right-tasks)]
    (is (value/nothing? (net/network-cell-strongest complete output)))))

(deftest equal-values-keep-path-distinct-origins
  (let [left [[:outer [:cell :left]] :same]
        right [[:outer [:cell :right]] :same]
        result (run-callback (dependency/dependency-value 3 #{left})
                             (dependency/dependency-value 3 #{right}))]
    (is (= 6 (:value result)))
    (is (= #{left right} (:sources result)))))

(deftest tms-conflicts-are-not-reinterpreted-as-refinements
  (let [[item captured] (tracked-inputs)
        complete (-> @(session (callback-source :higher-order))
                     (seed 'seed item) (seed 'captured captured))
        refined (seed complete 'seed
                      (tms/distributed-input-update
                       :more-evidence (dependency/dependency-value 4 #{:extra-origin})
                       :extra-premise 0 :extra-support))
        source (observe refined 'seed)]
    ;; Under existing TMS policy these differently layered values disagree,
    ;; even though both arithmetic bases are 4. Do not hide that contradiction.
    (is (value/contradiction? (:value source)))
    (is (= #{item-support :extra-support} (:supports source)))))

(deftest invalid-primitive-arity-is-explicit
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"two arguments"
       (primitives/install-call net/empty-net [] (ids/new-node-id) base/plus-closure))))
