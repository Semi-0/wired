(ns propagators.tui.experimental.view-collections-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.set :as set]
            [propagators.infra.cells.value :as value]
            [propagators.infra.core :as core]
            [propagators.infra.message :refer [message]]
            [propagators.infra.ids :as ids]
            [propagators.compiler.lowering.application :as application]
            [propagators.runtime.session.file-loader :as loader]
            [propagators.compiler.model.env :as env]
            [propagators.infra.experimental.visualization.data :as data]
            [propagators.tui.adapters.extensions.visualization :as extension]
            [propagators.runtime.experimental.visualization.layered-primitives :as primitives]
            [propagators.compiler.experimental.visualization.observation :as observation]
            [propagators.tui.experimental.visualization-composition-test :as fixture]
            [propagators.infra.datastructures.dependency :as dependency]
            [propagators.infra.datastructures.tms.core :as tms]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]
            [propagators.infra.semantic-trace :as trace]))

(defn run-source [source]
  @(loader/load-session-from-source source {:extensions [primitives/session-extension extension/extension]}))

(defn result [state name]
  (let [network (:program/net state)
        id (env/resolve-binding-id network (:program/env state) name)]
    (data/resolve-collection network (net/network-cell-strongest network id))))

(deftest nested-map-over-lain-list
  (let [state (run-source "(def increment (:: [x] (tracked+ x 1)))
                           (def result (map increment (list 2 3 4)))")]
    (is (= [3 4 5] (mapv :payload (:items (result state 'result)))))))

(deftest selectable-view-remains-collection-data
  (let [state (run-source "(def-cells selection)
                          (def result (map (:: [x] (tracked+ x 1))
                            (selectable (map (:: [x] x) (list 2)) selection)))")]
    (is (= [3] (mapv :payload (:items (result state 'result)))))))

(deftest explicit-output-network-callback
  (let [state (run-source "(def-net increment [x] [out] (-> (tracked+ x 1) out))
                          (def-net accept [x] [out] (-> true out))
                          (def result (map increment (filter accept (list 2 3))))")]
    (is (= [3 4] (mapv :payload (:items (result state 'result)))))))

(deftest nested-filter-and-map
  (let [state (run-source "(def accepted (:: [x] true))
                           (def increment (:: [x] (tracked+ x 1)))
                           (def result (map increment (filter accepted (list 2 3))))")]
    (is (= [3 4] (mapv :payload (:items (result state 'result)))))))

(deftest partial-list-keeps-order-and-origins
  (let [initial (run-source "(def-cells later)
                            (def identity (:: [x] x))
                            (def result (map identity (list 2 later 2)))")
        before (result initial 'result)
        after (result (fixture/seed initial 'later 7) 'result)]
    (is (= [2 2] (mapv :payload (:items before))))
    (is (= [2 7 2] (mapv :payload (:items after))))
    (is (= 3 (count (set (map :identity (:items after))))))
    (is (= 3 (count (set (map :sources (:items after))))))))

(deftest false-and-unavailable-predicates-do-not-include
  (doseq [predicate ["false" "missing"]]
    (let [state (run-source (str "(def-cells missing) (def predicate (:: [x] " predicate "))"
                                 "(def result (filter predicate (list 1 2)))"))
          projection (result state 'result)]
      (is (empty? (:items projection)))
      (is (every? #{(if (= predicate "false") :excluded :pending)}
                  (map :membership (:candidates projection))))
      (is (= 2 (count (:candidates projection)))))))

(deftest constant-callback-keeps-argument-sources
  (let [state (run-source "(def constant (:: [x] 9)) (def result (map constant (list 1 1)))")
        projection (result state 'result)
        origins (map #(dependency/sources (tms/distributed-base-value (:content %))) (:items projection))]
    (is (= [9 9] (mapv :payload (:items projection))))
    (is (every? seq origins))
    (is (= 2 (count (set origins))))))

(deftest late-callback-and-reactivation-have-stable-topology
  (let [session (loader/load-session-from-source
                 "(def-cells callback) (def result (map callback (list 1 2)))"
                 {:extensions [primitives/session-extension extension/extension]})]
    (is (empty? (:items (result @session 'result))))
    (loader/load-source! session "(-> (:: [x] (tracked+ x 2)) callback)")
    (let [state @session
          network (:program/net state)
          ids (keep (fn [[id entry]] (when (prop/prop? entry) id)) (net/net-env network))
          rerun (assoc state :program/net (nb/run-propagators network ids))]
      (is (= [3 4] (mapv :payload (:items (result state 'result)))))
      (is (= (set (keys (net/net-env network)))
             (set (keys (net/net-env (:program/net rerun))))))
      (is (= (result state 'result) (result rerun 'result))))))

(defn graph-fixture []
  (let [state (run-source
               "(def-cells input output selected graph)
                (def-net chain [x] [out] (-> (tracked+ x 1) out))
                (chain input output)
                (def boundary? (:: [item]
                  (tracked-or (member? item (inputs-of selected))
                              (member? item (outputs-of selected)))))
                (def boundary (filter boundary? graph))
                (def values (map (:: [item] (strongest-of item)) (transpose boundary :list)))")
        input (fixture/binding-id state 'input)
        output (fixture/binding-id state 'output)
        network (:program/net state)
        a (first (filter #(= [input output] (:argument-ids %))
                         (application/application-topologies network)))]
    (when-not a (throw (ex-info "Test network application not found" {})))
    (let [prop-id (observation/application-propagator a)
          keys (mapv #(vector [:outer] %) [input prop-id output])
          graph (trace/graph-union {:nodes (zipmap keys ["input" "chain" "output"])
                                   :edges (mapv vec (partition 2 1 keys))})]
      (-> state
          (fixture/seed 'selected (data/reference prop-id))
          (fixture/seed 'graph graph)
          (fixture/seed 'input 5)))))

(deftest declared-interface-filter-and-transpose
  (let [state (graph-fixture)
        boundary (result state 'boundary)
        values (result state 'values)]
    (is (= 2 (count (:items boundary))))
    (is (empty? (:edges boundary)))
    (is (= #{5 6} (set (map :payload (:items values)))))
    (is (= (mapv :identity (:items boundary)) (mapv :identity (:items values))))
    (is (= :list (:kind values)))))

(deftest headless-focus-follow-sources-and-transform-again
  (let [state (run-source "(def-cells selection)
                          (def values (map (:: [x] (tracked+ x 1)) (list 2 3)))
                          (def zoom (map (:: [ref] (strongest-of ref))
                                         (sources-of (focus values selection))))")
        entry (first (:items (result state 'values)))
        focused (fixture/seed state 'selection (:identity entry))]
    (is (empty? (:items (result state 'zoom))))
    (is (= [2] (mapv :payload (:items (result focused 'zoom)))))
    (is (= [3 4] (mapv :payload (:items (result focused 'values)))))))

(deftest empty-list-has-no-candidates
  (let [state (run-source "(def result (map (:: [x] x) (list)))")]
    (is (empty? (:candidates (result state 'result))))))

(deftest lexical-callback-retains-both-supports
  (let [[item captured] (fixture/tracked-inputs)
        state (-> (run-source "(def-cells seed captured)
                               (def result (map (:: [x] (tracked+ x captured)) (list seed)))")
                  (fixture/seed 'seed item)
                  (fixture/seed 'captured captured))
        row (first (:items (result state 'result)))
        content (:content row)]
    (is (= 7 (:payload row)))
    (is (set/subset? #{fixture/item-source fixture/captured-source}
                    (dependency/sources (data/evidence-value content))))
    (is (= #{fixture/item-support fixture/captured-support}
           (set (map tms/support-source (tms/distributed-supports content)))))
    (is (= item (net/network-cell-content (:program/net state) (fixture/binding-id state 'seed))))))

(deftest predicate-supports-survive-another-map
  (let [state (-> (run-source "(def-cells accepted)
                               (def result (map (:: [x] x)
                                 (filter (:: [x] accepted) (list 4))))")
                  (fixture/seed 'accepted
                    (tms/distributed-input-update :accepted-claim
                      (dependency/dependency-value true #{:predicate-origin})
                      :accepted-premise 0 :predicate-support)))
        row (first (:items (result state 'result)))
        content (:content row)]
    (is (= 4 (:payload row)))
    (is (contains? (dependency/sources (data/evidence-value content)) :predicate-origin))
    (is (contains? (set (map tms/support-source (tms/distributed-supports content))) :predicate-support))))

(deftest concrete-predicate-does-not-publish-unusable-values
  (let [state (run-source "(def-cells accepted)
                          (def result (filter (:: [x] accepted) (list 4)))")
        blocked (fixture/seed state 'accepted value/contradiction)
        accepted (fixture/seed state 'accepted true)
        conflicting (fixture/seed accepted 'accepted false)]
    (is (= [:pending] (mapv :membership (:candidates (result blocked 'result)))))
    ;; A later conflict blocks activation; it does not invalidate old output.
    (is (= [4] (mapv :payload (:items (result conflicting 'result)))))))

(deftest invalid-predicate-is-not-treated-as-false
  (let [state (run-source "(def result (filter (:: [x] 42) (list 4)))")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Boolean" (result state 'result)))))

(deftest nested-reference-watches-only-its-outer-owner
  (let [[item left right] (repeatedly 3 ids/new-node-id)
        child (nb/install-cells [item])
        [_ child] (core/eval-cell item (message item 9) child)
        outer (nb/install-cells [left right])
        [_ outer] (core/eval-cell left (message left child) outer)
        [_ outer] (core/eval-cell right (message right child) outer)
        a (data/reference [:outer [:cell left]] item [])
        b (data/reference [:outer [:cell right]] item [])]
    (is (= 9 (data/read-source outer a) (data/read-source outer b)))
    (is (not= a b))
    (is (= [left] (data/watch-ids outer a)))
    (is (= [right] (data/watch-ids outer b)))))

(deftest source-content-and-strongest-are-distinct-reads
  (let [id (ids/new-node-id)
        network (nb/install-cell net/empty-net id :retained-content 42)
        ref (data/reference id)]
    (is (= :retained-content (data/read-source network ref)))
    (is (= 42 (data/read-strongest network ref)))))

(deftest cyclic-network-uses-declared-ports-not-node-degree
  (let [state (run-source "(def-cells left right)
                          (def-net loop-net [x] [out] (-> x out) (-> out x))
                          (loop-net left right)
                          (-> 4 left)
                          (def selected (occurrence-of loop-net left right))
                          (def inputs (inputs-of selected))
                          (def outputs (outputs-of selected))")
        read-value (fn [name] (data/payload (net/network-cell-strongest
                                           (:program/net state) (fixture/binding-id state name))))]
    (is (= #{(data/reference (fixture/binding-id state 'left))} (read-value 'inputs)))
    (is (= #{(data/reference (fixture/binding-id state 'right))} (read-value 'outputs)))))

(deftest graph-growth-preserves-existing-identity-and-admits-earlier-node
  (let [a [[:outer] :a]
        b [[:outer] :b]
        initial (run-source "(def-cells graph)
                             (def result (transpose (filter (:: [x] true) graph) :list))")
        one (fixture/seed initial 'graph (trace/graph-union {:nodes {b "B"} :edges []}))
        two (fixture/seed one 'graph (trace/graph-union {:nodes {a "A" b "B"} :edges [[a b]]}))
        before (result one 'result)
        after (result two 'result)]
    (is (= [a b] (mapv :identity (:items after))))
    (is (= [(:value (first (:items before)))] (mapv :value (filter #(= b (:identity %)) (:items after)))))
    (is (= [[a b]] (:edges after)))))
