(ns graph.vijual.compiler-2-runtime-server-test
  (:require [charm.components.text-input :as text-input]
            [charm.components.viewport :as viewport]
            [charm.message :as charm-msg]
            [charm.style.core :as style]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-runtime.tui-annotations :as tui-annotations]
            [graph.compiler-2-runtime-server :as server]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [graph.compiler-2-tui :as tui]
            [graph.xr-runtime :as xr]
            [propagators.cells.value :as value]
            [propagators.compiler-2.env :as cenv]
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.event :as event]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.semantic-trace :as semantic-trace]))

(def source
  "(let-cell [same next]
     ((network [x] [same next]
        (<-> x same)
        (<-> (+ x 1) next))
      4 same next)
     next)")

(deftest runtime-session-compiles-inspects-and-traces
  (let [session (runtime/new-session)
        graph (runtime/compile-source! session source)
        trace (runtime/semantic-trace @session {:label "next"
                                                :direction :upstream})
        expansion (semantic-repl/expansion trace {:label "call :: [x]"})]
    (is (= "4" (get (semantic-repl/value-labels expansion) "x")))
    (is (= "5" (get (semantic-repl/value-labels graph) "next")))
    (is (contains? (set (semantic-repl/edge-labels trace)) ["4" "call :: [x]"]))
    (is (contains? (set (semantic-repl/edge-labels trace)) ["call :: [x]" "next"]))
    (is (contains? (set (semantic-repl/edge-labels expansion)) ["+" "<->"]))
    (is (contains? (set (semantic-repl/edge-labels expansion)) ["x" "+"]))))

(deftest downstream-trace-expands-monotonically-on-epoch
  (let [request-id (ids/new-node-id)
        graph-id (ids/new-node-id)
        epoch-id (ids/new-node-id)
        out-id (ids/new-node-id)
        start-graph (semantic-trace/graph-union
                     {:nodes {:a "a" :b "b"}
                      :edges [[:a :b]]
                      :values {}})
        expanded-graph (semantic-trace/graph-union
                        {:nodes {:a "a" :b "b" :c "c"}
                         :edges [[:a :b] [:b :c]]
                         :values {}})
        n0 (-> net/empty-net
               (nb/install-cell request-id {:label "a" :direction :downstream}
                                {:label "a" :direction :downstream})
               (nb/install-cell graph-id start-graph start-graph)
               (nb/install-cell epoch-id (semantic-trace/epoch 0) (semantic-trace/epoch 0))
               (nb/install-cell out-id))
        [prop-id n1] ((semantic-trace/p:semantic-trace request-id graph-id epoch-id out-id) n0)
        n2 (nb/run-propagators n1 [prop-id])
        [tasks n3] (core/eval-cells [(message graph-id expanded-graph)
                                     (message epoch-id (semantic-trace/epoch 1))]
                                    n2)
        n4 (core/run-tasks tasks n3)
        trace (net/network-cell-strongest n4 out-id)]
    (is (contains? (set (:edges trace)) [:a :b]))
    (is (contains? (set (:edges trace)) [:b :c]))
    (is (= "c" (get-in trace [:nodes :c])))))

(deftest trace-request-primitive-builds-label-request
  (let [source-id (ids/new-node-id)
        direction-id (ids/new-node-id)
        out-id (ids/new-node-id)
        n0 (-> net/empty-net
               (nb/install-cell source-id 'next 'next)
               (nb/install-cell direction-id :upstream :upstream)
               (nb/install-cell out-id))
        [prop-id n1] ((semantic-trace/p:trace-request source-id direction-id out-id) n0)
        n2 (nb/run-propagators n1 [prop-id])]
    (is (= {:label "next" :direction :upstream}
           (net/network-cell-strongest n2 out-id)))))

(deftest semantic-trace-does-not-target-by-equal-value
  (let [graph {:semantic-trace/graph true
               :nodes {:a "a" :b "b"}
               :values {:a 1 :b 1}
               :edges [[:a :b]]}
        trace (semantic-trace/trace-graph graph
                                          {:value-label 1
                                           :direction :downstream})]
    (is (empty? (:nodes trace)))
    (is (empty? (:edges trace)))))

(deftest runtime-rebuild-has-no-source-special-forms
  (let [runtime-source (slurp "graph/compiler_2_runtime.clj")]
    (is (not (str/includes? runtime-source "runtime-sync-form?")))
    (is (not (str/includes? runtime-source "runtime-trace-form?")))
    (is (not (str/includes? runtime-source "rebuild-sync-form")))
    (is (not (str/includes? runtime-source "rebuild-trace-form")))))

(deftest tui-view-is-monotone-linked-blocks
  (let [session (runtime/new-session)]
    (runtime/compile-source! session source)
    (is (= 0 (:next-index (runtime/register-tui! session {:client-id "tui-a"}))))
    (is (= 0 (:index (runtime/append-tui-block! session
                                                {:client-id "tui-a"
                                                 :text "(+ 1 2)"}))))
    (let [view (runtime/read-tui-view @session {:client-id "tui-a"})]
      (is (= [0] (mapv :index (:blocks view))))
      (is (= "(+ 1 2)" (get-in view [:blocks 0 :value]))))
    (is (= 1 (:index (runtime/append-tui-block! session
                                                {:client-id "tui-a"}))))
    (runtime/append-tui-block! session
                               {:client-id "tui-a"
                                :text "(let-cell [v]
                                        (<-> 3 v)
                                        (block-at % 1 v))"})
    (let [view (runtime/read-tui-view @session {:client-id "tui-a"})]
      (is (= [0 1 2] (mapv :index (:blocks view))))
      (is (= 3 (get-in view [:blocks 1 :value]))))))

(deftest expression-result-auto-loads-next-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/edit-tui-block! session {:client-id "A"
                                      :index 0
                                      :text "(+ 1 2)"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= "(+ 1 2)" (get-in view [:blocks 0 :value])))
      (is (= 3 (get-in view [:blocks 1 :value]))))))

(deftest tui-submit-builds-target-and-rebuilds-once
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (let [view (runtime/submit-tui-block! session {:client-id "A"
                                                   :text "(+ 1 2)"})]
      (is (= "(+ 1 2)" (get-in view [:blocks 0 :value])))
      (is (= 3 (get-in view [:blocks 1 :value])))
      (is (= :bool4/nothing (get-in view [:blocks 2 :value]))))))

(deftest free-def-does-not-write-error-into-source-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(def a)"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= "(def a)" (get-in view [:blocks 0 :value])))
      (is (nil? (get-in @session [:program/results ["A" 0] :error]))))))

(deftest explicit-block-at-target-displays-nothing
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(def a)"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/edit-tui-block! session {:client-id "A"
                                      :index 1
                                      :text "(block-at (instance A) 2 a)"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})
          rendered (tui/render-view view)]
      (is (= :bool4/nothing (get-in view [:blocks 2 :value])))
      (is (true? (get-in view [:blocks 2 :referenced?])))
      (is (str/includes? rendered "[2]"))
      (is (str/includes? rendered ":bool4/nothing"))
      (let [a-id (:binding/id (cenv/lookup (:program/env @session) 'a))]
        (runtime/commit-runtime-input! session
                                       {:runtime/input :cell-message
                                        :cell-id a-id
                                        :update 7})
        (is (= 7 (get-in (runtime/read-tui-view @session {:client-id "A"})
                         [:blocks 2 :value])))
        (runtime/commit-runtime-input! session
                                       {:runtime/input :cell-message
                                        :cell-id a-id
                                        :update 8})
        (is (= :bool4/contradiction
               (get-in (runtime/read-tui-view @session {:client-id "A"})
                       [:blocks 2 :value])))))))

(deftest tui-render-view-includes-block-annotation
  (let [rendered (tui/render-view
                  {:blocks [{:index 0
                             :value 9
                             :annotation "[be:a @1]"}]})]
    (is (str/includes? rendered "9  [be:a @1]"))))

(deftest tui-renders-graph-with-mixed-string-and-keyword-node-ids
  (let [rendered (tui/render-value
                  {:nodes {:a "a"
                           "b" "b"}
                   :edges [[:a "b"]]})]
    (is (not (str/includes? rendered "graph render error")))
    (is (str/includes? rendered "a"))
    (is (str/includes? rendered "b"))))

(deftest block-target-expression-writes-future-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session
                               {:client-id "A"
                                :text "(-> 7 (block 2))"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= "(-> 7 (block 2))" (get-in view [:blocks 0 :value])))
      (is (= :bool4/nothing (get-in view [:blocks 1 :value])))
      (is (= 7 (get-in view [:blocks 2 :value])))
      (is (true? (get-in view [:blocks 2 :referenced?]))))))

(deftest block-at-target-expression-writes-future-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session
                               {:client-id "A"
                                :text "(-> 7 (block-at (instance A) 2))"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= 7 (get-in view [:blocks 2 :value])))
      (is (true? (get-in view [:blocks 2 :referenced?]))))))

(deftest be-block-target-expression-displays-latest-update
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def events)"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def out)"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(def-net retain-event [acc update] [out]
               (behavior-add-event acc update out))"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(behavior events retain-event (behavior-empty-state) out)"})
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(-> out (be:block 5))"})
    (let [events-id (:binding/id (cenv/lookup (:program/env @session)
                                              'events))]
      (runtime/commit-runtime-input! session
                                     {:runtime/input :cell-message
                                      :cell-id events-id
                                      :update (obj/compound-object {1 7})})
      (let [block (get-in (runtime/read-tui-view @session {:client-id "A"})
                          [:blocks 5])]
        (is (= 7 (:value block)))
        (is (= :behavior (get-in block [:annotations 0 :kind])))
        (is (= #{events-id}
               (set (get-in block [:annotations 0 :identities]))))
        (is (= 1 (get-in block [:annotations 0 :latest-time])))
        (is (str/includes? (:annotation block) "[be:")))
      (runtime/commit-runtime-input! session
                                     {:runtime/input :cell-message
                                      :cell-id events-id
                                      :update (obj/compound-object {2 8})})
      (let [block (get-in (runtime/read-tui-view @session {:client-id "A"})
                          [:blocks 5])]
        (is (= 8 (:value block)))
        (is (= 2 (get-in block [:annotations 0 :latest-time])))))))

(deftest tui-simplified-behavior-slider-panel-updates-be-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(define-behaviors a b c)"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def out)"})
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(io:slider-panel a b c)"})
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(<-> (be:- (be:+ a b) c) out)"})
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(-> out (be:block 6))"})
    (runtime/commit-runtime-input! session
                                   {:runtime/input :xr/widget-event
                                    :widget-id "slider-panel-0"
                                    :channel "a"
                                    :value 10})
    (runtime/commit-runtime-input! session
                                   {:runtime/input :xr/widget-event
                                    :widget-id "slider-panel-0"
                                    :channel "b"
                                    :value 4})
    (runtime/commit-runtime-input! session
                                   {:runtime/input :xr/widget-event
                                    :widget-id "slider-panel-0"
                                    :channel "c"
                                    :value 3})
    (is (= 11 (get-in (runtime/read-tui-view @session {:client-id "A"})
                      [:blocks 6 :value])))))

(deftest tui-slider-panel-events-feed-default-arithmetic
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [source ["(def-cells a b c d)"
                    "(-> (- (+ a c) b) d)"
                    "(def-cell g)"
                    "(trace d g)"
                    "(io:xr g)"
                    "(io:slider-panels a b c)"
                    "(-> d (block 7))"]]
      (runtime/append-tui-block! session {:client-id "A" :text source}))
    (doseq [[channel value] [["a" 10] ["b" 4] ["c" 3]]]
      (runtime/commit-runtime-input! session
                                     {:runtime/input :xr/widget-event
                                      :widget-id "slider-panel-0"
                                      :channel channel
                                      :value value}))
    (let [d-id (cenv/binding-id (cenv/lookup (:program/env @session) 'd))
          d-content (net/network-cell-content (:program/net @session) d-id)
          annotations (tui-annotations/value-annotations d-content)]
      (is (= [9] (vec (vals (event/active-values d-content)))))
      (is (= 9 (tui-annotations/project-value d-content)))
      (is (= :event (get-in annotations [0 :kind])))
      (is (= 9 (get-in (runtime/read-tui-view @session {:client-id "A"})
                       [:blocks 7 :value]))))
    (runtime/commit-runtime-input! session
                                   {:runtime/input :xr/widget-event
                                    :widget-id "slider-panel-0"
                                    :channel "a"
                                    :value 11})
    (let [d-id (cenv/binding-id (cenv/lookup (:program/env @session) 'd))
          d-content (net/network-cell-content (:program/net @session) d-id)]
      (is (= [10] (vec (vals (event/active-values d-content)))))
      (is (= 10 (tui-annotations/project-value d-content)))
      (let [block (get-in (runtime/read-tui-view @session {:client-id "A"})
                          [:blocks 7])]
        (is (= 10 (:value block)))
        (is (str/includes? (:annotation block) "/slider-panel-0@4"))))))

(deftest tui-annotations-compact-trace-and-derived-event-identities
  (let [node-id (ids/new-node-id)
        trace-label (tui-annotations/format-annotations
                     [{:kind :behavior
                       :identities [[:xr/trace-subscription
                                     [:trace/subscribe
                                      node-id
                                      {:trace/target true
                                       :trace/symbol "d"}]]]
                       :latest-time 6}])
        event-label (tui-annotations/format-annotations
                     [{:kind :event
                       :facts [{:input-id [:compiler-2/primitive node-id]
                                :source [:event/derived
                                         [:compiler-2/primitive node-id]
                                         #{[node-id "slider-panel-0"]}]
                                :timestamp #{{:input-id node-id
                                              :source "slider-panel-0"
                                              :timestamp 3}}
                                :source-state event/active-state}]}])]
    (is (= "[be:xr-trace @6]" trace-label))
    (is (str/includes? event-label "[event:primitive/derived@joined{"))
    (is (str/includes? event-label "/slider-panel-0@3"))
    (is (not (str/includes? trace-label "#propagators.ids.NodeId")))
    (is (not (str/includes? trace-label ":trace/subscribe")))
    (is (not (str/includes? event-label "#propagators.ids.NodeId")))))

(deftest block-target-rejects-non-empty-past-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def occupied)"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A"})
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"non-empty past block"
         (runtime/edit-tui-block! session
                                  {:client-id "A"
                                   :index 2
                                   :text "(-> 7 (block 0))"})))))

(deftest block-at-application-is-traceable
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(def a)"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/edit-tui-block! session {:client-id "A"
                                      :index 1
                                      :text "(block-at (instance A) 2 a)"})
    (let [trace (runtime/semantic-trace @session {:label "block-at"
                                                  :direction :both})
          edges (set (semantic-repl/edge-labels trace))]
      (is (contains? edges ["a" "block-at"]))
      (is (contains? edges ["block-at" "a"])))))

(deftest blocks-compile-into-one-growing-env
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "grow"})
    (runtime/append-tui-block! session
                               {:client-id "grow"
                                :text "(def-net inc1 [x] [out]
                                        (<-> (+ x 1) out))"})
    (runtime/append-tui-block! session {:client-id "grow"})
    (runtime/append-tui-block! session
                               {:client-id "grow"
                                :text "(let-cell [out]
                                        (inc1 4 out)
                                        (block-at % 1 out)
                                        out)"})
    (let [view (runtime/read-tui-view @session {:client-id "grow"})]
      (is (= 5 (get-in view [:blocks 1 :value]))))
    (runtime/edit-tui-block! session
                             {:client-id "grow"
                              :index 0
                              :text "(def-net inc1 [x] [out]
                                      (<-> (+ x 2) out))"})
    (let [view (runtime/read-tui-view @session {:client-id "grow"})]
      (is (= "(def-net inc1 [x] [out]
                                      (<-> (+ x 2) out))"
             (get-in view [:blocks 0 :value])))
      (is (= 6 (get-in view [:blocks 1 :value]))))))

(deftest client-instances-have-separate-block-views-but-one-env
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "a"})
    (runtime/register-tui! session {:client-id "b"})
    (runtime/append-tui-block! session
                               {:client-id "a"
                                :text "(def-net inc1 [x] [out]
                                        (<-> (+ x 1) out))"})
    (runtime/append-tui-block! session {:client-id "b"})
    (runtime/append-tui-block! session
                               {:client-id "b"
                                :text "(let-cell [out]
                                        (inc1 4 out)
                                        (block-at % 0 out)
                                        out)"})
    (let [a-view (runtime/read-tui-view @session {:client-id "a"})
          b-view (runtime/read-tui-view @session {:client-id "b"})]
      (is (= [0] (mapv :index (:blocks a-view))))
      (is (= [0 1] (mapv :index (:blocks b-view))))
      (is (= 5 (get-in b-view [:blocks 0 :value]))))))

(deftest multiple-clients-can-share-one-tui-instance
  (let [session (runtime/new-session)]
    (is (= 0 (:next-index (runtime/register-tui! session {:client-id "shared"}))))
    (is (= 0 (:next-index (runtime/register-tui! session {:client-id "shared"}))))
    (runtime/submit-tui-block! session
                               {:client-id "shared"
                                :text "(+ 1 2)"})
    (let [view (runtime/read-tui-view @session {:client-id "shared"})]
      (is (= [0 1 2] (mapv :index (:blocks view))))
      (is (= 3 (get-in view [:blocks 1 :value]))))
    (is (= 1 (:remaining-clients
              (runtime/unregister-tui! session {:client-id "shared"}))))
    (runtime/submit-tui-block! session
                               {:client-id "shared"
                                :text "(+ 2 3)"})
    (let [view (runtime/read-tui-view @session {:client-id "shared"})]
      (is (= [0 1 2 3 4] (mapv :index (:blocks view))))
      (is (= 5 (get-in view [:blocks 3 :value]))))
    (is (zero? (:remaining-clients
                (runtime/unregister-tui! session {:client-id "shared"}))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"tui client not found"
                          (runtime/read-tui-view @session
                                                 {:client-id "shared"})))))

(deftest block-order-is-top-to-bottom
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "order"})
    (runtime/append-tui-block! session
                               {:client-id "order"
                                :text "(let-cell [out]
                                        (later 4 out)
                                        (block-at % 1 out)
                                        out)"})
    (runtime/append-tui-block! session {:client-id "order"})
    (runtime/append-tui-block! session
                               {:client-id "order"
                                :text "(def-net later [x] [out]
                                        (<-> (+ x 1) out))"})
    (let [view (runtime/read-tui-view @session {:client-id "order"})]
      (is (= 5 (get-in view [:blocks 1 :value]))))))

(deftest later-def-net-repairs-earlier-watch-without-full-rebuild
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(let-cell [out] (later 4 out) (block-at % 1 out) out)"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(def-net later [x] [out] (<-> (+ x 1) out))"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= 5 (get-in view [:blocks 1 :value])))
      (is (zero? (:runtime/full-rebuild-fallbacks @session))))))

(deftest be-block-watch-installs-and-updates-without-full-rebuild
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def x0)"})
    (doseq [i (range 1 11)]
      (runtime/append-tui-block!
       session
       {:client-id "A"
        :text (format "(-> (+ %d x%d) x%d)" i (dec i) i)}))
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(-> x10 (be:block 20))"})
    (runtime/commit-runtime-input!
     session
     {:runtime/input :cell-message
      :cell-id (cenv/binding-id (cenv/lookup (:program/env @session) 'x0))
      :update 1})
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= 56 (get-in view [:blocks 20 :value])))
      (is (zero? (:runtime/full-rebuild-fallbacks @session))))))

(deftest edited-be-block-watch-uses-new-display-epoch
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def x0)"})
    (doseq [i (range 1 11)]
      (runtime/append-tui-block!
       session
       {:client-id "A"
        :text (format "(-> (+ %d x%d) x%d)" i (dec i) i)}))
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(-> x10 (be:block 20))"})
    (runtime/commit-runtime-input!
     session
     {:runtime/input :cell-message
      :cell-id (cenv/binding-id (cenv/lookup (:program/env @session) 'x0))
      :update 1})
    (is (= 56 (get-in (runtime/read-tui-view @session {:client-id "A"})
                      [:blocks 20 :value])))
    (runtime/edit-tui-block! session {:client-id "A"
                                      :index 11
                                      :text "(-> x5 (be:block 20))"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= 16 (get-in view [:blocks 20 :value])))
      (is (zero? (:runtime/full-rebuild-fallbacks @session))))))

(deftest later-def-net-updates-earlier-free-cell-watch
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/register-tui! session {:client-id "B"})
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(def a)"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/edit-tui-block! session {:client-id "A"
                                      :index 1
                                      :text "(block-at (instance A) 2 a)"})
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(inc 1 a)"})
    (is (= :bool4/nothing
           (get-in (runtime/read-tui-view @session {:client-id "A"})
                   [:blocks 2 :value])))
    (runtime/append-tui-block! session
                               {:client-id "B"
                                :text "(def-net inc [x] [out]
                                        (<-> (+ x 1) out))"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= 2 (get-in view [:blocks 2 :value]))))))

(deftest tui-runtime-can-build-compiler-2-behavior-cell
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(def-net retain [acc next] [out]
               (behavior-add-event acc next full)
               (behavior-retain-last full 1 out))"})
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(let-cell [events retained]
               (behavior-event 6 2 events)
               (behavior-event 8 3 events)
               (behavior-cell events (behavior-empty-state) retain retained)
               (block-at % 1 retained)
               retained)"})
    (let [displayed (get-in (runtime/read-tui-view @session {:client-id "A"})
                            [:blocks 1 :value])]
      (is (= 3 displayed)))))

(deftest xr-graph-hides-generic-procedure-implementation-slots
  (let [session (runtime/new-session)]
    (xr/xr-extend-graph!
     session
     {:source "(def events)
               (def retained)
               (def out)
               (def-net retain [acc update] [full]
                 (behavior-add-event acc update full))
               (behavior-event 6 2 events)
               (behavior events retain (behavior-empty-state) retained)
               (<-> (+ retained retained) out)"})
    (let [labels (set (map :label (:nodes (xr/graph->json (:graph @session)))))]
      (is (not-any? #(str/starts-with? % "slot generic/") labels))
      (is (not-any? #(str/starts-with? % "slot method/") labels))
      (is (not-any? #(str/starts-with? % "slot operator/") labels))
      (is (contains? labels "behavior"))
      (is (contains? labels "behavior-event"))
      (is (contains? labels "+")))))

(deftest tui-runtime-can-project-distributed-tms-retraction
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(let-cell [out]
               (premise-input :yes :p 0 out)
               (premise-retract :p 1 out)
               (block-at % 1 out)
               out)"})
    (runtime/append-tui-block! session {:client-id "A"})
    (let [displayed (get-in (runtime/read-tui-view @session {:client-id "A"})
                            [:blocks 1 :value])]
      (is (value/nothing? displayed)))))

(deftest tui-block-at-displays-tms-annotations
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(let-cell [out]
               (premise-input :yes :p 0 out)
               (block-at % 1 out)
               out)"})
    (runtime/append-tui-block! session {:client-id "A"})
    (let [block (get-in (runtime/read-tui-view @session {:client-id "A"})
                        [:blocks 1])]
      (is (= :yes (:value block)))
      (is (= :tms (get-in block [:annotations 0 :kind])))
      (is (= :justified (get-in block [:annotations 0 :status])))
      (is (= #{:p}
             (set (get-in block [:annotations 0 :active-premises]))))
      (is (str/includes? (:annotation block) "[tms:justified")))))

(deftest top-level-relationships-do-not-auto-output-into-next-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (dotimes [_ 10]
      (runtime/append-tui-block! session {:client-id "A"}))
    (doseq [[index text] [[5 "(def out3)"]
                          [6 "(def x 1)"]
                          [7 "(<-> (- 3 x) out3)"]
                          [8 "(block-at (instance A) 9 out3)"]]]
      (runtime/edit-tui-block! session {:client-id "A"
                                        :index index
                                        :text text}))
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= "(block-at (instance A) 9 out3)"
             (get-in view [:blocks 8 :value])))
      (is (= 2 (get-in view [:blocks 9 :value]))))))

(deftest translate-primitive-is-bidirectional
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [text ["(def jp)"
                  "(def en)"
                  "(translate jp en)"
                  "(<-> jp \"猫\")"
                  "(block-at (instance A) 5 en)"]]
      (runtime/append-tui-block! session {:client-id "A"
                                          :text text}))
    (runtime/append-tui-block! session {:client-id "A"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= "cat" (get-in view [:blocks 5 :value])))))
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [text ["(def jp)"
                  "(def en)"
                  "(translate jp en)"
                  "(<-> en \"dog\")"
                  "(block-at (instance A) 5 jp)"]]
      (runtime/append-tui-block! session {:client-id "A"
                                          :text text}))
    (runtime/append-tui-block! session {:client-id "A"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})]
      (is (= "犬" (get-in view [:blocks 5 :value]))))))

(deftest blank-target-append-rebuilds-previous-trace-output
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [text ["(def a)"
                  "(<-> (- 3 1) a)"
                  "(def g)"
                  "(trace a :upstream g)"]]
      (runtime/append-tui-block! session {:client-id "A"
                                          :text text}))
    (runtime/append-tui-block! session {:client-id "A"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})
          graph (get-in view [:blocks 4 :value])
          edges (set (semantic-repl/edge-labels graph))]
      (is (tui/graph-value? graph))
      (is (contains? edges ["-" "2"]))
      (is (contains? edges ["2" "<->"]))
      (is (contains? edges ["<->" "a"])))))

(deftest trace-graph-output-does-not-stall-later-tui-updates
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/register-tui! session {:client-id "B"})
    (doseq [text ["(def a)"
                  "(<-> (- 3 1) a)"
                  "(def g)"
                  "(trace a :upstream g)"]]
      (runtime/append-tui-block! session {:client-id "A"
                                          :text text}))
    (runtime/append-tui-block! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "B"
                                        :text "(def b 9)"})
    (let [a-view (runtime/read-tui-view @session {:client-id "A"})
          b-view (runtime/read-tui-view @session {:client-id "B"})]
      (is (tui/graph-value? (get-in a-view [:blocks 4 :value])))
      (is (= "(def b 9)" (get-in b-view [:blocks 0 :value]))))))

(deftest trace-block-reacts-to-later-upstream-relationships
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [text ["(def a)"
                  "(def g)"
                  "(trace a :upstream g)"]]
      (runtime/submit-tui-block! session {:client-id "A"
                                          :text text}))
    (is (empty? (:edges (get-in (runtime/read-tui-view @session {:client-id "A"})
                                [:blocks 3 :value]))))
    (runtime/submit-tui-block! session {:client-id "A"
                                        :text "(<-> (- 3 1) a)"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})
          trace (get-in view [:blocks 3 :value])
          edges (set (semantic-repl/edge-labels trace))]
      (is (tui/graph-value? trace))
      (is (contains? edges ["-" "2"]))
      (is (contains? edges ["<->" "a"])))))

(deftest submitted-trace-keeps-upstream-literal-constants
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [text ["(def a)"
                  "(<-> (+ 1 2) a)"
                  "(def g)"
                  "(trace a :upstream g)"]]
      (runtime/submit-tui-block! session {:client-id "A"
                                          :text text}))
    (let [view (runtime/read-tui-view @session {:client-id "A"})
          trace (get-in view [:blocks 4 :value])
          edges (set (semantic-repl/edge-labels trace))
          rendered (tui/render-view view)]
      (is (tui/graph-value? trace))
      (is (contains? edges ["1" "+"]))
      (is (contains? edges ["2" "+"]))
      (is (contains? edges ["+" "3"]))
      (is (contains? edges ["<->" "a"]))
      (is (str/includes? rendered "| 1 |"))
      (is (str/includes? rendered "| 2 |"))
      (is (str/includes? rendered "| + |"))
      (is (str/includes? rendered "| 3 |")))))

(deftest trace-block-reacts-through-intermediate-cell-chain
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [text ["(def a)"
                  "(def b)"
                  "(<-> b a)"
                  "(def g)"
                  "(trace a :upstream g)"]]
      (runtime/submit-tui-block! session {:client-id "A"
                                          :text text}))
    (runtime/submit-tui-block! session {:client-id "A"
                                        :text "(<-> (+ 1 2) b)"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})
          trace (get-in view [:blocks 5 :value])
          edges (set (semantic-repl/edge-labels trace))]
      (is (tui/graph-value? trace))
      (is (contains? edges ["<->" "a"]))
      (is (contains? edges ["<->" "b"]))
      (is (contains? edges ["1" "+"]))
      (is (contains? edges ["2" "+"]))
      (is (contains? edges ["+" "3"])))))

(deftest tui-renders-graph-valued-blocks-with-vijual
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "tui-graph"})
    (runtime/append-tui-block! session {:client-id "tui-graph"})
    (runtime/append-tui-block! session
                               {:client-id "tui-graph"
                                :text "(let-cell [same next g]
                                        ((network [x] [same next]
                                           (<-> x same)
                                           (<-> (+ x 1) next))
                                         4 same next)
                                        (trace next g)
                                        (block-at % 0 g)
                                        g)"})
    (let [view (runtime/read-tui-view @session {:client-id "tui-graph"})
          rendered (tui/render-view view)]
      (is (tui/graph-value? (get-in view [:blocks 0 :value])))
      (is (str/includes? rendered "+"))
      (is (str/includes? rendered "next")))))

(deftest charm-tui-update-and-view-are-pure
  (let [model (tui/init-model {:client-id "pure" :poll-ms 10})
        [typed cmd1] (tui/update-fn model (charm-msg/key-press "a"))
        [submitted cmd2] (tui/update-fn typed (charm-msg/key-press :enter))
        [refreshed cmd3] (tui/update-fn submitted
                                        {:type :runtime/view
                                         :response {:ok true
                                                    :result {:blocks [{:index 0
                                                                       :value "a"}]}}})
        [resized resize-cmd] (tui/update-fn refreshed (charm-msg/window-size 20 6))]
    (is (= "a" (text-input/value (:input typed))))
    (is (nil? cmd1))
    (is (= "" (text-input/value (:input submitted))))
    (is (= :cmd (:type cmd2)))
    (is (= "a" (get-in refreshed [:view :blocks 0 :value])))
    (is (= :cmd (:type cmd3)))
    (is (= {:width 20 :height 6} (:window-size resized)))
    (is (= :cmd (:type resize-cmd)))
    (is (= "" (text-input/value (:input resized))))
    (is (= "" (tui/view resized)))
    (is (str/includes? (tui/view refreshed) "[0]"))
    (is (str/includes? (style/strip-ansi (tui/view typed)) "> a"))
    (is (str/includes? (style/strip-ansi (tui/view refreshed)) "[1]> "))))

(deftest charm-tui-refresh-skips-render-for-unchanged-view
  (let [renders (atom 0)
        view-a {:blocks [{:index 0 :value "a"}]}
        view-b {:blocks [{:index 0 :value "b"}]}
        model (tui/init-model {:client-id "skip-render" :poll-ms 10})]
    (with-redefs [tui/render-view (fn
                                    ([_view] "rendered")
                                    ([_view _viewport-size]
                                     (swap! renders inc)
                                     "rendered"))]
      (let [[first-refresh _] (tui/update-fn model
                                             {:type :runtime/view
                                              :response {:ok true
                                                         :result view-a}})]
        (is (= 1 @renders))
        (let [[same-refresh _] (tui/update-fn first-refresh
                                              {:type :runtime/view
                                               :response {:ok true
                                                          :result view-a}})]
          (is (= 1 @renders))
          (is (= view-a (:view same-refresh)))
          (let [[changed-refresh _] (tui/update-fn same-refresh
                                                   {:type :runtime/view
                                                    :response {:ok true
                                                               :result view-b}})]
            (is (= 2 @renders))
            (is (= view-b (:view changed-refresh)))))))))

(deftest charm-tui-resize-render-waits-for-latest-dimensions
  (let [model (tui/init-model {:client-id "resize" :poll-ms 10})
        [refreshed _] (tui/update-fn model
                                     {:type :runtime/view
                                      :response {:ok true
                                                 :result {:blocks [{:index 0
                                                                    :value "a"}]}}})
        [wide _] (tui/update-fn refreshed (charm-msg/window-size 80 20))
        [narrow _] (tui/update-fn wide (charm-msg/window-size 40 10))
        [stale _] (tui/update-fn narrow {:type :runtime/resize-render
                                         :window-size {:width 80
                                                       :height 20}})
        [settled _] (tui/update-fn stale {:type :runtime/resize-render
                                          :window-size {:width 40
                                                        :height 10}})]
    (is (= {:width 40 :height 10} (:resize/target stale)))
    (is (nil? (:resize/target settled)))
    (is (= "" (tui/view narrow)))
    (is (str/includes? (viewport/viewport-content (:viewport settled)) "[0]"))))

(deftest charm-tui-renders-changed-view-while-resize-is-pending
  (let [model (tui/init-model {:client-id "resize-update" :poll-ms 10})
        [refreshed _] (tui/update-fn model
                                     {:type :runtime/view
                                      :response {:ok true
                                                 :result {:blocks [{:index 0
                                                                    :value "old"}]}}})
        [resizing _] (tui/update-fn refreshed (charm-msg/window-size 80 20))
        [updated _] (tui/update-fn resizing
                                   {:type :runtime/view
                                    :response {:ok true
                                               :result {:blocks [{:index 0
                                                                  :value "new"}]}}})]
    (is (= {:width 80 :height 20} (:resize/target updated)))
    (is (= "new" (get-in updated [:view :blocks 0 :value])))
    (is (= "" (tui/view updated)))))

(deftest charm-tui-input-prompt-displays-next-block-index
  (let [model (tui/init-model {:client-id "next" :poll-ms 10})
        [refreshed _] (tui/update-fn model
                                     {:type :runtime/view
                                      :response {:ok true
                                                 :result {:blocks [{:index 0
                                                                    :value "a"}
                                                                   {:index 1
                                                                    :value :bool4/nothing}]}}})
        rendered (style/strip-ansi (tui/view refreshed))]
    (is (= "[1]> " (:prompt (:input refreshed))))
    (is (not (str/includes? rendered ":bool4/nothing")))
    (is (str/includes? rendered "[1]> "))))

(deftest charm-tui-submit-creates-next-target-before-compiling
  (let [{:keys [port close]} (server/start-server 0)]
    (try
      (server/request server/default-host port {:op :tui/register
                                                :client-id "A"})
      (let [model (-> (tui/init-model {:client-id "A"
                                       :port port
                                       :poll-ms 0})
                      (assoc :view {:blocks []})
                      (assoc-in [:input :value]
                                (vec "(let-cell [a]
                                       (<-> 3 a)
                                       (block-at (instance A) 1 a))")))
            [_ cmd] (tui/update-fn model (charm-msg/key-press :enter))
            response ((:fn cmd))
            view (:result (:response response))]
        (is (:ok (:response response)))
        (is (= 3 (get-in view [:blocks 1 :value])))
        (is (= :bool4/nothing (get-in view [:blocks 2 :value]))))
      (finally
        (close)))))

(deftest udp-agent-api-sends-and-reads-blocks
  (let [{:keys [udp-port close]} (server/start-server 0 0 0)]
    (try
      (let [sent (server/udp-request server/default-host
                                     udp-port
                                     {:op :agent/send-block
                                      :client-id "agent"
                                      :text "(+ 1 2)"})
            all-blocks (server/udp-request server/default-host
                                           udp-port
                                           {:op :agent/blocks
                                            :client-id "agent"})
            block-1 (server/udp-request server/default-host
                                        udp-port
                                        {:op :agent/block
                                         :client-id "agent"
                                         :index 1})]
        (is (:ok sent))
        (is (:ok all-blocks))
        (is (= "(+ 1 2)" (get-in all-blocks [:result :blocks 0 :value])))
        (is (= 3 (get-in all-blocks [:result :blocks 1 :value])))
        (is (= 3 (get-in block-1 [:result :value]))))
      (finally
        (close)))))

(deftest charm-tui-refresh-preserves-scroll-offset
  (let [blocks (vec (for [i (range 10)]
                      {:index i
                       :value (str "block " i "\nline\nline\nline")}))
        model (tui/init-model {:client-id "scroll" :poll-ms 10})
        [refreshed _] (tui/update-fn model
                                     {:type :runtime/view
                                      :response {:ok true
                                                 :result {:blocks blocks}}})
        [resized _] (tui/update-fn refreshed (charm-msg/window-size 40 8))
        scrolled (nth (iterate #(first (tui/update-fn %
                                                      (charm-msg/key-press "down")))
                               resized)
                      12)
        offset (get-in scrolled [:viewport :y-offset])
        blocks' (update-in blocks [9 :value] str "\nchanged")
        [rerendered _] (tui/update-fn scrolled
                                      {:type :runtime/view
                                       :response {:ok true
                                                  :result {:blocks blocks'}}})]
    (is (pos? offset))
    (is (= offset (get-in rerendered [:viewport :y-offset])))))

(deftest tui-keeps-full-rendered-graph-in-scrollable-viewport
  (let [graph {:nodes (into {}
                            (for [i (range 21)]
                              [(keyword (str "n" i)) (str "node" i)]))
               :edges (vec
                       (for [i (range 20)]
                         [(keyword (str "n" i))
                          (keyword (str "n" (inc i)))]))}
        model (tui/init-model {:client-id "viewport" :poll-ms 10})
        [refreshed _] (tui/update-fn model
                                     {:type :runtime/view
                                      :response {:ok true
                                                 :result {:blocks [{:index 0
                                                                    :value graph}]}}})
        [resized _] (tui/update-fn refreshed (charm-msg/window-size 60 6))
        [settled _] (tui/update-fn resized {:type :runtime/resize-render
                                            :window-size {:width 60
                                                          :height 6}})
        full-content (viewport/viewport-content (:viewport settled))
        visible (style/strip-ansi (tui/view settled))]
    (is (= "" (tui/view resized)))
    (is (str/includes? full-content "[0]"))
    (is (str/includes? full-content "node0"))
    (is (str/includes? full-content "node20"))
    (is (every? #(<= (count %) 60)
                (str/split-lines full-content)))
    (is (str/includes? visible "> "))))

(deftest block-language-can-sync-compiled-cell-into-another-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "tui-lang"})
    (runtime/append-tui-block! session {:client-id "tui-lang"})
    (runtime/append-tui-block! session
                               {:client-id "tui-lang"
                                :text "(let-cell [v]
                                         (<-> \"copied\" v)
                                         (block-at % 0 v)
                                         v)"})
    (let [view (runtime/read-tui-view @session {:client-id "tui-lang"})]
      (is (= "copied" (get-in view [:blocks 0 :value]))))))

(deftest old-dot-block-symbol-is-not-bound
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "b"})
    (runtime/append-tui-block! session {:client-id "b"})
    (runtime/append-tui-block! session
                               {:client-id "b"
                                :text "(let-cell [out]
                                        (block-at b.block 0 out)
                                        out)"})
    (let [view (runtime/read-tui-view @session {:client-id "b"})]
      (is (= :bool4/nothing (get-in view [:blocks 0 :value]))))))

(deftest block-language-can-trace-shared-runtime-graph-into-next-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "a"})
    (runtime/register-tui! session {:client-id "b"})
    (runtime/append-tui-block! session {:client-id "a"
                                        :text "(def-net inc1 [x] [next]
                                                (<-> (+ x 1) next))"})
    (runtime/append-tui-block! session {:client-id "b"})
    (runtime/append-tui-block! session {:client-id "b"
                                        :text "(let-cell [next g]
                                                (inc1 4 next)
                                                (trace next g)
                                                (block-at (instance b) 0 g)
                                                g)"})
    (let [b-view (runtime/read-tui-view @session {:client-id "b"})
          trace (get-in b-view [:blocks 0 :value])
          expansion (semantic-repl/expansion trace {:label "call inc1"})
          rendered (tui/render-view b-view)]
      (is (tui/graph-value? trace))
      (is (contains? (set (semantic-repl/edge-labels trace)) ["4" "call inc1"]))
      (is (contains? (set (semantic-repl/edge-labels trace)) ["call inc1" "next"]))
      (is (contains? (set (semantic-repl/edge-labels expansion)) ["+" "<->"]))
      (is (= "5" (get (semantic-repl/value-labels trace) "next")))
      (is (str/includes? rendered "call inc1"))
      (is (str/includes? rendered "next")))))

(deftest block-language-traces-def-net-application-dependence-graph
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "tui-a"})
    (runtime/append-tui-block! session
                               {:client-id "tui-a"
                                :text "(def-net inc [x] [next]
                                        (<-> (+ x 1) next))"})
    (runtime/append-tui-block! session {:client-id "tui-a"})
    (runtime/append-tui-block! session
                               {:client-id "tui-a"
                                :text "(let-cell [out g]
                                        (inc 4 out)
                                        (trace \"out\" g)
                                        (block-at % 1 g)
                                        g)"})
    (let [view (runtime/read-tui-view @session {:client-id "tui-a"})
          trace (get-in view [:blocks 1 :value])
          edges (set (semantic-repl/edge-labels trace))
          expansion (semantic-repl/expansion trace {:label "call inc"})
          expansion-edges (set (semantic-repl/edge-labels expansion))]
      (is (tui/graph-value? trace))
      (is (contains? edges ["4" "call inc"]))
      (is (contains? edges ["call inc" "out"]))
      (is (contains? expansion-edges ["x" "+"]))
      (is (contains? expansion-edges ["+" "<->"]))
      (is (not-any? #(= "slot block/text" %) (mapcat identity edges))))))

(deftest cross-session-block-at-writes-only-target-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "tui-a"})
    (runtime/register-tui! session {:client-id "tui-b"})
    (runtime/append-tui-block! session
                               {:client-id "tui-a"
                                :text "(def-net inc1 [x] [next]
                                        (<-> (+ x 1) next))"})
    (runtime/append-tui-block! session {:client-id "tui-a"})
    (runtime/append-tui-block! session {:client-id "tui-b"})
    (runtime/append-tui-block! session
                               {:client-id "tui-b"
                                :text "(let-cell [next g]
                                        (inc1 4 next)
                                        (trace next :downstream g)
                                        (block-at (instance tui-b) 0 g)
                                        g)"})
    (let [a-view (runtime/read-tui-view @session {:client-id "tui-a"})
          b-view (runtime/read-tui-view @session {:client-id "tui-b"})]
      (is (not= :bool4/contradiction
                (get-in a-view [:blocks 1 :value])))
      (is (not (tui/graph-value? (get-in a-view [:blocks 1 :value]))))
      (is (tui/graph-value? (get-in b-view [:blocks 0 :value]))))))

(deftest tcp-clients-share-one-runtime-session
  (let [{:keys [port close]} (server/start-server 0)]
    (try
      (testing "one client compiles and another can inspect the same session"
        (is (:ok (server/request server/default-host port
                                 {:op :compile/source
                                  :source source})))
        (let [cells (server/request server/default-host port {:op :cells/list})
              graph (server/request server/default-host port {:op :semantic/graph})
              trace (server/request server/default-host port
                                    {:op :semantic/trace
                                     :label "next"
                                     :direction :upstream})
              expansion (semantic-repl/expansion (:result trace)
                                                 {:label "call :: [x]"})]
          (is (:ok cells))
          (is (some #(= "result" (:label %)) (:result cells)))
          (is (= "4" (get (semantic-repl/value-labels expansion) "x")))
          (is (contains? (set (semantic-repl/edge-labels (:result trace)))
                         ["call :: [x]" "next"]))))
      (testing "installed tracer can be read by another client"
        (let [installed (server/request server/default-host port
                                        {:op :semantic/trace/install
                                         :label "next"
                                         :direction :upstream
                                         :interval-ms 5000})
              trace-id (get-in installed [:result :trace-id])
              read-back (server/request server/default-host port
                                        {:op :semantic/trace/read
                                         :trace-id trace-id})
              expansion (semantic-repl/expansion (get-in read-back [:result :graph])
                                                 {:label "call :: [x]"})]
          (is (:ok installed))
          (is (:ok read-back))
          (is (contains? (set (semantic-repl/edge-labels expansion))
                         ["+" "<->"]))
          (is (:ok (server/request server/default-host port
                                   {:op :semantic/trace/stop
                                    :trace-id trace-id})))))
      (testing "tui blocks are exposed over the socket"
        (is (:ok (server/request server/default-host port
                                 {:op :tui/register
                                  :client-id "socket-tui"})))
        (is (:ok (server/request server/default-host port
                                 {:op :tui/append-block
                                  :client-id "socket-tui"
                                  :text "hello"})))
        (let [view (server/request server/default-host port
                                   {:op :tui/read-view
                                    :client-id "socket-tui"})]
          (is (:ok view))
          (is (= "hello" (get-in view [:result :blocks 0 :value])))))
      (testing "bad ops fail without killing the server"
        (is (= false (:ok (server/request server/default-host port
                                          {:op :no/such-op}))))
        (is (:ok (server/request server/default-host port {:op :semantic/graph}))))
      (finally
        (close)))))
