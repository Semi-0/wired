(ns graph.vijual.compiler-2-runtime-server-test
  (:require [charm.components.text-input :as text-input]
            [charm.components.viewport :as viewport]
            [charm.message :as charm-msg]
            [charm.style.core :as style]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-runtime-server :as server]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [graph.compiler-2-tui :as tui]
            [propagators.core :as core]
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
                                                :direction :upstream})]
    (is (= "4" (get (semantic-repl/value-labels graph) "x")))
    (is (= "5" (get (semantic-repl/value-labels graph) "next")))
    (is (contains? (set (semantic-repl/edge-labels trace)) ["+" "<->"]))
    (is (contains? (set (semantic-repl/edge-labels trace)) ["x" "+"]))))

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
      (is (= [0 1] (mapv :index (:blocks view))))
      (is (= "(+ 1 2)" (get-in view [:blocks 0 :value])))
      (is (= 3 (get-in view [:blocks 1 :value]))))
    (is (= 2 (:index (runtime/append-tui-block! session
                                                {:client-id "tui-a"}))))
    (runtime/sync-tui-block! session
                             {:client-id "tui-a"
                              :index 2
                              :from {:label "result"}})
    (let [view (runtime/read-tui-view @session {:client-id "tui-a"})]
      (is (= [0 1 2 3] (mapv :index (:blocks view))))
      (is (= 3 (get-in view [:blocks 2 :value]))))))

(deftest blocks-compile-into-one-growing-env
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "grow"})
    (runtime/append-tui-block! session
                               {:client-id "grow"
                                :text "(def-net inc1 [x] [out]
                                        (<-> (+ x 1) out))"})
    (runtime/append-tui-block! session
                               {:client-id "grow"
                                :text "(let-cell [out]
                                        (inc1 4 out)
                                        out)"})
    (let [view (runtime/read-tui-view @session {:client-id "grow"})]
      (is (= 5 (get-in view [:blocks 3 :value]))))
    (runtime/edit-tui-block! session
                             {:client-id "grow"
                              :index 0
                              :text "(def-net inc1 [x] [out]
                                      (<-> (+ x 2) out))"})
    (let [view (runtime/read-tui-view @session {:client-id "grow"})]
      (is (= :bool4/contradiction (get-in view [:blocks 0 :value]))))))

(deftest client-instances-have-separate-block-views-but-one-env
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "a"})
    (runtime/register-tui! session {:client-id "b"})
    (runtime/append-tui-block! session
                               {:client-id "a"
                                :text "(def-net inc1 [x] [out]
                                        (<-> (+ x 1) out))"})
    (runtime/append-tui-block! session
                               {:client-id "b"
                                :text "(let-cell [out]
                                        (inc1 4 out)
                                        out)"})
    (let [a-view (runtime/read-tui-view @session {:client-id "a"})
          b-view (runtime/read-tui-view @session {:client-id "b"})]
      (is (= [0 1] (mapv :index (:blocks a-view))))
      (is (= [0 1] (mapv :index (:blocks b-view))))
      (is (= 5 (get-in b-view [:blocks 1 :value]))))))

(deftest block-order-is-top-to-bottom
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "order"})
    (runtime/append-tui-block! session
                               {:client-id "order"
                                :text "(let-cell [out]
                                        (later 4 out)
                                        out)"})
    (runtime/append-tui-block! session
                               {:client-id "order"
                                :text "(def-net later [x] [out]
                                        (<-> (+ x 1) out))"})
    (let [view (runtime/read-tui-view @session {:client-id "order"})]
      (is (not= 5 (get-in view [:blocks 1 :value]))))))

(deftest tui-renders-graph-valued-blocks-with-vijual
  (let [session (runtime/new-session)]
    (runtime/compile-source! session source)
    (let [installed (runtime/install-semantic-trace! session
                                                     {:label "next"
                                                      :direction :upstream
                                                      :interval-ms 5000})
          trace-id (:trace-id installed)]
      (runtime/append-tui-block! session {:client-id "tui-graph"})
      (runtime/sync-tui-block! session
                               {:client-id "tui-graph"
                                :index 0
                                :trace-id trace-id})
      (let [view (runtime/read-tui-view @session {:client-id "tui-graph"})
            rendered (tui/render-view view)]
        (is (tui/graph-value? (get-in view [:blocks 0 :value])))
        (is (str/includes? rendered "+"))
        (is (str/includes? rendered "next")))
      (runtime/stop-installed-trace! session {:trace-id trace-id}))))

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
    (is (nil? resize-cmd))
    (is (= "" (text-input/value (:input resized))))
    (is (str/includes? (tui/view refreshed) "[0]"))
    (is (str/includes? (style/strip-ansi (tui/view typed)) "> a"))
    (is (str/includes? (style/strip-ansi (tui/view refreshed)) "> "))))

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
        full-content (viewport/viewport-content (:viewport resized))
        visible (style/strip-ansi (tui/view resized))]
    (is (str/includes? full-content "[0]"))
    (is (str/includes? full-content "node0"))
    (is (str/includes? full-content "node20"))
    (is (every? #(<= (count %) 60)
                (str/split-lines full-content)))
    (is (every? #(<= (count %) 60)
                (str/split-lines (viewport/viewport-view (:viewport resized)))))
    (is (str/includes? visible "> "))))

(deftest block-language-can-sync-compiled-cell-into-another-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "tui-lang"})
    (runtime/append-tui-block! session {:client-id "tui-lang"
                                        :text source})
    (runtime/append-tui-block! session {:client-id "tui-lang"})
    (runtime/append-tui-block! session
                               {:client-id "tui-lang"
                                :text "(let-cell [v]
                                         (block-at tui-lang.block 1 v)
                                         (block-at tui-lang.block 2 v)
                                         v)"})
    (let [view (runtime/read-tui-view @session {:client-id "tui-lang"})]
      (is (= 5 (get-in view [:blocks 2 :value])))
      (is (= 5 (get-in view [:blocks 5 :value]))))))

(deftest block-language-can-trace-shared-runtime-graph-into-next-block
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "a"})
    (runtime/register-tui! session {:client-id "b"})
    (runtime/append-tui-block! session {:client-id "a"
                                        :text "(def-net inc1 [x] [next]
                                                (<-> (+ x 1) next))"})
    (runtime/append-tui-block! session {:client-id "b"
                                        :text "(let-cell [next g]
                                                (inc1 4 next)
                                                (trace next g)
                                                (block-at b.block 1 g)
                                                g)"})
    (let [b-view (runtime/read-tui-view @session {:client-id "b"})
          trace (get-in b-view [:blocks 1 :value])
          rendered (tui/render-view b-view)]
      (is (tui/graph-value? trace))
      (is (contains? (set (semantic-repl/edge-labels trace)) ["+" "<->"]))
      (is (= "5" (get (semantic-repl/value-labels trace) "next")))
      (is (str/includes? rendered "+"))
      (is (str/includes? rendered "next")))))

(deftest generated-output-blocks-are-replaced-on-rebuild
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "tui-a"})
    (runtime/register-tui! session {:client-id "tui-b"})
    (runtime/append-tui-block! session
                               {:client-id "tui-a"
                                :text "(def-net inc1 [x] [next]
                                        (<-> (+ x 1) next))"})
    (runtime/append-tui-block! session
                               {:client-id "tui-b"
                                :text "(let-cell [next g]
                                        (inc1 4 next)
                                        (trace next :downstream g)
                                        (block-at tui-b.block 1 g)
                                        g)"})
    (let [a-view (runtime/read-tui-view @session {:client-id "tui-a"})
          b-view (runtime/read-tui-view @session {:client-id "tui-b"})]
      (is (not= :bool4/contradiction
                (get-in a-view [:blocks 1 :value])))
      (is (tui/graph-value? (get-in a-view [:blocks 1 :value])))
      (is (tui/graph-value? (get-in b-view [:blocks 1 :value]))))))

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
                                     :direction :upstream})]
          (is (:ok cells))
          (is (some #(= "result" (:label %)) (:result cells)))
          (is (= "4" (get (semantic-repl/value-labels (:result graph)) "x")))
          (is (contains? (set (semantic-repl/edge-labels (:result trace)))
                         ["+" "<->"]))))
      (testing "installed tracer can be read by another client"
        (let [installed (server/request server/default-host port
                                        {:op :semantic/trace/install
                                         :label "next"
                                         :direction :upstream
                                         :interval-ms 5000})
              trace-id (get-in installed [:result :trace-id])
              read-back (server/request server/default-host port
                                        {:op :semantic/trace/read
                                         :trace-id trace-id})]
          (is (:ok installed))
          (is (:ok read-back))
          (is (contains? (set (semantic-repl/edge-labels (get-in read-back [:result :graph])))
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
