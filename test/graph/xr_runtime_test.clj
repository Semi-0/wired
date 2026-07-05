(ns graph.xr-runtime-test
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-runtime-server :as runtime-server]
            [graph.json :as json]
            [graph.xr-runtime :as xr]
            [graph.xr-server :as xr-server]
            [propagators.cells.value :as value]
            [propagators.compiler-2.env :as cenv]
            [propagators.datastructures.behavior :as behavior]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.tms :as tms]
            [propagators.network :as net]
            [propagators.semantic-trace :as semantic-trace])
  (:import [java.io BufferedInputStream BufferedOutputStream
            ByteArrayInputStream ByteArrayOutputStream]))

(defn- cell-id
  [session label]
  (cenv/binding-id (cenv/lookup (:program/env @session) (symbol label))))

(defn- raw-graph-value-for-cell
  [session cell-id]
  (let [graph (:graph @session)
        node-id (first (sort-by pr-str (get-in graph [:node-aliases cell-id])))]
    (get-in graph [:values node-id])))

(defn- projected-node-for-cell
  [projected cell-id]
  (let [cell-id* (pr-str cell-id)]
    (some (fn [node]
            (when (some #{cell-id*} (:aliases node))
              node))
          (get-in projected [:graph :nodes]))))

(defn- behavior-current
  [session label]
  (let [id (cell-id session label)
        content (net/network-cell-content (:program/net @session) id)
        current (behavior/strongest-value content)]
    (when-not (value/unusable? current)
      (behavior/base-value current))))

(defn- projected-runtime-graph
  [session]
  {:graph (xr/graph->json (:graph @session)
                           {:changed-node-ids (:runtime/changed-node-ids @session)
                            :changed-cell-ids (:runtime/changed-cells @session)})})

(defn- projected-current
  [node]
  (or (get-in node [:value :current])
      (get-in node [:value :value])))

(def behavior-widget-source
  "(def events)
   (def out)
   (def widget)
   (def-net retain-event [acc update] [out]
     (behavior-add-event acc update out))
   (behavior events retain-event (behavior-empty-state) out)
   (slider-io \"gain\" out events widget)")

(deftest xr-json-round-trips-runtime-command-shape
  (let [command {:op "xr/send-message"
                 :target {:label "a"}
                 :message {:kind "value" :value 42}}]
    (is (= command (json/read-json (json/write-json command))))))

(deftest xr-json-coalesces-nodes-that-alias-the-same-cell
  (let [graph {:nodes {:semantic-out "out"
                       :structural-out "out"
                       :prop "call inc"}
               :node-aliases {:runtime-out #{:semantic-out :structural-out}}
               :values {:semantic-out 5}
               :edges [[:prop :semantic-out]
                       [:structural-out :prop]]}
        projected (xr/graph->json graph)
        nodes (:nodes projected)
        out-nodes (filter #(= "out" (:label %)) nodes)]
    (is (= 1 (count out-nodes)))
    (is (= 2 (count (:edges projected))))
    (is (= [(pr-str :runtime-out)] (:aliases (first out-nodes))))))

(deftest traced-xr-json-preserves-aliases-and-coalesces-same-cell
  (let [graph {:semantic-trace/graph true
               :nodes {:prop "call inc"
                       :semantic-out "out"
                       :structural-out "out"}
               :node-aliases {:runtime-out #{:semantic-out :structural-out}}
               :values {:semantic-out 5}
               :edges [[:prop :semantic-out]
                       [:structural-out :prop]]}
        trace (semantic-trace/trace-graph graph {:label "out"
                                                 :direction :upstream})
        projected (xr/graph->json trace)
        out-nodes (filter #(= "out" (:label %)) (:nodes projected))]
    (is (= 1 (count out-nodes)))
    (is (= 2 (count (:edges projected))))
    (is (= [(pr-str :runtime-out)] (:aliases (first out-nodes))))))

(deftest xr-extend-graph-routes-through-runtime-compiler
  (let [session (runtime/new-session)
        result (xr/handle-command! session
                                   {:op :xr/extend-graph
                                    :source "(def a)"})]
    (is (true? (get-in result [:graph :graph])))
    (is (some? (cell-id session "a")))))

(deftest xr-extend-graph-accepts-textarea-with-multiple-forms
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source "(def out)\n(-> 42 out)"})
    (let [installed (xr/handle-command! session
                                        {:op :xr/trace/install
                                         :label "out"
                                         :direction :upstream})]
      (is (some? (cell-id session "out")))
      (is (seq (get-in installed [:graph :nodes])))
      (is (seq (get-in installed [:graph :edges])))
      (is (not-any? #(str/starts-with? (:label %) "app:")
                    (get-in installed [:graph :nodes]))))))

(deftest xr-send-message-injects-scalar-value-into-cell
  (let [session (runtime/new-session)]
    (xr/handle-command! session {:op :xr/extend-graph :source "(def a)"})
    (xr/handle-command! session
                        {:op :xr/send-message
                         :target {:label "a"}
                         :message {:kind "value" :value 42}})
    (is (= "42" (:strongest (runtime/read-cell
                             @session
                             {:cell-id (pr-str (cell-id session "a"))}))))))

(deftest xr-send-message-enters-runtime-commit-stage
  (let [session (runtime/new-session)]
    (xr/handle-command! session {:op :xr/extend-graph :source "(def a)"})
    (xr/handle-command! session
                        {:op :xr/send-message
                         :target {:label "a"}
                         :message {:kind "value" :value 42}})
    (is (= :xr/message
           (-> @session :runtime/inputs peek :runtime/input)))
    (is (= (cell-id session "a")
           (-> @session :runtime/inputs peek :cell-id)))))

(deftest tui-view-projection-is-pure
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A" :text "(+ 1 2)"})
    (let [state @session
          projected (runtime/project-tui-view state {:client-id "A"})]
      (is (= projected (runtime/project-tui-view state {:client-id "A"})))
      (is (= state @session)))))

(deftest xr-send-message-injects-behavior-event-content
  (let [session (runtime/new-session)]
    (xr/handle-command! session {:op :xr/extend-graph :source "(def events)"})
    (let [id (cell-id session "events")]
      (xr/handle-command! session
                          {:op :xr/send-message
                           :target {:label "events"}
                           :message {:kind "behavior-event"
                                     :tick 6
                                     :value 9}})
      (is (= 9 (obj/slot-value
                (net/network-cell-strongest (:program/net @session) id)
                6))))))

(deftest xr-send-message-can-inject-latest-behavior-value
  (let [session (runtime/new-session)]
    (xr/handle-command! session {:op :xr/extend-graph :source "(def behavior)"})
    (let [id (cell-id session "behavior")]
      (xr/handle-command! session
                          {:op :xr/send-message
                           :target {:label "behavior"}
                           :message {:kind "behavior-latest"
                                     :tick 1
                                     :value 9}})
      (xr/handle-command! session
                          {:op :xr/send-message
                           :target {:label "behavior"}
                           :message {:kind "behavior-latest"
                                     :tick 2
                                     :value 11}})
      (let [content (net/network-cell-content (:program/net @session) id)
            current (behavior/strongest-value content)]
        (is (= 11 (behavior/base-value current)))
        (is (= 2 (count (behavior/history-records content))))))))

(deftest xr-send-message-injects-distributed-tms-premise-fact
  (let [session (runtime/new-session)]
    (xr/handle-command! session {:op :xr/extend-graph :source "(def fact)"})
    (let [id (cell-id session "fact")]
      (xr/handle-command! session
                          {:op :xr/send-message
                           :target {:label "fact"}
                           :message {:kind "tms-premise"
                                     :premise "p/a"
                                     :epoch 1
                                     :active false}})
      (is (contains? (tms/distributed-slots
                      (net/network-cell-content (:program/net @session) id))
                     (tms/premise-slot-key "p/a" 1))))))

(deftest slider-io-registers-widget-through-boundary-effect
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source behavior-widget-source})
    (let [widget (get-in @session [:xr :widgets "gain"])
          channel (get-in widget [:channels "value"])]
      (is (= "slider" (:type widget)))
      (is (= (cell-id session "events") (:event-cell channel)))
      (is (= (cell-id session "out") (:view-cell channel)))
      (is (some #(= :xr/widget-register (:boundary/kind %))
                (get-in @session [:xr :effects]))))))

(deftest io-slider-registers-single-io-cell-with-symbol-default-name
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source "(def gain)
                                  (io:slider gain)"})
    (let [gain-id (cell-id session "gain")
          widget (get-in @session [:xr :widgets "gain"])
          channel (get-in widget [:channels "value"])]
      (is (= "slider" (:type widget)))
      (is (= gain-id (:view-cell channel)))
      (is (= gain-id (:event-cell channel)))
      (xr/handle-command! session
                          {:op :xr/widget-event
                           :widget-id "gain"
                           :channel "value"
                           :value 9})
      (is (= 9 (obj/slot-value
                (net/network-cell-strongest (:program/net @session) gain-id)
                1))))))

(deftest io-slider-panel-registers-channel-names-from-cell-symbols
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source "(def a)
                                  (def b)
                                  (def c)
                                  (io:slider-panel-name \"mix\" a b c)"})
    (let [widget (get-in @session [:xr :widgets "mix"])]
      (is (= "slider-panel" (:type widget)))
      (is (= #{"a" "b" "c"} (set (keys (:channels widget)))))
      (doseq [label ["a" "b" "c"]]
        (let [id (cell-id session label)
              channel (get-in widget [:channels label])]
          (is (= id (:view-cell channel)))
          (is (= id (:event-cell channel))))))))

(deftest io-slider-panel-defaults-panel-id-with-varargs
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source "(def a)
                                  (def b)
                                  (def c)
                                  (io:slider-panel a b c)"})
    (let [widget (get-in @session [:xr :widgets "slider-panel-0"])]
      (is (= "slider-panel" (:type widget)))
      (is (= #{"a" "b" "c"} (set (keys (:channels widget))))))))

(deftest io-slider-panel-routes-behavior-views-to-sibling-event-sources
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source "(define-behaviors a b c)
                                  (def out)
                                  (io:slider-panel a b c)
                                  (<-> (- (+ a b) c) out)"})
    (let [widget (get-in @session [:xr :widgets "slider-panel-0"])]
      (is (= "slider-panel" (:type widget)))
      (doseq [label ["a" "b" "c"]]
        (let [channel (get-in widget [:channels label])]
          (is (= (cell-id session label) (:view-cell channel)))
          (is (= (cell-id session (str label "-events"))
                 (:event-cell channel))))))
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "slider-panel-0"
                                 :channel "a"
                                 :value 10})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "slider-panel-0"
                                 :channel "b"
                                 :value 4})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "slider-panel-0"
                                 :channel "c"
                                 :value 3})
    (is (= 11 (behavior-current session "out")))))

(deftest xr-widget-event-rejects-unknown-widget-without-mutating
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source behavior-widget-source})
    (let [before (:program/net @session)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"xr widget channel not found"
           (xr/handle-command! session
                               {:op :xr/widget-event
                                :widget-id "missing"
                                :channel "value"
                                :value 42})))
      (is (= before (:program/net @session))))))

(deftest slider-widget-events-inject-behavior-source-epochs
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source behavior-widget-source})
    (xr/handle-command! session
                        {:op :xr/widget-event
                         :widget-id "gain"
                         :channel "value"
                         :value 9})
    (xr/handle-command! session
                        {:op :xr/widget-event
                         :widget-id "gain"
                         :channel "value"
                         :value 11})
    (let [events-id (cell-id session "events")
          events (net/network-cell-strongest (:program/net @session) events-id)]
      (is (= 9 (obj/slot-value events 1)))
      (is (= 11 (obj/slot-value events 2)))
      (is (= 11 (behavior-current session "out"))))))

(deftest xr-widget-event-response-does-not-replace-traced-graph
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [source ["(define-behaviors a b c)"
                    "(def out)"
                    "(<-> (- (+ a b) c) out)"
                    "(let-cell [g]
                       (trace out g)
                       (io:xr g))"
                    "(io:slider-panel a b c)"]]
      (runtime/append-tui-block! session {:client-id "A" :text source}))
    (let [response (xr/handle-command! session {:op :xr/widget-event
                                                :widget-id "slider-panel-0"
                                                :channel "a"
                                                :value 9})]
      (is (contains? response :widgets))
      (is (not (contains? response :graph))))))

(deftest xr-widget-projection-uses-strongest-not-behavior-content
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source behavior-widget-source})
    (xr/handle-command! session
                        {:op :xr/widget-event
                         :widget-id "gain"
                         :channel "value"
                         :value 9})
    (xr/handle-command! session
                        {:op :xr/widget-event
                         :widget-id "gain"
                         :channel "value"
                         :value 11})
    (let [projected (projected-runtime-graph session)
          out-id (cell-id session "out")
          content (net/network-cell-content (:program/net @session) out-id)
          strongest (net/network-cell-strongest (:program/net @session) out-id)
          raw-graph-value (raw-graph-value-for-cell session out-id)
          projected-node (projected-node-for-cell projected out-id)]
      (is (= 2 (count (behavior/history-records content))))
      (is (not= content strongest))
      (is (= strongest raw-graph-value))
      (is (= {:kind "behavior"
              :current "11"
              :retained-count 2
              :latest-time "2"}
             (:value projected-node))))))

(deftest slider-panel-routes-multiple-channels
  (let [session (runtime/new-session)
        source "(def ea)
                (def eb)
                (def ec)
                (def widget)
                (slider-panel-io \"panel\" \"a\" ea ea \"b\" eb eb \"c\" ec ec widget)"]
    (xr/handle-command! session {:op :xr/extend-graph :source source})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "panel"
                                 :channel "a"
                                 :value 10})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "panel"
                                 :channel "b"
                                 :value 20})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "panel"
                                 :channel "c"
                                 :value 30})
    (is (= 10 (obj/slot-value
               (net/network-cell-strongest (:program/net @session)
                                           (cell-id session "ea"))
               3)))
    (is (= 20 (obj/slot-value
               (net/network-cell-strongest (:program/net @session)
                                           (cell-id session "eb"))
               3)))
    (is (= 30 (obj/slot-value
               (net/network-cell-strongest (:program/net @session)
                                           (cell-id session "ec"))
               3)))))

(def complex-widget-behavior-source
  "(define-behaviors a b c)
   (def out)
   (io:slider-panel-name \"mix\" a b c)
   (<-> (- (+ a b) c) out)")

(def user-route-widget-behavior-extension
  "(def a-events)
   (def b-events)
   (def c-events)
   (def a)
   (def b)
   (def c)
   (def widget)
   (def-net retain-event [acc update] [out]
     (behavior-add-event acc update out))
   (behavior a-events retain-event (behavior-empty-state) a)
   (behavior b-events retain-event (behavior-empty-state) b)
   (behavior c-events retain-event (behavior-empty-state) c)
   (slider-panel-io \"mix\"
     \"a\" a a-events
     \"b\" b b-events
     \"c\" c c-events
     widget)
   (<-> (- (+ a b) c) out)")

(deftest widget-events-drive-complex-behavior-arithmetic-chain
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source complex-widget-behavior-source})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix" :channel "a" :value 10})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix" :channel "b" :value 4})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix" :channel "c" :value 3})
    (let [projected (projected-runtime-graph session)
          current-values (set (keep #(get-in % [:value :current])
                                    (get-in projected [:graph :nodes])))]
      (is (set/subset? #{"10" "4" "3"} current-values)))
    (is (= 11 (behavior-current session "out")))
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix" :channel "a" :value 20})
    (is (= 21 (behavior-current session "out")))
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix" :channel "c" :value 7})
    (is (= 17 (behavior-current session "out")))))

(deftest xr-extend-graph-preserves-tui-env-for-user-route-be-block-at
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "taro"})
    (runtime/submit-tui-block! session {:client-id "taro"
                                        :text "(def out)"})
    (runtime/submit-tui-block! session {:client-id "taro"
                                        :text "(let-cell [g r]
                                                (trace out g)
                                                (xr-io g r)
                                                r)"})
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source user-route-widget-behavior-extension})
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source "(be:block-at (instance taro) 2 out)"})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix"
                                 :channel "a"
                                 :value 10})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix"
                                 :channel "b"
                                 :value 4})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix"
                                 :channel "c"
                                 :value 3})
    (let [view (runtime/read-tui-view @session {:client-id "taro"})
          block-2 (some #(when (= 2 (:index %)) %) (:blocks view))]
      (is (contains? (:tuis @session) "taro"))
      (is (= 11 (behavior-current session "out")))
      (is (= 11 (:value block-2))))
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix"
                                 :channel "a"
                                 :value 20})
    (let [view (runtime/read-tui-view @session {:client-id "taro"})
          block-2 (some #(when (= 2 (:index %)) %) (:blocks view))]
      (is (= 21 (behavior-current session "out")))
      (is (= 21 (:value block-2))))
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix"
                                 :channel "c"
                                 :value 7})
    (let [view (runtime/read-tui-view @session {:client-id "taro"})
          block-2 (some #(when (= 2 (:index %)) %) (:blocks view))]
      (is (= 17 (behavior-current session "out")))
      (is (= 17 (:value block-2))))
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source "(def replay-check)"})
    (let [view (runtime/read-tui-view @session {:client-id "taro"})
          block-2 (some #(when (= 2 (:index %)) %) (:blocks view))]
      (is (= 17 (:value block-2))))))

(deftest widget-event-transaction-marks-downstream-output-node
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source complex-widget-behavior-source})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix" :channel "a" :value 10})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix" :channel "b" :value 4})
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "mix"
                                 :channel "c"
                                 :value 3})
    (let [projected (projected-runtime-graph session)
          out-id (cell-id session "out")
          out-node (projected-node-for-cell projected out-id)
          changed-cells (set (get-in projected [:graph :changed-cell-ids]))
          changed-nodes (set (get-in projected [:graph :changed-node-ids]))]
      (is out-node)
      (is (contains? changed-cells (pr-str out-id)))
      (is (contains? changed-nodes (:id out-node)))
      (is (< 1 (count changed-cells))))))

(deftest tui-submit-after-xr-traced-behavior-update-returns
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [source ["(def a-events)"
                    "(def out)"
                    "(def widget)"
                    "(def r)"
                    "(def-net retain-event [acc update] [out]
                       (behavior-add-event acc update out))"
                    "(behavior a-events retain-event (behavior-empty-state) out)"
                    "(slider-io \"gain\" out a-events widget)"
                    "(let-cell [g]
                       (trace out g)
                       (xr-io g r)
                       r)"]]
      (runtime/submit-tui-block! session {:client-id "A" :text source}))
    (xr/handle-command! session {:op :xr/widget-event
                                 :widget-id "gain"
                                 :channel "value"
                                 :value 9})
    (let [submitted (promise)
          submit-thread (Thread.
                         #(deliver submitted
                                   (runtime/submit-tui-block!
                                    session
                                    {:client-id "A"
                                     :text "out"}))
                         "test-submit-out-after-xr-widget")]
      (.setDaemon submit-thread true)
      (.start submit-thread)
      (let [view (deref submitted 3000 ::timeout)]
        (is (not= ::timeout view))
        (is (= 9 (behavior-current session "out")))
        (is (some #(= 9 (:value %)) (:blocks view)))))))

(deftest xr-trace-projects-widget-nodes-and-metadata
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source complex-widget-behavior-source})
    (let [trace (xr/handle-command! session
                                    {:op :xr/trace/install
                                     :label "out"
                                     :direction :upstream})
          nodes (get-in trace [:graph :nodes])
          widget-node (some #(when (= "widget" (:kind %)) %) nodes)
          labels (frequencies (map :label nodes))]
      (is widget-node)
      (is (= "slider-panel" (get-in widget-node [:ui :type])))
      (is (= "mix" (get-in widget-node [:ui :widget-id])))
      (is (= #{"a" "b" "c"}
             (set (map :channel (get-in widget-node [:ui :channels])))))
      (is (= 1 (get labels "out"))))))

(deftest xr-trace-install-and-read-projects-semantic-graph-json
  (let [session (runtime/new-session)]
    (xr/handle-command! session {:op :xr/extend-graph :source "(def a)"})
    (xr/handle-command! session {:op :xr/extend-graph :source "(-> 42 a)"})
    (let [installed (xr/handle-command! session
                                        {:op :xr/trace/install
                                         :label "a"
                                         :direction :upstream})
          read-back (xr/handle-command! session
                                        {:op :xr/trace/read
                                         :trace-id (:trace-id installed)})]
      (is (:trace-id installed))
      (is (seq (get-in read-back [:graph :nodes])))
      (is (seq (get-in read-back [:graph :edges]))))))

(deftest xr-trace-collapses-compound-application-and-expands-on-request
  (let [session (runtime/new-session)]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source "(def-net inc [x] [out] (<-> (+ x 1) out))"})
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source "(let-cell [out] (inc 4 out) out)"})
    (let [installed (xr/handle-command! session
                                        {:op :xr/trace/install
                                         :label "out"
                                         :direction :upstream})
          nodes (get-in installed [:graph :nodes])
          labels (set (map :label nodes))
          call-node (some #(when (= "call inc" (:label %)) %) nodes)
          expansion (xr/handle-command! session
                                        {:op :xr/trace/expand
                                         :label "call inc"})
          expansion-labels (set (map :label (get-in expansion [:graph :nodes])))]
      (is (contains? labels "call inc"))
      (is (not (contains? labels "+")))
      (is (= "propagator" (:kind call-node)))
      (is (true? (:expandable call-node)))
      (is (contains? expansion-labels "+")))))

(deftest xr-trace-keeps-forward-sync-output-cell-canonical
  (let [session (runtime/new-session)
        source "(def out)
                (-> 42 out)

                (def inc (network [a] [b] (-> (+ a 1) b)))
                (def out3)
                (inc out out3)"]
    (xr/handle-command! session
                        {:op :xr/extend-graph
                         :source source})
    (let [trace (xr/handle-command! session
                                    {:op :xr/trace/install
                                     :label "out"
                                     :direction :both})
          labels (map :label (get-in trace [:graph :nodes]))
          edges (set (map (fn [{:keys [from to]}]
                            [(some #(when (= (:id %) from) (:label %))
                                   (get-in trace [:graph :nodes]))
                             (some #(when (= (:id %) to) (:label %))
                                   (get-in trace [:graph :nodes]))])
                          (get-in trace [:graph :edges])))]
      (is (= 1 (get (frequencies labels) "out")))
      (is (contains? edges ["42" "->"]))
      (is (contains? edges ["->" "out"]))
      (is (contains? edges ["out" "call inc"]))
      (is (contains? edges ["call inc" "out3"])))))

(deftest compiler-runtime-xr-io-launches-effect-and-writes-receipt
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def a)"})
    (runtime/append-tui-block! session {:client-id "A" :text "(<-> 42 a)"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def receipt)"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(let-cell [g]
               (trace a g)
               (xr-io g receipt)
               receipt)"})
    (let [effects (:result (runtime/handle-command! session {:op :xr/effects}))
          receipt-id (cell-id session "receipt")
          receipt-value (net/network-cell-strongest (:program/net @session)
                                                    receipt-id)
          receipt-keys (obj/public-slot-keys receipt-value)
          receipt (obj/slot-value receipt-value (first receipt-keys))]
      (is (= 1 (count (:effects effects))))
      (is (= :xr/launch-trace (get-in effects [:effects 0 :boundary/kind])))
      (is (= :delivered (:boundary/status receipt)))
      (is (= :xr (:boundary/port receipt))))))

(deftest compiler-runtime-io-xr-returns-receipt-cell
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def a)"})
    (runtime/append-tui-block! session {:client-id "A" :text "(<-> 42 a)"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(let-cell [g]
               (trace a g)
               (io:xr g))"})
    (let [effects (:result (runtime/handle-command! session {:op :xr/effects}))
          receipt-id (get-in effects [:effects 0 :boundary/receipt-id])
          delivered (first (:launched effects))]
      (is (= 1 (count (:effects effects))))
      (is (= :xr/launch-trace (get-in effects [:effects 0 :boundary/kind])))
      (is (some? receipt-id))
      (is (net/network? (:receipt delivered))))))

(deftest compiler-runtime-io-xr-rejects-explicit-receipt-argument
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def a)"})
    (runtime/append-tui-block! session {:client-id "A" :text "(<-> 42 a)"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def receipt)"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(let-cell [g]
               (trace a g)
               (io:xr g receipt))"})
    (is (re-find #"io:xr expects trace graph"
                 (get-in @session
                         [:program/results ["A" 3] :error])))))

(deftest tui-submit-xr-io-records-runtime-transaction
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/submit-tui-block! session {:client-id "A" :text "(def a)"})
    (runtime/submit-tui-block! session {:client-id "A" :text "(<-> 42 a)"})
    (runtime/submit-tui-block! session {:client-id "A" :text "(def receipt)"})
    (let [view (runtime/submit-tui-block!
                session
                {:client-id "A"
                 :text "(let-cell [g]
                          (trace a g)
                          (xr-io g receipt)
                          receipt)"})
          effects (:result (runtime/handle-command! session {:op :xr/effects}))
          launch (first (:effects effects))
          graph (get-in launch [:boundary/payload :graph])]
      (is (seq (:blocks view)))
      (is (seq (:changed-cells view)))
      (is (seq (:changed-node-ids view)))
      (is (= :xr/launch-trace (:boundary/kind launch)))
      (is (seq (:nodes graph)))
      (is (every? (fn [[_node-id v]]
                    (not (behavior/behavior-value? v)))
                  (:values graph))))))

(deftest runtime-server-can-start-xr-with-shared-session
  (let [server (runtime-server/start-server-with-xr 0 0)]
    (try
      (is (some? (:session server)))
      (is (= (:session server) (get-in server [:xr :session])))
      (is (pos? (:port server)))
      (is (pos? (get-in server [:xr :port])))
      (finally
        ((:close server))))))

(deftest runtime-server-starts-xr-lazily-when-xr-io-is-compiled
  (let [server (runtime-server/start-server 0 0)
        port (:port server)
        request #(runtime-server/request runtime-server/default-host port %)]
    (try
      (is (nil? (:server @(:xr-state server))))
      (request {:op :tui/register :client-id "A"})
      (request {:op :tui/append-block :client-id "A" :text "(def a)"})
      (request {:op :tui/append-block :client-id "A" :text "(<-> 42 a)"})
      (request {:op :tui/append-block :client-id "A" :text "(def receipt)"})
      (request {:op :tui/append-block
                :client-id "A"
                :text "(let-cell [g]
                         (trace a g)
                         (xr-io g receipt)
                         receipt)"})
      (let [xr (:server @(:xr-state server))]
        (is (some? xr))
        (is (= (:session server) (:session xr)))
        (is (pos? (:port xr))))
      (finally
        ((:close server))))))

(deftest websocket-read-frame-unmasks-high-bit-payload-bytes
  (let [text "é"
        payload (.getBytes text "UTF-8")
        mask-key (byte-array [1 2 3 4])
        masked (byte-array (map-indexed (fn [i b]
                                          (unchecked-byte
                                           (bit-xor (bit-and b 0xff)
                                                    (bit-and (aget mask-key
                                                                  (mod i 4))
                                                             0xff))))
                                        payload))
        frame-bytes (byte-array (concat [0x81
                                         (bit-or 0x80 (alength payload))]
                                        (seq mask-key)
                                        (seq masked)))
        frame (#'xr-server/read-frame
               (BufferedInputStream.
                (ByteArrayInputStream. frame-bytes)))]
    (is (= 1 (:opcode frame)))
    (is (= text (:text frame)))))

(deftest websocket-effect-push-sends-xr-io-launch-graph
  (let [session (runtime/new-session)
        bytes (ByteArrayOutputStream.)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def out)"})
    (runtime/append-tui-block! session {:client-id "A" :text "(-> (+ 1 2) out)"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def receipt)"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(let-cell [g]
               (trace out g)
               (xr-io g receipt)
               receipt)"})
    (let [stop (#'xr-server/start-effect-push!
                session
                (BufferedOutputStream. bytes))]
      (try
        (stop)
        (let [frame (#'xr-server/read-frame
                     (BufferedInputStream.
                      (ByteArrayInputStream. (.toByteArray bytes))))
              payload (json/read-json (:text frame))]
          (is (= "xr/effects/update" (:type payload)))
          (is (true? (:ok payload)))
          (is (seq (get-in payload [:result :graph :nodes]))))
        (finally
          (stop))))))

(deftest xr-effect-payload-includes-widgets-registered-after-io-xr
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [source ["(define-behaviors a b c)"
                    "(def out)"
                    "(<-> (- (+ a b) c) out)"
                    "(let-cell [g]
                       (trace out g)
                       (io:xr g))"
                    "(io:slider-panel a b c)"]]
      (runtime/append-tui-block! session {:client-id "A" :text source}))
    (let [payload (#'xr-server/latest-effect-payload session)
          widgets (get-in payload [:graph :widgets])]
      (is (some #(= "slider-panel-0" (:id %)) widgets))
      (is (= #{"a" "b" "c"}
             (->> widgets
                  (filter #(= "slider-panel-0" (:id %)))
                  first
                  :channels
                  vals
                  (map :channel)
                  set))))))

(deftest io-xr-relaunches-on-widget-update-after-be-block-rebuild
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [source ["(define-behaviors a b c)"
                    "(def out)"
                    "(<-> (- (+ a b) c) out)"
                    "(let-cell [g]
                       (trace out g)
                       (io:xr g))"
                    "(io:slider-panel a b c)"
                    "(-> out (be:block 7))"]]
      (runtime/append-tui-block! session {:client-id "A" :text source}))
    (doseq [[channel value] [["a" 76] ["b" 4] ["c" 3]]]
      (runtime/commit-runtime-input! session
                                     {:runtime/input :xr/widget-event
                                      :widget-id "slider-panel-0"
                                      :channel channel
                                      :value value}))
    (runtime/append-tui-block! session {:client-id "A"
                                        :text "(-> a (be:block 9))"})
    (let [launch-effect? #(= [:xr :xr/launch-trace]
                             [(:boundary/port %) (:boundary/kind %)])
          before-effects (:effects (:result
                                    (runtime/handle-command!
                                     session
                                     {:op :xr/effects})))
          before-launches (filter launch-effect? before-effects)
          before-epoch (apply max (map :boundary/epoch before-launches))]
      (runtime/commit-runtime-input! session
                                     {:runtime/input :xr/widget-event
                                      :widget-id "slider-panel-0"
                                      :channel "a"
                                      :value 77})
      (let [effects (:effects (:result
                               (runtime/handle-command!
                                session
                                {:op :xr/effects})))
            launches (filter launch-effect? effects)
            after-epoch (apply max (map :boundary/epoch launches))
            payload (#'xr-server/latest-effect-payload session)
            out-node (projected-node-for-cell payload (cell-id session "out"))
            a-node (projected-node-for-cell payload (cell-id session "a"))
            view (runtime/read-tui-view @session {:client-id "A"})]
        (is (= before-epoch after-epoch))
        (is (= "78" (projected-current out-node)))
        (is (= "77" (projected-current a-node)))
        (is (= 78 (get-in view [:blocks 7 :value])))
        (is (= 77 (get-in view [:blocks 9 :value])))))))

(deftest xr-io-supports-local-let-cell-receipt-target
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (runtime/append-tui-block! session {:client-id "A" :text "(def out)"})
    (runtime/append-tui-block! session {:client-id "A" :text "(-> (+ 1 2) out)"})
    (runtime/append-tui-block!
     session
     {:client-id "A"
      :text "(let-cell [g r]
               (trace out g)
               (xr-io g r)
               r)"})
    (let [view (runtime/read-tui-view @session {:client-id "A"})
          values (mapv :value (:blocks view))
          effects (:effects (:result (runtime/handle-command! session
                                                               {:op :xr/effects})))]
      (is (= "(let-cell [g r]
               (trace out g)
               (xr-io g r)
               r)"
             (nth values 2)))
      (is (= 1 (count effects)))
      (is (= :xr/launch-trace (get-in effects [0 :boundary/kind]))))))

(deftest xr-io-trace-updates-when-later-block-adds-upstream-producer
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [source ["(def out2)"
                    "(def a)"
                    "(-> (+ a 2) out2)"
                    "(def r)"
                    "(let-cell [g]
                       (trace out2 g)
                       (xr-io g r)
                       r)"
                    "(-> (+ 1 2) a)"]]
      (runtime/append-tui-block! session {:client-id "A" :text source}))
    (let [effects (:effects (:result (runtime/handle-command! session
                                                               {:op :xr/effects})))
          labels (->> effects
                      last
                      :boundary/payload
                      :graph
                      :nodes
                      vals
                      frequencies)]
      (is (<= 2 (get labels "+" 0)))
      (is (<= 2 (get labels "->" 0)))
      (is (= 1 (get labels "1" 0)))
      (is (<= 2 (get labels "a" 0)))
      (is (pos? (get labels "out2" 0))))))

(deftest xr-io-trace-renders-topology-before-upstream-value-arrives
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "A"})
    (doseq [source ["(def out)"
                    "(def a)"
                    "(-> (+ 1 a) out)"
                    "(let-cell [g r]
                       (trace out g)
                       (xr-io g r)
                       r)"]]
      (runtime/append-tui-block! session {:client-id "A" :text source}))
    (let [effects (:effects (:result (runtime/handle-command! session
                                                               {:op :xr/effects})))
          labels (->> effects
                      last
                      :boundary/payload
                      :graph
                      :nodes
                      vals
                      frequencies)]
      (is (pos? (get labels "out" 0)))
      (is (pos? (get labels "a" 0)))
      (is (pos? (get labels "+" 0)))
      (is (pos? (get labels "->" 0))))))
