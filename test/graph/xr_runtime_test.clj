(ns graph.xr-runtime-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-runtime-server :as runtime-server]
            [graph.json :as json]
            [graph.xr-runtime :as xr]
            [graph.xr-server :as xr-server]
            [propagators.compiler-2.env :as cenv]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.tms :as tms]
            [propagators.network :as net]
            [propagators.semantic-trace :as semantic-trace])
  (:import [java.io BufferedInputStream BufferedOutputStream
            ByteArrayInputStream ByteArrayOutputStream]))

(defn- cell-id
  [session label]
  (cenv/binding-id (cenv/lookup (:program/env @session) (symbol label))))

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
