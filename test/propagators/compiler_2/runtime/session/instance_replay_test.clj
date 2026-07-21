(ns propagators.compiler-2.runtime.session.instance-replay-test
  (:require [clojure.test :refer [deftest is testing]]
            [propagators.compiler-2.runtime :as runtime]
            [propagators.compiler-2.runtime.boundary.effects :as effects]
            [propagators.compiler-2.runtime.session.instance-replay :as replay]
            [graph.compiler-2-runtime-json-server :as json-server]
            [graph.compiler-2-runtime-server :as server]
            [graph.json :as json]
            [propagators.cells.value :as value]
            [propagators.datastructures.tms.distributed :as tms]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(def fixture-path
  "test/propagators/compiler_2/runtime/fixtures/four_edits.json")

(defn fixture-manifest
  []
  (json/read-json (slurp fixture-path)))

(defn start-json-runtime
  []
  (binding [server/*temperature-logger-enabled?* false]
    (server/attach-json-server (server/start-server 0 0 0) 0)))

(defn display-content
  [runtime-state client-id index]
  (let [block (get-in runtime-state [:tuis client-id :blocks index])]
    (net/network-cell-content (:program/net runtime-state)
                              (:display-id block))))

(defn display-view
  [runtime-state client-id index]
  (tms/tms-view (tms/distributed-slots
                 (display-content runtime-state client-id index))))

(defn block-record
  [runtime-state client-id block-index version]
  (get-in runtime-state
          [:tuis client-id :blocks block-index :version-history version]))

(defn premise-update!
  [session record epoch active?]
  (runtime/commit-runtime-input!
   session
   {:runtime/input :cell-message
    :cell-id (:premise-state-cell record)
    :update (tms/distributed-premise-update (:premise-id record)
                                            epoch active?)}))

(deftest four-edit-json-replay-keeps-tms-in-the-block-display-cell
  (let [runtime-server (start-json-runtime)]
    (try
      (let [port (:json-port runtime-server)
            imported (json-server/request
                      "127.0.0.1" port
                      {:op "instance/import"
                       :manifest (fixture-manifest)})
            state @(:session runtime-server)
            steps (get-in imported [:result :steps])
            display (display-view state "debug" 1)
            display-props
            (filter #(and (prop/prop? %)
                          (= :runtime/tui-block-display (prop/prop-name %)))
                    (vals (net/net-env (:program/net state))))
            topology-before (replay/topology-counts (:program/net state))
            retried (json-server/request
                     "127.0.0.1" port
                     {:op "instance/import"
                      :manifest (fixture-manifest)})
            exported (json-server/request
                      "127.0.0.1" port {:op "instance/export"})]
        (is (:ok imported))
        (is (= "imported" (get-in imported [:result :status])))
        (is (= [11 12 13 14] (mapv :compiled-result steps)))
        (is (= [11 12 13 14]
               (mapv #(get-in % [:view :blocks 1 :value]) steps)))
        (is (= 4 (count (:versioned/commit-log state))))
        (is (= "replayed" (get-in retried [:result :status])))
        (is (= topology-before
               (replay/topology-counts
                (:program/net @(:session runtime-server))))
            "an exact socket retry declares no duplicate topology")
        (is (= 4 (count display-props)))
        (is (= 1 (count (tms/active-claims display))))
        (is (= 3 (count (:tms/inactive-claims display))))
        (is (empty? (effects/outbox-effects (:program/net state))))
        (is (nil? (get-in state
                          [:tuis "debug" :blocks 1 :display-effect-tick])))
        (is (= 3 (get-in exported
                         [:result :snapshot :views 0 :blocks 0 :version])))
        (is (= "(+ 4 10)"
               (get-in exported
                       [:result :snapshot :views 0 :blocks 0 :source])))
        (is (= 14 (get-in exported
                          [:result :snapshot :views 0 :blocks 1 :value])))
        (is (= {:active-claim-count 1 :inactive-claim-count 3}
               (select-keys (first (get-in exported
                                           [:result :snapshot :displays]))
                            [:active-claim-count
                             :inactive-claim-count]))))
      (finally
        ((:close runtime-server))))))

(deftest block-display-reacts-to-retract-bring-in-repeat-and-conflict
  (let [session (runtime/new-session)]
    (runtime/import-instance! session (fixture-manifest))
    (let [v2 (block-record @session "debug" 0 2)
          v3 (block-record @session "debug" 0 3)]
      (premise-update! session v3 10 false)
      (is (= value/nothing
             (get-in (runtime/read-tui-view @session {:client-id "debug"})
                     [:blocks 1 :value])))
      (premise-update! session v2 11 true)
      (is (= 13
             (get-in (runtime/read-tui-view @session {:client-id "debug"})
                     [:blocks 1 :value])))
      (premise-update! session v3 12 true)
      (is (= :contradiction
             (first
              (get-in (runtime/read-tui-view @session {:client-id "debug"})
                      [:blocks 1 :value])))
          "the host view preserves the TMS contradiction provenance")
      (let [before (display-content @session "debug" 1)]
        (premise-update! session v3 12 true)
        (is (= before (display-content @session "debug" 1))
            "repeated premise delivery is idempotent"))
      (premise-update! session v3 13 false)
      (is (= 13
             (get-in (runtime/read-tui-view @session {:client-id "debug"})
                     [:blocks 1 :value]))))))

(deftest export-import-round-trip-preserves-multi-client-commit-order
  (let [manifest
        {:schema replay/schema
         :schema-version replay/schema-version
         :clients [{:client-id "A" :mode :versioned-premise}
                   {:client-id "B" :mode :versioned-premise}]
         :commits [{:commit-id "00000000-0000-0000-0000-000000000011"
                    :client-id "B" :index 0 :expected-version nil :text "2"}
                   {:commit-id "00000000-0000-0000-0000-000000000012"
                    :client-id "A" :index 0 :expected-version nil :text "1"}]}
        round-trip (json/read-json (json/write-json manifest))
        session (runtime/new-session)
        result (runtime/import-instance! session round-trip)
        exported (runtime/export-instance @session)]
    (is (= :imported (:status result)))
    (is (= ["B" "A"]
           (mapv :client-id (:versioned/commit-log @session))))
    (is (= ["A" "B"] (mapv :client-id (:clients exported))))
    (is (= ["1" 1 value/nothing]
           (mapv :value (get-in exported [:snapshot :views 0 :blocks]))))
    (is (= ["2" 2 value/nothing]
           (mapv :value (get-in exported [:snapshot :views 1 :blocks]))))))

(deftest import-is-atomic-idempotent-and-rejects-divergence
  (testing "a later failed commit publishes none of the candidate"
    (let [session (runtime/new-session)
          manifest (update (fixture-manifest) :commits
                           #(conj (vec (take 1 %))
                                  {:commit-id
                                   "00000000-0000-0000-0000-000000000099"
                                   :client-id "debug"
                                   :index 0
                                   :expected-version 0
                                   :text "("}))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"compilation failed"
                            (runtime/import-instance! session manifest)))
      (is (nil? @session))))
  (testing "exact replay is mutation-free and divergent history is rejected"
    (let [session (runtime/new-session)
          manifest (fixture-manifest)]
      (runtime/import-instance! session manifest)
      (let [before @session
            replayed (runtime/import-instance! session manifest)]
        (is (= :replayed (:status replayed)))
        (is (identical? before @session)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"diverges"
           (runtime/import-instance!
            session
            (assoc-in manifest [:commits 3 :text] "(+ 40 10)")))))))

(deftest json-socket-rejects-malformed-schema-without-mutating-and-closes
  (let [runtime-server (start-json-runtime)
        port (:json-port runtime-server)
        session (:session runtime-server)]
    (try
      (let [response (json-server/request
                      "127.0.0.1" port
                      {:op "instance/import"
                       :manifest {:schema "wrong"
                                  :schema-version 1
                                  :clients []
                                  :commits []}})]
        (is (false? (:ok response)))
        (is (re-find #"schema" (:error response)))
        (is (nil? @session)))
      (finally
        ((:close runtime-server))))
    (is (thrown? java.net.ConnectException
                 (json-server/request "127.0.0.1" port
                                      {:op "instance/export"})))))
