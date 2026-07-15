(ns graph.compiler-2-versioned-tui-bench
  "Diagnostic topology/latency receipt for versioned block commits."
  (:require [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-runtime.block-model :as block-model]
            [graph.compiler-2-runtime.input :as input]
            [graph.compiler-2-runtime.program :as program]
            [graph.compiler-2-runtime.temperature :as temperature]
            [graph.compiler-2-runtime.version-history :as history]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [propagators.compiler-2.main :as compiler]
            [propagators.compiler-2.model.application-value :as application-value]
            [propagators.compiler-2.operators.versioned-definition :as definition]
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.gur.flat :as fvm]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]
            [propagators.semantic-trace :as semantic-trace])
  (:import [java.util UUID]))

(defn topology-counts
  [runtime-state]
  (let [entries (vals (net/net-env (:program/net runtime-state)))
        named-count (fn [name]
                      (count (filter #(and (prop/prop? %)
                                           (= name (prop/prop-name %)))
                                     entries)))
        block-gates (named-count :compiler-2/block-premise)
        application-premises (named-count :compiler-2/application-premise)]
    {:cells (count (remove prop/prop? entries))
     :propagators (count (filter prop/prop? entries))
     :premise-gates (+ block-gates application-premises)
     :block-premise-gates block-gates
     :application-premises application-premises
     :retained-candidates
     (count (net/network-dict-entry (:program/net runtime-state)
                                    definition/candidates-key))
     :placeholder-cells
     (->> (vals (get (net/network-dict-entry
                      (:program/net runtime-state) fvm/name-bindings-key)
                     definition/call-metadata-scope {}))
          (mapcat :placeholder-ids) set count)
     :warnings
     (->> (vals (get (net/network-dict-entry
                      (:program/net runtime-state) fvm/name-bindings-key)
                     definition/diagnostic-scope {}))
          (mapcat identity) count)}))

(defn propagation-totals [runtime-state]
  (into {}
        (map (fn [[phase samples]]
               [phase {:runs (count samples)
                       :queued (reduce + (map :queued samples))
                       :ms (reduce + (map :ms samples))}]))
        (group-by :phase (get-in runtime-state
                                 [:runtime :temperature :samples]))))

(defn- timed [profile phase f]
  (fn [& args]
    (let [started (System/nanoTime)]
      (try
        (apply f args)
        (finally
          (swap! profile update phase
                 (fn [{:keys [calls ms] :or {calls 0 ms 0.0}}]
                   {:calls (inc calls)
                    :ms (+ ms (/ (double (- (System/nanoTime) started))
                                 1000000.0))})))))))

(defn benchmark-edits
  [edits]
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "bench"
                                    :mode :versioned-premise})
    (let [started (System/nanoTime)]
      (dotimes [version edits]
        (runtime/commit-version!
         session
         {:commit-id (str (UUID/randomUUID))
          :client-id "bench"
          :index 0
          :expected-version (when (pos? version) (dec version))
          :text (str version)}))
      (merge {:edits edits
              :commit-ms (/ (double (- (System/nanoTime) started)) 1000000.0)
              :retained-versions (count (get-in @session
                                                [:tuis "bench" :blocks 0
                                                 :version-history]))}
             (topology-counts @session)))))

(defn benchmark-definition-and-application-edits [edits]
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "bench"
                                    :mode :versioned-premise})
    (let [started (System/nanoTime)]
      (dotimes [version edits]
        (runtime/commit-version!
         session
         {:commit-id (str (UUID/randomUUID))
          :client-id "bench" :index 0
          :expected-version (when (pos? version) (dec version))
          :text (str "(def-net f [x] [out] (-> (+ x " version ") out))")})
        (runtime/commit-version!
         session
         {:commit-id (str (UUID/randomUUID))
          :client-id "bench" :index 1
          :expected-version (when (pos? version) (dec version))
          :text "(let-cell [out] (f 1 out) out)"}))
      (merge {:edits edits
              :definition-and-application-commits (* 2 edits)
              :commit-ms (/ (double (- (System/nanoTime) started)) 1000000.0)
              :retained-versions
              (+ (count (get-in @session [:tuis "bench" :blocks 0
                                          :version-history]))
                 (count (get-in @session [:tuis "bench" :blocks 1
                                          :version-history])))}
             {:propagation (propagation-totals @session)}
             (topology-counts @session)))))

(defn- current-application-io
  [runtime-state client-id block-index]
  (let [record (peek (get-in runtime-state
                             [:tuis client-id :blocks block-index
                              :version-history]))
        application-id (first (get-in record [:topology :application-ids]))
        application (net/network-cell-strongest (:program/net runtime-state)
                                                application-id)]
    {:input-id
     (first (obj/slot-value application
                            application-value/application-arg-cells-slot))
     :output-id (get-in record [:topology :result-cell])}))

(defn benchmark-definition-edits-existing-application-input
  "Compile one application, edit only its definition, then time one late input."
  [edits]
  (when-not (pos? edits)
    (throw (ex-info "benchmark expects at least one edit" {:edits edits})))
  (let [session (runtime/new-session)
        client-id "bench-existing-application"
        commit! #(runtime/commit-version! session %)]
    (runtime/register-tui! session {:client-id client-id
                                    :mode :versioned-premise})
    (commit! {:commit-id (str (UUID/randomUUID))
              :client-id client-id :index 0 :expected-version nil
              :text "(def-net f [x] [out] (-> (+ x 0) out))"})
    (commit! {:commit-id (str (UUID/randomUUID))
              :client-id client-id :index 1 :expected-version nil
              :text "(let-cell [x out] (f x out) out)"})
    (doseq [version (range 1 edits)]
      (commit! {:commit-id (str (UUID/randomUUID))
                :client-id client-id :index 0
                :expected-version (dec version)
                :text (str "(def-net f [x] [out] (-> (+ x " version
                           ") out))")}))
    (let [runtime-state @session
          {:keys [input-id output-id]}
          (current-application-io runtime-state client-id 1)
          activations (atom {})
          eval-propagator core/eval-propagator
          started (System/nanoTime)
          updated
          (with-redefs
            [core/eval-propagator
             (fn [id tasks network]
               (let [name (some-> (net/network-env-lookup network id)
                                  prop/prop-name)]
                 (swap! activations update name
                        (fn [{:keys [calls ids] :or {calls 0 ids #{}}}]
                          {:calls (inc calls) :ids (conj ids id)})))
               (eval-propagator id tasks network))]
            (input/apply-program-updates runtime-state
                                         [{:cell-id input-id :update 7}]))
          elapsed (/ (double (- (System/nanoTime) started)) 1000000.0)
          result (net/network-cell-strongest (:program/net updated) output-id)
          result-value (if (= :distributed-projection (:tms/kind result))
                         (:tms/value result)
                         result)
          expected (+ 7 (dec edits))]
      (when-not (= expected result-value)
        (throw (ex-info "late application input produced the wrong value"
                        {:expected expected :actual result-value})))
      (merge {:definition-edits edits
              :application-edits 1
              :input-ms elapsed
              :input 7
              :result result-value
              :activations (reduce + (map (comp :calls val) @activations))
              :activation-counts
              (into {} (map (fn [[name {:keys [calls]}]] [name calls]))
                    @activations)
              :distinct-activation-counts
              (into {} (map (fn [[name {:keys [ids]}]] [name (count ids)]))
                    @activations)}
             (topology-counts updated)))))

(defn- timed-commit! [session request]
  (let [started (System/nanoTime)]
    (runtime/commit-version! session request)
    (/ (double (- (System/nanoTime) started)) 1000000.0)))

(defn benchmark-edit-latencies [edits]
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "bench"
                                    :mode :versioned-premise})
    (mapv
     (fn [version]
       (let [expected (when (pos? version) (dec version))
             definition-ms
             (timed-commit!
              session
              {:commit-id (str (UUID/randomUUID))
               :client-id "bench" :index 0 :expected-version expected
               :text (str "(def-net f [x] [out] (-> (+ x " version
                          ") out))")})
             application-ms
             (timed-commit!
              session
              {:commit-id (str (UUID/randomUUID))
               :client-id "bench" :index 1 :expected-version expected
               :text "(let-cell [out] (f 1 out) out)"})
             definition-record (peek (get-in @session
                                             [:tuis "bench" :blocks 0
                                              :version-history]))
             application-record (peek (get-in @session
                                              [:tuis "bench" :blocks 1
                                               :version-history]))]
         {:version version
          :definition-ms definition-ms
          :application-ms application-ms
          :definition-props (count (get-in definition-record
                                           [:topology :propagator-ids]))
          :application-props (count (get-in application-record
                                            [:topology :propagator-ids]))}))
     (range edits))))

(defn profiled-definition-and-application-edits [edits]
  (let [profile (atom {})
        compile-program-form program/compile-program-form
        compile-source compiler/compile-source
        compiled-semantic-graph semantic-repl/compiled-semantic-graph
        graph-union semantic-trace/graph-union
        apply-program-updates input/apply-program-updates
        run-propagators nb/run-propagators
        candidates-for-block-version definition/candidates-for-block-version
        registry-candidates definition/registry-candidates
        commit-by-id history/commit-by-id
        all-blocks block-model/all-blocks]
    (with-redefs [program/compile-program-form
                  (timed profile :compile-program-form compile-program-form)
                  compiler/compile-source
                  (timed profile :compile-source compile-source)
                  semantic-repl/compiled-semantic-graph
                  (timed profile :compiled-semantic-graph compiled-semantic-graph)
                  semantic-trace/graph-union
                  (timed profile :graph-union graph-union)
                  input/apply-program-updates
                  (timed profile :apply-program-updates apply-program-updates)
                  nb/run-propagators
                  (timed profile :run-propagators run-propagators)
                  definition/candidates-for-block-version
                  (timed profile :candidates-for-block-version
                         candidates-for-block-version)
                  definition/registry-candidates
                  (timed profile :registry-candidates registry-candidates)
                  history/commit-by-id
                  (timed profile :commit-by-id commit-by-id)
                  block-model/all-blocks
                  (timed profile :all-blocks all-blocks)]
      (assoc (benchmark-definition-and-application-edits edits)
             :profile @profile))))

(defn profiled-propagator-activations
  "Benchmark edit commits and group full activation cost by propagator name."
  [edits]
  (let [profile (atom {})
        eval-propagator core/eval-propagator
        timed-eval
        (fn [current-id tasks network]
          (let [name (some-> (net/network-env-lookup network current-id)
                             prop/prop-name)
                started (System/nanoTime)]
            (try
              (eval-propagator current-id tasks network)
              (finally
                (let [elapsed (/ (double (- (System/nanoTime) started))
                                 1000000.0)]
                  (swap! profile update name
                         (fn [{:keys [calls ms max-ms]
                               :or {calls 0 ms 0.0 max-ms 0.0}}]
                           {:calls (inc calls)
                            :ms (+ ms elapsed)
                            :max-ms (max max-ms elapsed)})))))))]
    (with-redefs [core/eval-propagator timed-eval]
      (assoc (benchmark-definition-and-application-edits edits)
             :activation-profile @profile))))

(defn -main [& _]
  (doseq [edits [1 10 50]]
    (prn (profiled-definition-and-application-edits edits))))
