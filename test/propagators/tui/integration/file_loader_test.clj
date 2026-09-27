(ns propagators.tui.integration.file-loader-test
  (:require [propagators.tui.assembly :as assembly]
            [clojure.test :refer [deftest is]]
            [propagators.runtime :as runtime]
            [propagators.runtime.session.file-loader :as loader]
            [propagators.tui.adapters.bridge.web :as bridge]
            [propagators.tui.graph.compiler-2-runtime-server :as server]
            [propagators.compiler.model.env :as cenv]
            [propagators.infra.network :as net]))

(def demo-file "examples/lain/demo.lain")

(deftest demo-lain-info-form-loads
  (let [session (assembly/new-session)
        info-source (-> demo-file
                        loader/read-file-source
                        loader/source-forms
                        first
                        pr-str)
        client-id "printer"]
    (loader/load-source! session info-source {:client-id client-id})
    (is (some? (cenv/resolve-binding-id
                (:program/net @session)
                (:program/env @session)
                'info)))))

(deftest runtime-server-loads-lain-file-command
  (let [{:keys [port close]} (server/start-server 0)
        client-id "printer"]
    (try
      (let [response (server/request server/default-host
                                     port
                                     {:op :compile/load-file
                                      :file demo-file
                                      :client-id client-id})]
        (is (:ok response))
        (is (= client-id (get-in response [:result :client-id])))
        (is (re-find #"demo\.lain$"
                     (get-in response [:result :file]))))
      (finally
        (close)))))

(deftest runtime-server-startup-load-option-loads-lain-file
  (let [temporary (java.io.File/createTempFile "compiler2-startup-" ".lain")
        _ (spit temporary "(def info 1)")
        startup-file (.getAbsolutePath temporary)
        client-id "printer"
        opts (#'server/parse-server-args
              ["--load" startup-file
               "--load-client" client-id
               "--load-blocks" "1"])
        {:keys [close] :as server-state} (server/start-server 0)]
    (try
      (is (= startup-file (:load-file opts)))
      (is (= client-id (:load-client-id opts)))
      (let [loaded (#'server/load-startup-file! server-state opts)]
        (is (= client-id (:client-id loaded)))
        (is (= startup-file (:file loaded))))
      (finally
        (close)
        (.delete temporary)))))

(deftest runtime-server-watch-loads-a-fresh-relationship-environment
  (let [temporary (java.io.File/createTempFile "compiler2-watch-server-" ".lain")
        _ (spit temporary
                "(def-cells a graph) (<-> 1 a) (relationship:roots a graph) (xr:io graph)")
        path (.getAbsolutePath temporary)
        opts (#'server/parse-server-args ["--watch" path "--no-dashboard"])
        server-state (binding [server/*temperature-logger-enabled?* false]
                       (server/start-server 0))]
    (try
      (is (:xr? opts))
      (is (= path (:load-file opts)))
      (is (= path (:watch-file opts)))
      (#'server/load-startup-file! server-state opts)
      (#'server/start-file-watch! server-state opts)
      (is (some? @(:file-watch-state server-state)))
      (is (seq (get-in @(:session server-state) [:xr :effects])))
      (finally
        ((:close server-state))
        (.delete temporary)))))

(deftest lain-source-normalizer-accepts-consecutive-top-level-forms
  (is (= ['(def-cell x) '(<-> 1 x)]
         (loader/source-forms "(def-cell x)\n(<-> 1 x)"))))

(deftest lain-loader-preserves-block-by-block-def-net-application
  (let [session (assembly/new-session)
        source "(def-cell clients)
                (runtime:clients clients)
                (def-net first-client [clients] [out]
                  (p:car clients))
                (def-cell f)
                (first-client clients f)"]
    (loader/load-source! session source {:client-id "file-test"})
    (runtime/commit-runtime-input! session
                                   {:runtime/input :cell-message
                                    :cell-id (bridge/client-list-source-id)
                                    :update (bridge/linked-list-value ["A" "B"])})
      (is (= (bridge/client-handle "A")
           (net/network-cell-strongest
            (:program/net @session)
            (cenv/resolve-binding-id
             (:program/net @session)
             (:program/env @session)
             'f))))))

(defn- binding-value
  [session symbol]
  (let [state @session]
    (net/network-cell-strongest
     (:program/net state)
     (cenv/resolve-binding-id (:program/net state)
                              (:program/env state)
                              symbol))))

(deftest fresh-replacement-discards-the-previous-environment
  (let [session (loader/load-session-from-source "(def old-name 1)")]
    (loader/replace-session-from-source! session "(def new-name 2)")
    (is (nil? (cenv/resolve-binding-id
               (:program/net @session) (:program/env @session) 'old-name)))
    (is (= 2 (binding-value session 'new-name)))))

(deftest failed-replacement-retains-the-last-good-environment
  (let [session (loader/load-session-from-source "(def stable 7)")
        before @session]
    (is (thrown? Throwable
                 (loader/replace-session-from-source! session "(def stable")))
    (is (identical? before @session))
    (is (= 7 (binding-value session 'stable)))))

(deftest watched-file-promotes-only-successful-fresh-environments
  (let [temporary (java.io.File/createTempFile "compiler2-watch-" ".lain")
        reloaded (promise)
        failed (promise)
        _ (spit temporary "(def watched 1)")
        session (loader/load-session-from-file temporary)
        watcher (loader/watch-file!
                 session temporary
                 {:interval-ms 10
                  :on-reload #(deliver reloaded %)
                  :on-error #(deliver failed %)})]
    (try
      (spit temporary "(def watched 2)")
      (is (not= ::timeout (deref reloaded 2000 ::timeout)))
      (is (= 2 (binding-value session 'watched)))
      (spit temporary "(def watched")
      (is (instance? Throwable (deref failed 2000 ::timeout)))
      (is (= 2 (binding-value session 'watched)))
      (finally
        ((:close watcher))
        (.delete temporary)))))
