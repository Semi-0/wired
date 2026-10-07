(ns propagators.tui.integration.file-loader-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.runtime :as runtime]
            [propagators.tui.assembly :as assembly]
            [propagators.runtime.session.file-loader :as loader]
            [propagators.runtime.inspection.annotations :as annotations]
            [propagators.tui.adapters.bridge.web :as bridge]
            [propagators.tui.graph.compiler-2-runtime-server :as server]
            [propagators.compiler.model.env :as cenv]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.network :as net]))

(def slider-file "dev/examples/lain/slider-panel.lain")
(def demo-file "dev/examples/lain/demo.lain")
(def demo-print-command "(print-lines info 0)")
(def demo-expected-text
  "knights-of-the-situation-calculus:\nhttps://discord.gg/aPRZfafAns")

(defn- commit-slider!
  [session channel value]
  (runtime/commit-runtime-input! session
                                 {:runtime/input :xr/widget-event
                                  :widget-id "slider-panel-0"
                                  :channel channel
                                  :value value}))

(defn- cell-content
  [session symbol]
  (let [cell-id (cenv/resolve-binding-id (:program/net @session)
                                         (:program/env @session)
                                         symbol)]
    (net/network-cell-content (:program/net @session) cell-id)))

(deftest lain-file-loads-pure-server-instance-and-slider-events-update-output
  (let [session (assembly/new-session)
        server {:session session :loaded (loader/load-file! session slider-file)}]
    (is (some? session))
    (is (= 9 (count (get-in server [:loaded :load-result :blocks]))))
    (is (= "slider-panel-0"
           (-> @session :xr :widgets keys first)))
    (commit-slider! session "a" 10)
    (commit-slider! session "b" 4)
    (commit-slider! session "c" 3)
    (let [d-content (cell-content session 'd)]
      (is (= [9] (vec (vals (event/active-values d-content)))))
      (is (= 9 (annotations/project-value d-content))))
    (commit-slider! session "a" 11)
    (let [d-content (cell-content session 'd)]
      (is (= [10] (vec (vals (event/active-values d-content)))))
      (is (= 10 (annotations/project-value d-content))))))

(deftest demo-lain-prints-info-cell-into-one-tui-block
  (let [session (runtime/new-session)
        client-id "printer"]
    (runtime/register-tui! session {:client-id client-id})
    (runtime/append-tui-block! session {:client-id client-id})
    (loader/load-file! session demo-file {:client-id client-id})
    (runtime/append-tui-block! session {:client-id client-id
                                        :text demo-print-command})
    (let [view (runtime/read-tui-view @session {:client-id client-id})]
      (is (= demo-expected-text (get-in view [:blocks 0 :value]))))))

(deftest runtime-server-loads-lain-file-command
  (let [{:keys [port close session]} (server/start-server 0)
        client-id "printer"]
    (try
      (runtime/register-tui! session {:client-id client-id})
      (runtime/append-tui-block! session {:client-id client-id})
      (let [response (server/request server/default-host
                                     port
                                     {:op :compile/load-file
                                      :file demo-file
                                      :client-id client-id})]
        (is (:ok response))
        (is (= client-id (get-in response [:result :client-id])))
        (is (re-find #"demo\.lain$"
                     (get-in response [:result :file]))))
      (runtime/append-tui-block! session {:client-id client-id
                                          :text demo-print-command})
      (let [view (runtime/read-tui-view @session {:client-id client-id})]
        (is (= demo-expected-text (get-in view [:blocks 0 :value]))))
      (finally
        (close)))))

(deftest runtime-server-startup-load-option-loads-lain-file
  (let [client-id "printer"
        opts (#'server/parse-server-args
              ["--load" demo-file
               "--load-client" client-id
               "--load-blocks" "1"])
        {:keys [close session] :as server-state} (server/start-server 0)]
    (try
      (is (= demo-file (:load-file opts)))
      (is (= client-id (:load-client-id opts)))
      (let [loaded (#'server/load-startup-file! server-state opts)]
        (is (= client-id (:client-id loaded)))
        (is (re-find #"demo\.lain$" (:file loaded))))
      (runtime/append-tui-block! session {:client-id client-id
                                          :text demo-print-command})
      (let [view (runtime/read-tui-view @session {:client-id client-id})]
        (is (= demo-expected-text (get-in view [:blocks 0 :value]))))
      (finally
        (close)))))

(deftest lain-source-normalizer-accepts-consecutive-top-level-forms
  (is (= ['(define x) '(<-> 1 x)]
         (loader/source-forms "(define x)\n(<-> 1 x)"))))

(deftest lain-loader-preserves-block-by-block-def-net-application
  (let [session (assembly/new-session)
        source "(define clients)\n(runtime:clients clients)\n(define first-client (network [clients out] (p:car out clients) (list out)))\n(define f)\n(first-client clients f)"]
    (loader/load-source! session source {:client-id "file-test"})
    (runtime/commit-runtime-input! session
                                   {:runtime/input :cell-message
                                    :cell-id (bridge/client-list-source-id)
                                    :update (bridge/linked-list-value ["A" "B"])})
    (is (= (bridge/client-handle "A")
           (net/network-cell-strongest
            (:program/net @session)
            (cenv/resolve-binding-id (:program/net @session)
                                     (:program/env @session) 'f))))))

(deftest runtime-server-watch-loads-a-fresh-relationship-environment
  (let [temporary (java.io.File/createTempFile "compiler2-watch-server-" ".lain")
        _ (spit temporary
                "(define a) (define graph) (<-> 1 a) (relationship:roots a graph) (xr:io graph)")
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


(defn- binding-value
  [session symbol]
  (let [state @session]
    (net/network-cell-strongest
     (:program/net state)
     (cenv/resolve-binding-id (:program/net state)
                              (:program/env state)
                              symbol))))

(deftest fresh-replacement-discards-the-previous-environment
  (let [session (loader/load-session-from-source "(define old-name 1)")]
    (loader/replace-session-from-source! session "(define new-name 2)")
    (is (nil? (cenv/resolve-binding-id
               (:program/net @session) (:program/env @session) 'old-name)))
    (is (= 2 (binding-value session 'new-name)))))

(deftest failed-replacement-retains-the-last-good-environment
  (let [session (loader/load-session-from-source "(define stable 7)")
        before @session]
    (is (thrown? Throwable
                 (loader/replace-session-from-source! session "(def stable")))
    (is (identical? before @session))
    (is (= 7 (binding-value session 'stable)))))

(deftest watched-file-promotes-only-successful-fresh-environments
  (let [temporary (java.io.File/createTempFile "compiler2-watch-" ".lain")
        reloaded (promise)
        failed (promise)
        _ (spit temporary "(define watched 1)")
        session (loader/load-session-from-file temporary)
        watcher (loader/watch-file!
                 session temporary
                 {:interval-ms 10
                  :on-reload #(deliver reloaded %)
                  :on-error #(deliver failed %)})]
    (try
      (spit temporary "(define watched 2)")
      (is (not= ::timeout (deref reloaded 2000 ::timeout)))
      (is (= 2 (binding-value session 'watched)))
      (spit temporary "(def watched")
      (is (instance? Throwable (deref failed 2000 ::timeout)))
      (is (= 2 (binding-value session 'watched)))
      (finally
        ((:close watcher))
        (.delete temporary)))))
