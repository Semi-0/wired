(ns propagators.compiler-2.runtime.session.file-loader-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.compiler-2.runtime :as runtime]
            [propagators.compiler-2.runtime.session.file-loader :as loader]
            [propagators.compiler-2.runtime.tui.annotations :as annotations]
            [propagators.compiler-2.runtime.bridge.web :as bridge]
            [graph.compiler-2-runtime-server :as server]
            [propagators.compiler-2.model.env :as cenv]
            [propagators.datastructures.event :as event]
            [propagators.network :as net]))

(def slider-file "examples/lain/slider-panel.lain")
(def demo-file "examples/lain/demo.lain")
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
  (let [cell-id (cenv/binding-id (cenv/lookup (:program/env @session) symbol))]
    (net/network-cell-content (:program/net @session) cell-id)))

(deftest lain-file-loads-pure-server-instance-and-slider-events-update-output
  (let [server (loader/load-server-instance slider-file)
        session (:session server)]
    (is (some? session))
    (is (= 6 (count (get-in server [:loaded :load-result :blocks]))))
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
  (is (= ['(def-cell x) '(<-> 1 x)]
         (loader/source-forms "(def-cell x)\n(<-> 1 x)"))))

(deftest lain-loader-preserves-block-by-block-def-net-application
  (let [session (runtime/new-session)
        source "(def-cell clients)
                (runtime:clients clients)
                (def-net first-client [clients] [out]
                  (p:car out clients))
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
            (cenv/binding-id
             (cenv/lookup (:program/env @session) 'f)))))))
