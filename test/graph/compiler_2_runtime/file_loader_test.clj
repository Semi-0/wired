(ns graph.compiler-2-runtime.file-loader-test
  (:require [clojure.test :refer [deftest is]]
            [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-runtime.file-loader :as loader]
            [graph.compiler-2-runtime.tui-annotations :as annotations]
            [propagators.compiler-2.env :as cenv]
            [propagators.datastructures.event :as event]
            [propagators.network :as net]))

(def slider-file "examples/lain/slider-panel.lain")

(defn- commit-slider!
  [session channel value]
  (runtime/commit-runtime-input! session
                                 {:runtime/input :xr/widget-event
                                  :widget-id "slider-panel-0"
                                  :channel channel
                                  :value value}))

(defn- cell-content
  [session symbol]
  (let [cell-id (cenv/binding-id
                 (cenv/lookup (:program/env @session) symbol))]
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

(deftest lain-source-normalizer-accepts-consecutive-top-level-forms
  (is (= ['(def-cell x) '(<-> 1 x)]
         (loader/source-forms "(def-cell x)\n(<-> 1 x)"))))
