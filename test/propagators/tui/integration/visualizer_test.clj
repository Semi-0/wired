(ns propagators.tui.integration.visualizer-test
  (:require [clojure.test :refer [deftest is testing]]
            [propagators.compiler.model.env :as cenv]
            [propagators.tui.adapters.extensions.relationship-xr :as relationship-xr]
            [propagators.runtime.session.file-loader :as loader]
            [propagators.infra.network :as net]
            [propagators.infra.relationship :as relationship]
            [propagators.infra.visualizer :as visualizer]))

(defn- binding-value
  [session symbol]
  (let [{:keys [program/net program/env]} @session
        id (cenv/resolve-binding-id net env symbol)]
    (net/network-cell-strongest net id)))

(deftest lain-visualizers-compose-over-a-compound-propagator
  (let [session
        (loader/load-session-from-source
         "(def-cells source result refs tree window history dashboard)
          (<-> 1 source)
          (def-net inc [x] [out]
            (-> (+ x 1) out))
          (inc source result)
          (propagator-references result :inputs refs)
          (hierarchy refs tree)
          (cell-window result window)
          (cell-history result history)
          (juxtapose window tree history dashboard)
          (xr:io dashboard)"
         {:client-id "visualizer-test"
          :extensions [(relationship-xr/extension)]})
        network (:program/net @session)
        references (visualizer/resolve-reference-set
                    network
                    (binding-value session 'refs))
        hierarchy (visualizer/resolve-view network
                                             (binding-value session 'tree))
        dashboard (visualizer/resolve-view network
                                             (binding-value session 'dashboard))
        roots (set (:view/roots hierarchy))
        descendants (set (keys (get-in hierarchy [:view/graph :nodes])))]
    (testing "declarations stay composable data until a view boundary resolves them"
      (is (seq references))
      (is (= [:cell-window :hierarchy :cell-history]
             (mapv :view/type (:view/resolved-children dashboard))))
      (is (= 2 (get-in dashboard [:view/resolved-children 0 :view/strongest])))
      (is (seq (get-in dashboard [:view/resolved-children 2 :view/samples])))
      (is (some #(= :xr/present-view (:boundary/kind %))
                (get-in @session [:xr :effects]))))
    (testing "a Lain network expression exposes the compound propagator's children"
      (is (some #(seq (relationship/children
                       (net/net-relationship network) %))
                roots))
      (is (every? #(contains? descendants %) roots))
      (is (seq (remove roots descendants))))))

(deftest propagator-reference-direction-is-exhaustive
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"must be :inputs or :outputs"
       (visualizer/reference-set :source :sideways))))
