(ns propagators.tui.integration.relationship-observer-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.compiler.model.env :as cenv]
            [propagators.tui.adapters.extensions.relationship-xr :as relationship-xr]
            [propagators.runtime.session.file-loader :as loader]
            [propagators.infra.network :as net]
            [propagators.infra.relationship :as relationship]
            [propagators.infra.relationship-observer :as relationship-observer]
            [propagators.infra.semantic-trace :as semantic-trace]))

(deftest relationship-extension-compiles-a-composed-root-observer
  (let [session
        (loader/load-session-from-source
          "(def-cells a b graph)
          (<-> 1 a)
          (-> (+ a 1) b)
          (relationship:roots a graph)
          (xr:io graph)"
         {:client-id "relationship-test"
          :extensions [(relationship-xr/extension)]})
        network (:program/net @session)
        graph-id (cenv/resolve-binding-id network
                                           (:program/env @session)
                                           'graph)
        a-id (cenv/resolve-binding-id network (:program/env @session) 'a)
        b-id (cenv/resolve-binding-id network (:program/env @session) 'b)
        traced (net/network-cell-strongest network graph-id)
        traced-ids (set (map second (keys (:nodes traced))))
        labels (set (vals (:nodes traced)))]
    (is (semantic-trace/semantic-trace-graph? traced))
    (is (contains? traced-ids a-id))
    (is (contains? traced-ids b-id))
    (is (every? #(empty? (relationship/parents
                          (net/net-relationship network) %))
                (keys (:nodes traced))))
    (is (not-any? #(= relationship-observer/observer-name %)
                  (vals (:nodes traced))))
    (is (contains? labels '+))
    (is (contains? labels '->))
    (is (seq (:edges traced)))
    (is (seq (get-in @session [:xr :effects])))
    (is (= "/api/relationships"
           relationship-xr/relationship-endpoint))))
