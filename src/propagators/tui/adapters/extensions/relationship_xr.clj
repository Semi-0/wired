(ns propagators.tui.adapters.extensions.relationship-xr
  "Opt-in Compiler-2 bindings for relationship observation and XR publication."
  (:require [propagators.runtime.ids :as runtime-ids]
            [propagators.runtime.operators.relationship-observer :as relationship-observer]
            [propagators.runtime.operators.visualizer :as visualizer]
            [propagators.tui.adapters.operators.xr :as xr]
            [propagators.runtime.session.extension :as extension]))

(def relationship-endpoint "/api/relationships")

(defn extension
  []
  (extension/extension-bundle
   {:id :compiler-2/relationship-xr
    :bindings
    [['relationship:roots (relationship-observer/roots-operator)]
     ['relationship:dataflow (relationship-observer/dataflow-operator)]
     ['cell-window (visualizer/cell-window-operator)]
     ['cell-history (visualizer/cell-history-operator)]
     ['propagator-references (visualizer/propagator-references-operator)]
     ['hierarchy (visualizer/hierarchy-operator)]
     ['juxtapose (visualizer/juxtapose-operator)]
     ['xr:io (xr/xr-io-operator (runtime-ids/boundary-outbox-id))]
     ['relationship-endpoint relationship-endpoint]]
    :effects []}))
