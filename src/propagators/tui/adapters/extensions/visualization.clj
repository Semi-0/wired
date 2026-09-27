(ns propagators.tui.adapters.extensions.visualization
  (:require [propagators.runtime.session.extension :as session]
            [propagators.runtime.ids :as runtime-ids]
            [propagators.tui.adapters.operators.xr :as xr]
            [propagators.runtime.operators.relationship-observer :as relationship]
            [propagators.compiler.experimental.visualization.collections :as collections]
            [propagators.compiler.experimental.visualization.observation :as observation]
            [propagators.compiler.experimental.visualization.presentation :as presentation]
            [propagators.compiler.experimental.visualization.zoom :as zoom]))

(def extension
  (session/extension-bundle
   {:id ::collections
    :bindings [['map (collections/collection-operator :map)]
               ['filter (collections/collection-operator :filter)]
               ['transpose (collections/transpose-operator)]
               ['inputs-of observation/inputs-of]
               ['outputs-of observation/outputs-of]
               ['occurrence-of observation/occurrence-of]
               ['member? observation/member?]
               ['strongest-of observation/strongest-of]
               ['focus zoom/focus]
               ['sources-of zoom/sources-of]
               ['juxtapose presentation/juxtapose]
               ['selectable presentation/selectable]
               ['relationship:dataflow (relationship/dataflow-operator)]
               ['relationship:roots (relationship/roots-operator)]
               ['xr:io (xr/xr-io-operator (runtime-ids/boundary-outbox-id))]]
    :effects []}))
