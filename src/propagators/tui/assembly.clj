(ns propagators.tui.assembly
  "Application assembly for UI-specific runtime adapters."
  (:require [propagators.runtime :as runtime]
            [propagators.tui.adapters.bridge.widget :as widget]
            [propagators.tui.adapters.bridge.xr-projection :as xr-projection]
            [propagators.tui.adapters.operators.web-bridge :as web-bridge]
            [propagators.tui.adapters.operators.xr :as xr]))

(defn- xr-bindings
  [{:keys [boundary-outbox-id]}]
  [['xr-io (xr/xr-io-operator boundary-outbox-id)]
   ['io:xr (xr/io-xr-operator boundary-outbox-id)]])

(defn- widget-bindings
  [{:keys [boundary-outbox-id]}]
  [['slider-io (widget/slider-io-operator boundary-outbox-id)]
   ['slider-panel-io (widget/slider-panel-io-operator boundary-outbox-id)]
   ['io:slider (widget/io-slider-operator boundary-outbox-id)]
   ['io:slider-panel (widget/io-slider-panel-operator boundary-outbox-id)]
   ['io:slider-panels (widget/io-slider-panel-operator boundary-outbox-id)]
   ['io:slider-panel-name
    (widget/io-slider-panel-name-operator boundary-outbox-id)]])

(defn- web-client-bindings
  [_]
  [['runtime:clients (web-bridge/runtime-clients-operator)]
   ['runtime:client-pipe (web-bridge/runtime-client-pipe-operator)]])

(defn runtime-options
  []
  {:operator-groups [:xr :widget :web-client]
   :operator-providers {:xr xr-bindings
                        :widget widget-bindings
                        :web-client web-client-bindings}
   :command-handlers
   {:xr/effects
    (fn [session _]
      (xr-projection/read-xr-effects @session))}})

(defn new-session
  []
  (runtime/new-session (runtime-options)))

(def project-xr-effects xr-projection/project-xr-effects)
(def read-xr-effects xr-projection/read-xr-effects)
