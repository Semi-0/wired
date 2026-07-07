(ns graph.compiler-2-runtime
  "Shared compiler-2 runtime session for socket clients."
  (:require [graph.compiler-2-runtime.cells :as cells]
            [graph.compiler-2-runtime.commands :as commands]
            [graph.compiler-2-runtime.effects :as effects]
            [graph.compiler-2-runtime.input :as input]
            [graph.compiler-2-runtime.program :as program]
            [graph.compiler-2-runtime.state :as state]
            [graph.compiler-2-runtime.temperature :as temperature]
            [graph.compiler-2-runtime.trace-session :as trace-session]
            [graph.compiler-2-runtime.trace-subscriptions :as trace-subscriptions]
            [graph.compiler-2-runtime.tui-session :as tui-session]
            [graph.compiler-2-runtime.xr-projection :as xr-projection]))

(def new-session state/new-session)
(def ensure-session-state! state/ensure-session-state!)
(def record-runtime-error! state/record-runtime-error!)
(def default-xr-client-id state/default-xr-client-id)
(def compile-source! program/compile-source!)
(def list-cells cells/list-cells)
(def read-cell cells/read-cell)
(def run-runtime-cycle effects/run-runtime-cycle)
(def commit-runtime-input input/commit-runtime-input)
(def commit-runtime-input! input/commit-runtime-input!)
(def extend-source! input/extend-source!)
(def semantic-trace trace-session/semantic-trace)
(def semantic-expansion trace-session/semantic-expansion)
(def install-semantic-trace! trace-session/install-semantic-trace!)
(def read-installed-trace trace-session/read-installed-trace)
(def stop-installed-trace! trace-session/stop-installed-trace!)
(def schedule-trace-refreshes! trace-subscriptions/schedule-refreshes!)
(def refresh-trace-subscriptions! trace-subscriptions/refresh-now!)
(def summarize-temperature temperature/summarize)
(def drain-temperature! temperature/drain-summary!)
(def register-tui! tui-session/register-tui!)

(defn- with-trace-refresh!
  [session result]
  (refresh-trace-subscriptions! session)
  result)

(defn append-tui-block!
  [session command]
  (with-trace-refresh! session
    (tui-session/append-tui-block! session command)))

(defn edit-tui-block!
  [session command]
  (with-trace-refresh! session
    (tui-session/edit-tui-block! session command)))

(defn submit-tui-block!
  [session command]
  (with-trace-refresh! session
    (tui-session/submit-tui-block! session command)))

(def project-tui-view tui-session/project-tui-view)
(def read-tui-view tui-session/read-tui-view)
(def read-agent-blocks tui-session/read-agent-blocks)
(def read-agent-block tui-session/read-agent-block)
(def send-agent-block! tui-session/send-agent-block!)
(def unregister-tui! tui-session/unregister-tui!)
(def project-xr-effects xr-projection/project-xr-effects)
(def read-xr-effects xr-projection/read-xr-effects)
(def handle-command! commands/handle-command!)
