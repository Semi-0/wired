(ns graph.compiler-2-runtime.commands
  "Public command dispatch for compiler-2 runtime."
  (:require [graph.compiler-2-runtime.cells :as cells]
            [graph.compiler-2-runtime.input :as input]
            [graph.compiler-2-runtime.program :as program]
            [graph.compiler-2-runtime.state :as state]
            [graph.compiler-2-runtime.trace-session :as trace-session]
            [graph.compiler-2-runtime.tui-session :as tui-session]
            [graph.compiler-2-runtime.xr-projection :as xr-projection]))

(def require-state state/require-state)
(def register-tui! tui-session/register-tui!)
(def append-tui-block! tui-session/append-tui-block!)
(def edit-tui-block! tui-session/edit-tui-block!)
(def submit-tui-block! tui-session/submit-tui-block!)
(def read-tui-view tui-session/read-tui-view)
(def unregister-tui! tui-session/unregister-tui!)
(def read-agent-blocks tui-session/read-agent-blocks)
(def read-agent-block tui-session/read-agent-block)
(def send-agent-block! tui-session/send-agent-block!)
(def compile-source! program/compile-source!)
(def list-cells cells/list-cells)
(def read-cell cells/read-cell)
(def semantic-trace trace-session/semantic-trace)
(def semantic-expansion trace-session/semantic-expansion)
(def install-semantic-trace! trace-session/install-semantic-trace!)
(def read-installed-trace trace-session/read-installed-trace)
(def stop-installed-trace! trace-session/stop-installed-trace!)
(def read-xr-effects xr-projection/read-xr-effects)

(defn handle-command!
  [session {:keys [op source] :as command}]
  (try
    (locking session
      {:ok true
       :result
       (case op
         :tui/register (register-tui! session command)
         :tui/append-block (append-tui-block! session command)
         :tui/edit-block (edit-tui-block! session command)
         :tui/submit-block (submit-tui-block! session command)
         :tui/read-view (read-tui-view @session command)
         :tui/unregister (unregister-tui! session command)
         :agent/blocks (read-agent-blocks @session command)
         :agent/block (read-agent-block @session command)
         :agent/send-block (send-agent-block! session command)
         :compile/source (compile-source! session source)
         :cells/list (list-cells @session)
         :cell/read (read-cell @session command)
         :semantic/graph (:graph (require-state @session))
         :semantic/trace (semantic-trace @session command)
         :semantic/expand (semantic-expansion @session command)
         :semantic/trace/install (install-semantic-trace! session command)
         :semantic/trace/read (read-installed-trace @session command)
         :semantic/trace/stop (stop-installed-trace! session command)
         :xr/effects (read-xr-effects @session)
         (throw (ex-info "unknown runtime op" {:op op})))})
    (catch Throwable t
      {:ok false
       :error (ex-message t)
       :data (ex-data t)})))
