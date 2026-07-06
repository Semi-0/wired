(ns graph.compiler-2-runtime.state
  "Shared compiler-2 runtime state primitives."
  (:require [graph.compiler-2-runtime.boundary :as boundary]
            [graph.compiler-2-runtime.ids :as runtime-ids]
            [graph.vijual-compiler-2-demo :as demo]
            [propagators.cells.cell-protocol :as cell-protocol]
            [propagators.compile :as compile1]
            [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.network :as net])
  (:import [java.util.concurrent Executors]))

(defn empty-graph []
  {:nodes {} :edges [] :values {} :expansions {}})

(defn new-session []
  (atom nil))

(def default-xr-client-id runtime-ids/default-xr-client-id)

(def external-source-client-id runtime-ids/external-source-client-id)

(defn stable-node-id [& parts]
  (apply runtime-ids/stable-node-id parts))

(defn runtime-graph-id []
  (runtime-ids/runtime-graph-id))

(defn boundary-outbox-id []
  (runtime-ids/boundary-outbox-id))

(defn receipt-slot-key [effect-id]
  (runtime-ids/receipt-slot-key effect-id))

(defn effect-slot-key [effect-id]
  (runtime-ids/effect-slot-key effect-id))

(defn xr-receipt
  [request status]
  (boundary/xr-receipt request status))

(defn daemon-executor
  [name]
  (Executors/newSingleThreadScheduledExecutor
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r]
       (doto (Thread. ^Runnable r name)
         (.setDaemon true))))))

(defn install-runtime-protocols
  [n]
  (-> n
      (compile1/install-and-run (cell-protocol/install-cell-protocol))
      (compile1/install-and-run (cell-protocol/install-event-protocol))
      (compile1/install-and-run (cell-protocol/install-behavior-protocol))
      (compile1/install-and-run (cell-protocol/install-tms-distributed-protocol))))

(defn runtime-base-net []
  (install-runtime-protocols net/empty-net))

(defn runtime-compiler-env []
  ((requiring-resolve
    'propagators.compiler-2.behavior/bind-behavior-operators)
   (compiler-helpers/default-env)))

(defn empty-state []
  {:network (install-runtime-protocols net/empty-net)
   :program/net (runtime-base-net)
   :program/env (runtime-compiler-env)
   :program/graph {:nodes {} :edges [] :values {} :expansions {}}
   :program/results {}
   :program/epoch 0
   :runtime/commit-tick 0
   :runtime/full-rebuild-fallbacks 0
   :block-order []
   :next-order 0
   :traces {}
   :xr {:launched {}}
   :tuis {}})

(defn ensure-session-state! [session]
  (when-not @session
    (reset! session (empty-state)))
  @session)

(defn- preserve-xr-traces
  [old-state new-state]
  (let [xr-traces (or (:xr/traces old-state)
                      (get-in old-state [:xr :traces]))]
    (if (and (seq xr-traces)
             (empty? (or (:xr/traces new-state)
                         (get-in new-state [:xr :traces]))))
      (-> new-state
          (assoc :xr/traces xr-traces)
          (assoc-in [:xr :traces] xr-traces))
      new-state)))

(defn mutate-session!
  [session f]
  (locking session
    (let [state @session
          state' (preserve-xr-traces state (f state))]
      (reset! session state')
      state')))

(defn require-state
  [state]
  (when-not state
    (throw (ex-info "no compiled source in runtime session" {})))
  state)

(defn labels
  [state]
  (let [compiled (:compiled state)
        program-net (:program/net state)]
    (if (and compiled program-net)
      (demo/compiled-labels compiled program-net)
      {})))
