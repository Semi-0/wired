(ns graph.compiler-2-runtime.trace-subscriptions
  "External latest-result trace subscriptions for XR."
  (:require [graph.compiler-2-runtime.effects :as effects]
            [graph.compiler-2-runtime.program :as program]
            [graph.compiler-2-runtime.state :as state]
            [propagators.cells.cell :as cell]
            [propagators.datastructures.behavior :as behavior]
            [propagators.network :as net]
            [propagators.semantic-trace :as semantic-trace]))

(def mutate-session! state/mutate-session!)

(defonce worker-executor
  (state/daemon-executor "xr-trace-subscriptions"))

(defn runtime-epoch
  [state]
  (max (long (or (:program/epoch state) 0))
       (long (or (:runtime/commit-tick state) 0))))

(defn result-behavior
  [subscription-id epoch graph]
  (behavior/latest-value epoch
                         graph
                         #{[:xr/trace-subscription subscription-id]}))

(defn result-epoch
  [result]
  (some-> result :behavior behavior/strongest-value behavior/summary-latest-time))

(defn result-graph
  [result]
  (some-> result :behavior behavior/strongest-value behavior/base-value))

(defn- stale-result?
  [state subscription-id epoch]
  (let [current (get-in state [:trace/results subscription-id])
        current-epoch (long (or (result-epoch current) Long/MIN_VALUE))]
    (<= (long epoch) current-epoch)))

(defn- unchanged-result?
  [state subscription-id graph]
  (= graph (result-graph (get-in state [:trace/results subscription-id]))))

(defn publish-result-state
  [state {:keys [subscription epoch graph]}]
  (let [subscription-id (:id subscription)]
    (cond
      (not (get-in state [:trace/subscriptions subscription-id]))
      (update state :trace/stale-results (fnil inc 0))

      (stale-result? state subscription-id epoch)
      (update state :trace/stale-results (fnil inc 0))

      :else
      (let [trace-update (result-behavior subscription-id epoch graph)
            unchanged? (unchanged-result? state subscription-id graph)
            xr-traces (or (:xr/traces state)
                          (get-in state [:xr :traces]))
            n1 (net/assoc-net-cell (:program/net state)
                                   (:target-id subscription)
                                   (cell/cell trace-update trace-update))
            n2 (program/settle-application-props n1 [])]
        (-> state
            (assoc :program/net n2)
            (assoc-in [:trace/results subscription-id]
                      {:subscription-id subscription-id
                       :epoch epoch
                       :behavior trace-update})
            (update :trace/published-results (fnil inc 0))
            (cond-> unchanged?
              (update :trace/unchanged-results (fnil inc 0)))
            effects/perform-boundary-effects
            (cond-> (seq xr-traces)
              (assoc :xr/traces xr-traces)
              (seq xr-traces)
              (assoc-in [:xr :traces] xr-traces)))))))

(defn publish-result!
  [session result]
  (mutate-session! session #(publish-result-state % result)))

(defn- subscription-snapshot
  [state subscription]
  {:subscription subscription
   :epoch (runtime-epoch state)
   :graph (:graph state)})

(defn- needs-refresh?
  [state subscription]
  (let [epoch (runtime-epoch state)
        scheduled (long (or (:scheduled-epoch subscription) Long/MIN_VALUE))]
    (< scheduled epoch)))

(defn- mark-scheduled
  [state subscription]
  (assoc-in state
            [:trace/subscriptions (:id subscription) :scheduled-epoch]
            (runtime-epoch state)))

(defn- schedule-snapshots-state
  [state]
  (let [subscriptions (vals (:trace/subscriptions state))
        pending (filter #(needs-refresh? state %) subscriptions)]
    [(reduce mark-scheduled state pending)
     (mapv #(subscription-snapshot state %) pending)]))

(defn- compute-result
  [{:keys [subscription epoch graph]}]
  {:subscription subscription
   :epoch epoch
   :graph (semantic-trace/trace-graph graph
                                      (:request subscription))})

(defn schedule-refreshes!
  [session]
  (let [snapshots (atom [])]
    (mutate-session!
     session
     (fn [state]
       (let [[state' snapshots'] (schedule-snapshots-state state)]
         (reset! snapshots snapshots')
         state')))
    (doseq [snapshot @snapshots]
      (.submit worker-executor
               ^Runnable
                 (fn []
                   (try
                     (publish-result! session (compute-result snapshot))
                     (catch Throwable t
                       (state/record-runtime-error!
                        session
                        {:phase :xr/trace-subscription}
                        t))))))
    (count @snapshots)))

(defn refresh-now!
  [session]
  (let [snapshots (atom [])]
    (mutate-session!
     session
     (fn [state]
       (let [[state' snapshots'] (schedule-snapshots-state state)]
         (reset! snapshots snapshots')
         state')))
    (doseq [snapshot @snapshots]
      (try
        (publish-result! session (compute-result snapshot))
        (catch Throwable t
          (state/record-runtime-error!
           session
           {:phase :xr/trace-subscription}
           t))))
    (count @snapshots)))
