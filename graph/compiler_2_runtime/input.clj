(ns graph.compiler-2-runtime.input
  "Runtime input commit and replay logic."
  (:require [graph.compiler-2-runtime.block-model :as block-model]
            [graph.compiler-2-runtime.effects :as effects]
            [graph.compiler-2-runtime.graph-projection :as graphp]
            [graph.compiler-2-runtime.program :as program]
            [graph.compiler-2-runtime.program-rebuild :as program-rebuild]
            [graph.compiler-2-runtime.state :as state]
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]))

(def run-runtime-cycle effects/run-runtime-cycle)
(def perform-boundary-effects effects/perform-boundary-effects)
(def record-runtime-transaction graphp/record-runtime-transaction)
(def assoc-graph-cell-value graphp/assoc-graph-cell-value)
(def settle-application-props program/settle-application-props)
(def rebuild-program-state program-rebuild/rebuild-program-state)
(def incremental-block-state program-rebuild/incremental-block-state)
(def read-source-forms program/read-source-forms)
(def ensure-session-state! state/ensure-session-state!)
(def mutate-session! state/mutate-session!)
(def default-xr-client-id state/default-xr-client-id)
(def external-source-client-id state/external-source-client-id)
(def next-global-order block-model/next-global-order)

(defn next-widget-epoch
  [state widget-id]
  (inc (long (get-in state [:xr :widget-epochs widget-id] 0))))

(defn next-runtime-commit-tick
  [state]
  (inc (long (or (:runtime/commit-tick state) 0))))

(defn assoc-program-commit-tick
  [state tick]
  (assoc state
         :program/net
         (net/assoc-net-dict-entry (:program/net state)
                                   :runtime/commit-tick
                                   tick)))

(defn widget-channel
  [state widget-id channel]
  (or (get-in state [:xr :widgets widget-id :channels channel])
      (throw (ex-info "xr widget channel not found"
                      {:widget-id widget-id
                       :channel channel}))))

(defn annotate-widget-cell-values
  [state widget-id]
  (let [channels (vals (get-in state [:xr :widgets widget-id :channels]))
        cell-ids (distinct (mapcat (juxt :event-cell :view-cell) channels))]
    (reduce (fn [s cell-id]
              (if cell-id
                (assoc-graph-cell-value s cell-id)
                s))
            state
            cell-ids)))

(defn apply-program-updates
  [state updates]
  (let [updates (vec (remove (comp nil? :cell-id) updates))
        n0 (reduce (fn [n {:keys [cell-id]}]
                     (nb/ensure-cell n cell-id))
                   (:program/net state)
                   updates)
        [tasks n1] (core/eval-cells
                    (mapv (fn [{:keys [cell-id update]}]
                            (message cell-id update))
                          updates)
                    n0)
        n2 (core/run-tasks tasks n1)
        n3 (settle-application-props n2 [])]
    (assoc state :program/net n3)))

(defn replay-widget-updates
  [state {:keys [widget-id updates]}]
  (let [widget-id (str widget-id)
        channels (get-in state [:xr :widgets widget-id :channels])
        updates* (vec
                  (keep (fn [{:keys [channel value update]}]
                          (when-let [event-cell (get-in channels
                                                         [(str channel)
                                                          :event-cell])]
                            {:cell-id event-cell
                             :value value
                             :update update}))
                        updates))]
    (-> state
        (apply-program-updates updates*)
        (annotate-widget-cell-values widget-id))))

(defn replay-runtime-input
  [state input]
  (let [state (if-let [tick (:runtime/commit-tick input)]
                (assoc-program-commit-tick state tick)
                state)]
    (case (:runtime/input input)
      (:cell-message :xr/message)
      (apply-program-updates state [(select-keys input [:cell-id :update])])

      :xr/widget-event
      (replay-widget-updates state input)

      state)))

(defn replay-runtime-inputs
  [state]
  (reduce replay-runtime-input state (:runtime/inputs state)))

(defn commit-runtime-input
  "Commit an external cell message into the runtime model, then propagate/effect."
  [state input]
  (let [tick (or (:runtime/commit-tick input)
                 (next-runtime-commit-tick state))
        input (assoc input :runtime/commit-tick tick)
        state (-> state
                  (assoc :runtime/commit-tick tick)
                  (assoc-program-commit-tick tick))]
    (case (:runtime/input input)
    (:cell-message :xr/message)
    (let [before-net (:program/net state)
          cell-id (:cell-id input)
          update-value (:update input)
          n3 (:program/net
              (apply-program-updates state [{:cell-id cell-id
                                             :update update-value}]))]
      (-> state
          (assoc :program/net n3)
          (update :runtime/inputs (fnil conj []) input)
          run-runtime-cycle
          (record-runtime-transaction before-net)))

    :xr/widget-event
    (let [before-net (:program/net state)
          widget-id (str (:widget-id input))
          channel (str (or (:channel input) "value"))
          _widget-channel (widget-channel state widget-id channel)
          epoch (next-widget-epoch state widget-id)
          latest-values (assoc (get-in state [:xr :widget-latest widget-id] {})
                               channel
                               (:value input))
          channels (get-in state [:xr :widgets widget-id :channels])
          updates (vec
                   (keep (fn [[channel-name channel-info]]
                           (when (contains? latest-values channel-name)
                             {:channel channel-name
                              :cell-id (:event-cell channel-info)
                              :value (get latest-values channel-name)
                              :update (obj/compound-object
                                       {epoch (get latest-values channel-name)})}))
                         channels))
          n3 (:program/net (apply-program-updates state updates))]
      (-> state
          (assoc :program/net n3)
          (assoc-in [:xr :widget-epochs widget-id] epoch)
          (assoc-in [:xr :widget-latest widget-id] latest-values)
          (update :runtime/inputs (fnil conj [])
                  (assoc input
                         :runtime/input :xr/widget-event
                         :widget-id widget-id
                         :channel channel
                         :epoch epoch
                         :updates updates))
          run-runtime-cycle
          (annotate-widget-cell-values widget-id)
          (record-runtime-transaction before-net)))

    (throw (ex-info "unsupported runtime input" {:input input})))))

(defn commit-runtime-input!
  [session input]
  (mutate-session! session #(commit-runtime-input % input)))

(defn rebuild-program!
  [session]
  (mutate-session!
   session
   (fn [state]
     (let [before-net (:program/net state)]
       (-> state
           rebuild-program-state
           perform-boundary-effects
           replay-runtime-inputs
           run-runtime-cycle
           (record-runtime-transaction before-net))))))

(defn install-block-incremental!
  [session block]
  (mutate-session!
   session
   (fn [state]
     (let [before-net (:program/net state)]
       (if-let [state' (incremental-block-state state block)]
         (-> state'
             perform-boundary-effects
             replay-runtime-inputs
             run-runtime-cycle
             (record-runtime-transaction before-net))
         (-> state
             (update :runtime/full-rebuild-fallbacks (fnil inc 0))
             rebuild-program-state
             perform-boundary-effects
             replay-runtime-inputs
             run-runtime-cycle
             (record-runtime-transaction before-net)))))))

(defn extend-source!
  "Append compiler source to the active runtime without replacing TUI state.

  The source may contain multiple top-level forms. Each form is installed as a
  hidden ordered source block, so definitions can extend the existing compiler
  environment the same way visible TUI blocks do.
  "
  [session {:keys [source client-id] :or {client-id default-xr-client-id}}]
  (ensure-session-state! session)
  (let [forms (read-source-forms source)
        installed (atom [])]
    (swap! session
           (fn [state]
             (let [start-order (next-global-order state)
                   start-index (count (:external-sources state))
                   blocks (mapv
                           (fn [offset form]
                             {:client-id external-source-client-id
                              :source-client-id client-id
                              :index (+ start-index offset)
                              :order (+ start-order offset)
                              :epoch 0
                              :source (pr-str form)
                              :external? true})
                           (range)
                           forms)]
               (reset! installed blocks)
               (-> state
                   (update :external-sources (fnil into []) blocks)
                   (assoc :next-order (+ start-order (count blocks)))))))
    (rebuild-program! session)
    {:client-id client-id
     :blocks @installed}))
