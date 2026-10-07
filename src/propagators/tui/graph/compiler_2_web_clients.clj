(ns propagators.tui.graph.compiler-2-web-clients
  "Independent web-client runtime instances coordinated through compiler-2 bridges."
  (:require [propagators.runtime :as runtime]
            [propagators.tui.assembly :as assembly]
            [propagators.runtime.session.file-loader :as file-loader]
            [propagators.tui.adapters.bridge.web :as bridge]
            [propagators.tui.adapters.bridge.route-projection :as routes]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.model.env :as cenv]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.datastructures.event :as event]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]))

(def default-model-file "dev/examples/lain/multi-client-messaging.lain")

(defn new-system []
  (atom {:coordinator (assembly/new-session)
         :clients {}
         :order []
         :next-client-index 0}))

(defn- ensure-id
  [system client-id]
  (or client-id
      (str "client-" (:next-client-index system))))

(defn- client-view-cell-id
  [client-id]
  (bridge/client-view-source-id client-id))

(defn- client-input-cell-id
  [client-id pipe]
  (bridge/client-input-id client-id pipe))

(defn- strongest
  [session cell-id]
  (let [state @session
        program-net (:program/net state)]
    (if (and program-net (contains? (net/net-env program-net) cell-id))
      (net/network-cell-strongest program-net cell-id)
      value/nothing)))

(defn- cell-id-for-symbol
  [session sym]
  (let [state @session]
    (cenv/resolve-binding-id (:program/net state)
                             (:program/env state)
                             sym)))

(defn- symbol-strongest
  [session sym]
  (if-let [cell-id (cell-id-for-symbol session sym)]
    (strongest session cell-id)
    value/nothing))

(defn- base-value
  [v]
  (cond
    (event/event-projection? v)
    (let [facts (event/projection-facts v)]
      (if (= 1 (count facts))
        (event/event-value (first facts))
        v))

    :else
    v))

(defn- commit-cell!
  [session cell-id update]
  (runtime/commit-runtime-input! session
                                 {:runtime/input :cell-message
                                  :cell-id cell-id
                                  :update update}))

(defn- prepare-routes!
  [system]
  (swap! (:coordinator @system)
         (fn [state]
           (if-let [root-id (cenv/resolve-binding-id (:program/net state)
                                                    (:program/env state) 'routes)]
             (let [projected (routes/install (:program/net state) root-id)]
               (assoc state :program/net projected :compiled-network projected))
             state))))

(defn load-model-source!
  [system source]
  (let [loaded (file-loader/load-source! (:coordinator @system)
                                       source {:client-id "web-coordinator"})]
    (prepare-routes! system)
    loaded))

(defn load-model-file!
  ([system] (load-model-file! system default-model-file))
  ([system file]
   (let [loaded (file-loader/load-file! (:coordinator @system)
                                      file {:client-id "web-coordinator"})]
     (prepare-routes! system)
     loaded)))

(defn- publish-client-list!
  [system]
  (commit-cell! (:coordinator @system)
                (bridge/client-list-source-id)
                (bridge/linked-list-value (:order @system))))

(defn register-client!
  ([system] (register-client! system {}))
  ([system {:keys [client-id]}]
   (let [client-id (ensure-id @system client-id)
         session (assembly/new-session)]
     (swap! system
            (fn [s]
              (let [known? (contains? (:clients s) client-id)]
                (as-> (assoc-in s
                                 [:clients client-id]
                                 {:client-id client-id
                                  :session session}) s*
                  (if known?
                    s*
                    (-> s*
                        (update :order conj client-id)
                        (update :next-client-index inc)))))))
     (publish-client-list! system)
     (prepare-routes! system)
     {:client-id client-id
      :session session})))

(defn client-session
  [system client-id]
  (or (get-in @system [:clients (bridge/normalize-client-id client-id) :session])
      (throw (ex-info "web client not found" {:client-id client-id}))))

(defn- message-event
  [cell-id from tick payload]
  (event/active-event cell-id (bridge/normalize-client-id from) tick payload))

(defn- message-payload
  [{:keys [from to text pipe value]}]
  (or value
      {:message/from (bridge/normalize-client-id from)
       :message/to (bridge/normalize-client-id to)
       :message/text text
       :message/pipe (bridge/normalize-pipe pipe)}))

(defn commit-client-message!
  [system {:keys [from pipe] :as message}]
  (let [from (bridge/normalize-client-id from)
        pipe (bridge/normalize-pipe pipe)
        payload (message-payload (assoc message :from from :pipe pipe))
        client-session (client-session system from)
        client-cell (client-input-cell-id from pipe)
        tick (inc (long (or (:web/commit-tick @system) 0)))
        update (message-event client-cell from tick payload)]
    (swap! system assoc :web/commit-tick tick)
    (commit-cell! client-session client-cell update)
    {:client-id from
     :payload payload
     :tick tick}))

(defn route-allowed?
  ([system from to] (route-allowed? system from to bridge/default-pipe))
  ([system from to pipe]
   (let [from (bridge/normalize-client-id from)
         to (bridge/normalize-client-id to)
         network (:program/net @(:coordinator @system))
         root-id (cell-id-for-symbol (:coordinator @system) 'routes)
         route-rows (if root-id (routes/members network root-id) [])]
     (boolean
      (some (fn [row]
              (let [row-from (routes/slot-value network row :car)
                    row-outs (routes/slot-id network row :cdr)]
                (and (not (value/unusable? row-from))
                     (= from (bridge/normalize-client-id row-from))
                     row-outs
                     (some (fn [destination-id]
                             (let [destination (net/network-cell-strongest network destination-id)]
                               (and (not (value/unusable? destination))
                                    (= to (bridge/normalize-client-id destination)))))
                           (routes/members network row-outs)))))
            route-rows)))))

(defn- deliver-client-view!
  [system client-id payload tick]
  (let [client-id (bridge/normalize-client-id client-id)
        view-cell (client-view-cell-id client-id)
        update (message-event view-cell client-id tick payload)
        view payload]
    (commit-cell! (client-session system client-id) view-cell update)
    (swap! system assoc-in [:clients client-id :latest-view] view)
    view))

(defn latest-client-view
  [system client-id]
  (or (get-in @system [:clients (bridge/normalize-client-id client-id) :latest-view])
      (strongest (client-session system client-id)
                 (client-view-cell-id client-id))))

(defn route-message!
  [system message]
  (let [{:keys [payload tick] :as committed} (commit-client-message! system message)
        from (bridge/normalize-client-id (:from message))
        to (bridge/normalize-client-id (:to message))
        pipe (bridge/normalize-pipe (:pipe message))
        delivered? (route-allowed? system from to pipe)]
    (when delivered?
      (deliver-client-view! system to payload tick))
    (assoc committed
           :to to
           :pipe pipe
           :delivered? (boolean delivered?))))
