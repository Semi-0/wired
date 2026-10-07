(ns propagators.tui.adapters.bridge.route-projection
  "Live route-slot declarations; authorization only observes their cells."
  (:require [propagators.infra.cells.value :as value]
            [propagators.infra.core :as core]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.gur :as gur]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]))

(def slot-scope :web/route-slots)
(def maximum-depth 256)

(defn slot-id [network collection-id slot-key]
  (get-in (net/network-dict-entry network gur/name-bindings-key)
          [slot-scope [collection-id slot-key]]))

(defn slot-value [network collection-id slot-key]
  (if-let [id (slot-id network collection-id slot-key)]
    (net/network-cell-strongest network id)
    value/nothing))

(defn- declare-slots [network collection-id]
  (reduce
   (fn [{:keys [net props effects slots]} slot-key]
     (let [fallback (gur/stable-node-id [:web/route-slot collection-id slot-key])
           [id installed updated]
           (obj/install-slot-access net slot-key collection-id fallback)]
       {:net updated
        :props (into props installed)
        :effects (conj effects (gur/bind-name slot-scope [collection-id slot-key] id))
        :slots (assoc slots slot-key id)}))
   {:net network :props [] :effects [] :slots {}}
   [:car :cdr]))

(declare walk-effects)

(defn- accessor-effects [network collection-id props]
  (mapv
   (fn [id]
     (let [entry (net/network-env-lookup network id)
           slot-key (second (prop/prop-name entry))
           node (get (net/net-graph network) id)]
       (gur/declare-prop
        (gur/stable-node-id [:web/route-slot-propagator collection-id slot-key])
        [:web/route-slot slot-key]
        (vec (:inputs node)) (vec (:outputs node)) (prop/prop-f entry))))
   props))

(defn- available-body [root-id role depth seen]
  (fn [context [collection-id] _]
    (let [network (:network context)
          collection (net/network-cell-strongest network collection-id)]
      (if (or (>= depth maximum-depth)
              (value/unusable? collection)
              (= :compiler-2/list-empty collection))
        {:effects []}
        (if (obj/accessor-network? collection)
          (let [{updated :net :keys [props effects slots]} (declare-slots network collection-id)
                head (:car slots)
                tail (:cdr slots)
                seen (conj seen [role collection-id])
                children (case role
                           :rows [(walk-effects root-id :row 0 head seen)
                                  (walk-effects root-id :rows (inc depth) tail seen)]
                           :row [(walk-effects root-id :destinations 0 tail seen)]
                           :destinations [(walk-effects root-id :destinations (inc depth) tail seen)]
                           (throw (ex-info "Unknown route projection role" {:role role})))]
            {:effects (into effects
                            (concat (map gur/declare-cell
                                         (remove #(contains? (net/net-env network) %)
                                                 (vals slots)))
                                    (accessor-effects updated collection-id props)
                                    (mapcat :effects children)))
             :messages (vec (mapcat :messages children))})
          {:effects []})))))

(defn- walk-body [root-id role depth seen]
  (fn [context [collection-id] result-id]
    (let [ready-id (gur/stable-node-id [:web/ready-route-body root-id role depth collection-id])]
      {:effects
       [(gur/when-effect
         [:web/route-ready root-id role depth collection-id]
         collection-id
         (fn []
           {:effects [(gur/declare-cell ready-id)
                      (gur/apply-closure-effect ready-id [collection-id] result-id)]
            :messages [(message ready-id
                                (gur/recursive-closure
                                 [:web/ready-routes role depth]
                                 (available-body root-id role depth seen)))]}))]})))

(defn- walk-effects [root-id role depth collection-id seen]
  (if (contains? seen [role collection-id])
    {:effects [] :messages []}
    (let [closure-id (gur/stable-node-id [:web/route-walker root-id role depth collection-id])
        result-id (gur/stable-node-id [:web/route-walk root-id role depth collection-id])]
    {:effects [(gur/declare-cell closure-id)
               (gur/declare-cell result-id)
               (gur/apply-closure-effect closure-id [collection-id] result-id)]
     :messages [(message closure-id
                         (gur/recursive-closure [:web/routes role depth]
                                                (walk-body root-id role depth seen)))]})))

(defn install [network root-id]
  (let [closure-id (gur/stable-node-id [:web/route-root root-id])
        result-id (gur/stable-node-id [:web/route-root-result root-id])
        effect (gur/apply-closure-effect closure-id [root-id] result-id)]
    (if (contains? (net/net-env network) (:id effect))
      network
      (let [seeded (-> network
                       (nb/ensure-cell closure-id)
                       (nb/ensure-cell result-id)
                       (nb/seed-cell closure-id
                                     (gur/recursive-closure
                                      :web/routes (walk-body root-id :rows 0 #{}))))
            [_ declared] (core/eval-activation-result effect seeded)]
        (nb/run-propagators declared [(:id effect)])))))

(defn members
  "Read a usable prefix. A contradictory/malformed/cyclic branch is unusable."
  [network root-id]
  (loop [id root-id seen #{} remaining maximum-depth values []]
    (let [collection (net/network-cell-strongest network id)]
      (cond
        (value/contradiction? collection) []
        (value/nothing? collection) values
        (= :compiler-2/list-empty collection) values
        (contains? seen id) []
        (zero? remaining) values
        (not (obj/accessor-network? collection)) []
        :else
        (let [head (slot-id network id :car)
              tail (slot-id network id :cdr)]
          (if (and head tail)
            (recur tail (conj seen id) (dec remaining) (conj values head))
            values))))))
