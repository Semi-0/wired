(ns propagators.tui.adapters.bridge.route-projection-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.tui.adapters.bridge.route-projection :as routes]
            [propagators.infra.cells.value :as value]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.ids :as ids]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]))

(defn- projected-route [from to]
  (let [root (ids/new-node-id)
        row (obj/as-accessor-network
             {:car from :cdr (obj/as-accessor-network {:car to :cdr value/nothing})})
        seeded (-> net/empty-net
                   (nb/ensure-cell root)
                   (nb/seed-cell root (obj/as-accessor-network {:car row :cdr value/nothing})))]
    [root (routes/install seeded root)]))

(deftest seeded-accessors-and-unavailable-tail
  (let [[root network] (projected-route "A" "B")
        rows (routes/members network root)
        row (first rows)
        destinations (routes/members network (routes/slot-id network row :cdr))]
    (is (= 1 (count rows)))
    (is (= "A" (routes/slot-value network row :car)))
    (is (= ["B"] (mapv #(net/network-cell-strongest network %) destinations)))
    (is (= network (routes/install network root)))))

(deftest contradictory-head-is-not-an-authorization-witness
  (let [[root network] (projected-route "A" value/contradiction)
        row (first (routes/members network root))
        destination (first (routes/members network (routes/slot-id network row :cdr)))]
    (is (value/unusable? (net/network-cell-strongest network destination)))))

(deftest late-root-declares-existing-availability-topology
  (let [root (ids/new-node-id)
        pending (routes/install (nb/ensure-cell net/empty-net root) root)
        row (obj/as-accessor-network {:car "A" :cdr value/nothing})
        updated (nb/seed-cell pending root (obj/as-accessor-network {:car row :cdr value/nothing}))
        settled (nb/run-propagators updated (nb/neighbor-propagator-ids updated root))
        rows (routes/members settled root)]
    (is (= [] (routes/members pending root)))
    (is (= 1 (count rows)))
    (is (= "A" (routes/slot-value settled (first rows) :car)))))

(deftest equivalent-redeclaration-has-stable-topology-identities
  (let [root (ids/new-node-id)
        row (obj/as-accessor-network {:car "A" :cdr value/nothing})
        seeded (-> net/empty-net (nb/ensure-cell root)
                   (nb/seed-cell root (obj/as-accessor-network {:car row :cdr value/nothing})))
        first-net (routes/install seeded root)
        second-net (routes/install seeded root)]
    (is (= (set (keys (net/net-env first-net)))
           (set (keys (net/net-env second-net)))))
    (is (= (net/net-graph first-net) (net/net-graph second-net)))))

(deftest cyclic-and-malformed-lists-do-not-authorize
  (let [root (ids/new-node-id)
        row (obj/as-accessor-network {:car "A" :cdr value/nothing})
        cycle (obj/register-accessor-parent
               (obj/as-accessor-network {:car row}) :cdr root)
        seeded (-> net/empty-net (nb/ensure-cell root) (nb/seed-cell root cycle))
        projected (routes/install seeded root)
        malformed (routes/install (nb/seed-cell (nb/ensure-cell net/empty-net root)
                                               root 42) root)]
    (is (= [] (routes/members projected root)))
    (is (< (count (net/net-env projected)) 100))
    (is (= projected (routes/install projected root)))
    (is (= [] (routes/members malformed root)))))
