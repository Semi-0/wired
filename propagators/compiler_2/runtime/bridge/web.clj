(ns propagators.compiler-2.runtime.bridge.web
  "Stable cell ids and values for web-client runtime bridge primitives."
  (:require [propagators.compiler-2.runtime.session.state :as state]
            [propagators.cells.value :as value]
            [propagators.datastructures.compound-object :as obj]))

(def default-pipe "message")

(defn normalize-client-id
  [client]
  (str (or (and (map? client) (:client/id client))
           client)))

(defn normalize-pipe
  [pipe]
  (str (or pipe default-pipe)))

(defn client-handle
  [client-id]
  {:client/id (normalize-client-id client-id)})

(defn client-handle?
  [v]
  (and (map? v)
       (contains? v :client/id)))

(defn client-list-source-id []
  (state/stable-node-id :web :clients :source))

(defn client-input-id
  [client-id pipe]
  (state/stable-node-id :web :client (normalize-client-id client-id)
                        :input (normalize-pipe pipe)))

(defn client-view-source-id
  [client-id]
  (state/stable-node-id :web :client (normalize-client-id client-id) :view))

(defn client-route-id
  [from to pipe]
  (state/stable-node-id :web :route
                        (normalize-client-id from)
                        (normalize-client-id to)
                        (normalize-pipe pipe)))

(defn route-value
  [from to pipe]
  {:route/from (normalize-client-id from)
   :route/to (normalize-client-id to)
   :route/pipe (normalize-pipe pipe)})

(defn linked-list-value
  [client-ids]
  (if-let [client-id (first (seq client-ids))]
    (obj/as-accessor-network
     {:car (client-handle client-id)
      :cdr (linked-list-value (rest client-ids))})
    value/nothing))

(defn linked-list-values
  ([xs] (linked-list-values xs 256))
  ([xs max-depth]
   (loop [current xs
          values []
          seen #{}
          remaining (long max-depth)]
    (if (value/unusable? current)
      values
      (let [identity-key (System/identityHashCode current)]
        (if (or (zero? remaining) (contains? seen identity-key))
          values
          (let [head (obj/accessor-source-slot-value current :car)
                tail (obj/accessor-source-slot-value current :cdr)]
            (recur tail
                   (conj values head)
                   (conj seen identity-key)
                   (dec remaining)))))))))
