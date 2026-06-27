(ns graph.gur-mapper-topology-draw
  "Draw the real accumulated GUR mapper topology with source-level prop labels."
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [graph.vijual :as v]
            [propagators.cells.cell :as cell]
            [propagators.cells.value :as value]
            [propagators.compile :as compile]
            [propagators.core :as core]
            [propagators.datastructures.compound-object :as obj]
            [propagators.datastructures.compound-object.network-slot :as network-slot]
            [propagators.graph :as pgraph]
            [propagators.gur.accumulating :as acc]
            [propagators.gur.accumulating.runner :as acc-runner]
            [propagators.gur.subenv.env :as env]
            [propagators.helpers.task-queue :as tq]
            [propagators.ids :as ids]
            [propagators.network :as net]
            [propagators.network-builder :as nb]
            [propagators.propagator :as prop]))

(def label-dict-key [:debug :prop-labels])
(def cell-label-dict-key [:debug :cell-labels])

(def draw-opts
  {:stress-node-spacing 2.2
   :stress-iterations 200
   :stress-refine-iterations 200
   :stress-aspect-ratio 1.35
   :arrow-position :middle})

(defn- prop-ids
  [installed]
  (cond
    (nil? installed) []
    (sequential? installed) (vec (mapcat prop-ids installed))
    :else [installed]))

(defn- prop-label-records
  [label args installed]
  (let [ids (prop-ids installed)
        base {:op label :args (vec args)}]
    (if (and (#{'cons 'obj/p:cons} label) (= 2 (count ids)))
      (zipmap ids [(assoc base :op 'cons/car)
                   (assoc base :op 'cons/cdr)])
      (if (and (= 'chain/apply label) (= 2 (count ids)))
        (zipmap ids [(assoc base :op 'chain/apply-request)
                     (assoc base :op 'chain/apply-runner)])
        (zipmap ids (map-indexed (fn [i _] (assoc base :index i)) ids))))))

(defn- label-installer
  [label f]
  (fn [& args]
    (fn [n]
      (let [[installed n'] ((apply f args) n)]
        [installed
         (net/update-net-dict-entry n'
                                    label-dict-key
                                    #(merge (or % {})
                                            (prop-label-records label args installed)))]))))

(defn- add-cell-label
  [n id label]
  (net/update-net-dict-entry n
                             cell-label-dict-key
                             #(update (or % {}) id (fnil conj []) label)))

(defn- value-predicate
  [pred]
  (prop/primitive-propagator
   (fn [v]
     (cond
       (value/contradiction? v) value/contradiction
       (value/nothing? v) value/nothing
       :else (pred v)))))

(def p:even?
  (value-predicate
   (fn [v]
     (if (integer? v) (even? v) value/contradiction))))

(defn- labelled-installers
  [runtime]
  (let [base (acc/contextual-installers
              (merge (compile/default-installers)
                     {'p:even? p:even?
                      'obj/p:car obj/p:car
                      'obj/p:cdr obj/p:cdr
                      'obj/p:cons obj/p:cons
                      'cons obj/p:cons})
              runtime)]
    (into {}
          (map (fn [[k v]]
                 [k (label-installer k v)]))
          base)))

(def map-list
  (acc/source-recursive-closure
   :map-list
   '[list mapper acc-list out]
   '(do
      (let-cell [head rest mapped-rest]
        (obj/p:car head list)
        (obj/p:cdr rest list)
        (let [mapped (::apply mapper head)
              mapped-node (::cons mapped mapped-rest)]
          (when rest
            (p:id (::recur rest mapper acc-list) mapped-rest))
          mapped-node)))
   labelled-installers))

(def double-value
  (acc/source-recursive-closure
   :double-value
   '[x out]
   '(do
      (let [two 2]
        (::* x two)))
   labelled-installers))

(def unused-acc-list ::unused-acc-list)

(defn- strongest
  [n id]
  (net/network-cell-strongest n id))

(defn- pcons-list-source
  [n values terminal-value]
  (let [heads (vec (repeatedly (count values) ids/new-node-id))
        colls (vec (repeatedly (count values) ids/new-node-id))
        terminal-id (ids/new-node-id)
        n0 (reduce nb/install-cell n (conj (into heads colls) terminal-id))
        n0 (reduce (fn [n [id label]]
                     (add-cell-label n id label))
                   n0
                   (concat (map-indexed (fn [i id] [id (str "source.head" i)]) heads)
                           (map-indexed (fn [i id] [id (str "source.node" i)]) colls)
                           [[terminal-id "source.tail-nothing"]]))
        p:source-cons (label-installer 'obj/p:cons obj/p:cons)
        [prop-ids n1]
        (reduce
         (fn [[props n] i]
           (let [tail-id (if (< i (dec (count values)))
                           (colls (inc i))
                           terminal-id)
                 [ids n'] ((p:source-cons (heads i) tail-id (colls i)) n)]
             [(into props ids) n']))
         [[] n0]
         (range (count values)))
        seeds (conj (mapv vector heads values)
                    [terminal-id terminal-value])]
    {:net n1
     :prop-ids prop-ids
     :root-id (first colls)
     :seeds seeds}))

(defn- run-list-hop-chain
  [depth]
  (let [op-id (ids/new-node-id)
        mapper-id (ids/new-node-id)
        acc-id (ids/new-node-id)
        out-ids (vec (repeatedly depth ids/new-node-id))
        n0 (-> net/empty-net
               (nb/install-cell op-id map-list map-list)
               (nb/install-cell mapper-id double-value double-value)
               (nb/install-cell acc-id unused-acc-list unused-acc-list)
               (add-cell-label op-id "map-list-closure")
               (add-cell-label mapper-id "double-value-closure")
               (add-cell-label acc-id "acc-list"))
        {:keys [net prop-ids root-id seeds]} (pcons-list-source n0 [1 1 1 1 1] value/nothing)
        n1 (reduce nb/install-cell net out-ids)
        n1 (reduce-kv (fn [n i id]
                        (add-cell-label n id (str "hop" (inc i) ".out")))
                      n1
                      out-ids)
        p:top-apply (label-installer 'chain/apply acc/p:apply-closure)
        [props n2]
        (reduce
         (fn [[props n] i]
           (let [in-id (if (zero? i) root-id (out-ids (dec i)))
                 out-id (out-ids i)
                 [ids n'] ((p:top-apply op-id [in-id mapper-id acc-id] out-id) n)]
             [(into props ids) n']))
         [[] n1]
         (range depth))
        [n3 tasks] (reduce (fn [[n tasks] [id v]]
                             (nb/seed-cell! n tasks id v))
                           [n2 (tq/enqueue-all tq/empty-queue
                                               (into prop-ids props))]
                           seeds)
        n4 (core/run-tasks tasks n3)]
    {:net n4
     :out-id (peek out-ids)
     :out-value (strongest n4 (peek out-ids))}))

(defn- application-entry?
  [[k v]]
  (and (vector? k)
       (= :gur/application (first k))
       (ids/node-id? v)))

(defn- application-owner-ids
  [n]
  (->> (net/net-dict-or-empty n)
       (filter application-entry?)
       (sort-by (comp pr-str key))
       (mapv val)))

(defn- shorten
  [s n]
  (let [s (str s)]
    (if (<= (count s) n)
      s
      (str (subs s 0 (max 0 (- n 1))) "..."))))

(defn- stable-ids
  [ids]
  (sort-by pr-str ids))

(defn- node-name-renderer
  []
  (let [counter (atom 0)
        labels (atom {})]
    (fn [id]
      (or (@labels id)
          (let [label (str "n" (swap! counter inc))]
            (swap! labels assoc id label)
            label)))))

(defn- dict-labels
  [n]
  (let [named (net/network-dict-entry n cell-label-dict-key)
        direct (reduce-kv
                (fn [m k v]
                  (if (and (ids/node-id? v)
                           (not= k cell-label-dict-key)
                           (not= k label-dict-key))
                    (update m v (fnil conj []) (shorten k 28))
                    m))
                {}
                (net/net-dict-or-empty n))
        scoped (reduce
                (fn [m [scope bindings]]
                  (reduce-kv
                   (fn [m name local-id]
                     (if (ids/node-id? local-id)
                       (update m local-id (fnil conj [])
                               (str (shorten (second scope) 10)
                                    "."
                                    (shorten name 16)))
                       m))
                   m
                   bindings))
                {}
                (or (net/network-dict-entry n env/scopes-key) {}))]
    (merge-with into
                (into {} (map (fn [[id labels]] [id (vec labels)])) named)
                scoped
                direct)))

(defn- env-kind
  [entry]
  (cond
    (prop/prop? entry) :prop
    (cell/cell? entry) :cell
    (nil? entry) :meta
    :else :entry))

(defn- role-label
  [n id]
  (or (first (get (dict-labels n) id))
      (when (ids/node-id? id) "cell")
      (shorten id 12)))

(defn- friendly-op
  [op]
  (case op
    ctx/apply "ctx/apply"
    ctx/recur "ctx/recur"
    ctx/when "ctx/when"
    obj/p:car "obj/p:car"
    obj/p:cdr "obj/p:cdr"
    obj/p:cons "obj/p:cons"
    cons "cons"
    cons/car "cons/car"
    cons/cdr "cons/cdr"
    p:id "p:id"
    prop/* "prop/*"
    prop/+ "prop/+"
    prop/- "prop/-"
    prop/<= "prop/<="
    prop/not "prop/not"
    prop/and "prop/and"
    prop/or "prop/or"
    prop/switch "prop/switch"
    (shorten op 12)))

(defn- arrow-label
  [n inputs output]
  (str (str/join "," (map #(role-label n %) inputs))
       " -> "
       (role-label n output)))

(defn- prop-label
  [n id]
  (let [record (get (net/network-dict-entry n label-dict-key) id)
        op (:op record)
        args (:args record)]
    (cond
      (= op 'ctx/apply)
      (str (friendly-op op) " "
           (role-label n (first args))
           " "
           (arrow-label n (butlast (rest args)) (last args)))

      (= op 'chain/apply-request)
      (str "apply.request "
           (role-label n (first args))
           " "
           (arrow-label n (second args) (last args)))

      (= op 'chain/apply-runner)
      (str "apply.runner "
           (role-label n (first args))
           " "
           (arrow-label n (second args) (last args)))

      (= op 'ctx/recur)
      (str (friendly-op op) " " (arrow-label n (butlast args) (last args)))

      (= op 'ctx/when)
      (str (friendly-op op) " " (role-label n (first args)))

      (= op 'obj/p:car)
      (str (friendly-op op) " "
           (role-label n (second args)) " -> " (role-label n (first args)))

      (= op 'obj/p:cdr)
      (str (friendly-op op) " "
           (role-label n (second args)) " -> " (role-label n (first args)))

      (= op 'cons/car)
      (str (friendly-op op) " "
           (role-label n (nth args 2)) " <- " (role-label n (first args)))

      (= op 'cons/cdr)
      (str (friendly-op op) " "
           (role-label n (nth args 2)) " <- " (role-label n (second args)))

      (= op 'p:id)
      (str (friendly-op op) " " (arrow-label n [(first args)] (second args)))

      (= op 'prop/*)
      (str (friendly-op op) " " (arrow-label n (butlast args) (last args)))

      record
      (str (friendly-op op) " " (str/join "," (map #(role-label n %) args)))

      :else
      (or (first (get (dict-labels n) id))
          :prop))))

(defn- cell-label
  [_render-id n id entry]
  (let [names (get (dict-labels n) id)
        strongest (when (cell/cell? entry)
                    (cell/cell-strongest entry))]
    (str (or (first names) "cell")
         " "
         (shorten
          (cond
            (net/net? strongest) "Net"
            (value/nothing? strongest) "nothing"
            :else strongest)
          18))))

(defn- local-node-label
  [render-id n id]
  (let [entry (get (net/net-env n) id)]
    (case (env-kind entry)
      :prop (shorten (prop-label n id) 22)
      :cell (cell-label render-id n id entry)
      :meta "meta"
      :entry "entry")))

(defn- graph-ids
  [n]
  (set (concat (keys (net/net-graph n))
               (mapcat (fn [[_ node]]
                         (concat (pgraph/node-input-ids node)
                                 (pgraph/node-output-ids node)))
                        (net/net-graph n)))))

(defn- full-cell-label
  [n id entry]
  (let [names (get (dict-labels n) id)
        strongest (when (cell/cell? entry)
                    (cell/cell-strongest entry))]
    (str (or (first names) "cell")
         (when (seq (rest names))
           (str " aliases=" (pr-str (vec (rest names)))))
         " strongest="
         (cond
           (net/net? strongest) (str "Net(nodes=" (count (net/net-env strongest)) ")")
           (value/nothing? strongest) "nothing"
           :else (pr-str strongest)))))

(defn- full-local-label
  [n id]
  (let [entry (get (net/net-env n) id)]
    (case (env-kind entry)
      :prop (str "prop " (prop-label n id))
      :cell (str "cell " (full-cell-label n id entry))
      :meta "meta"
      :entry (str "entry " (pr-str entry)))))

(defn- topology-legend-section
  [title owner-label n]
  (let [render-id (node-name-renderer)
        ids (stable-ids (graph-ids n))
        _ (doseq [id ids] (render-id id))
        prop-ids (filter #(prop/prop? (get (net/net-env n) %)) ids)
        cell-ids (filter #(cell/cell? (get (net/net-env n) %)) ids)
        edge-lines
        (for [[from node] (sort-by (comp pr-str key) (net/net-graph n))
              to (stable-ids (pgraph/node-output-ids node))
              :when (and (contains? (set ids) from)
                         (contains? (set ids) to))]
          (str "  " (full-local-label n from)
               " => " (full-local-label n to)))]
    (str "=== " title " ===\n"
         "nodes=" (count ids)
         " props=" (count prop-ids)
         " cells=" (count cell-ids)
         " edges=" (count edge-lines)
         "\n\n"
         "propagators:\n"
         (if (seq prop-ids)
           (str/join "\n"
                     (map (fn [id]
                            (str "  " (full-local-label n id)))
                          prop-ids))
           "  <none>")
         "\n\ncells:\n"
         (if (seq cell-ids)
           (str/join "\n"
                     (map (fn [id]
                            (str "  " (full-local-label n id)))
                          cell-ids))
           "  <none>")
         "\n\nedges:\n"
         (if (seq edge-lines)
           (str/join "\n" edge-lines)
           "  <none>")
         "\n\n")))

(defn- network->vijual
  [owner-label n]
  (let [render-id (node-name-renderer)
        ids (graph-ids n)
        owner-name (name owner-label)
        node-id (fn [id] (str owner-name ":" (render-id id)))
        nodes (into {}
                    (map (fn [id]
                           [(node-id id)
                            (local-node-label render-id n id)]))
                    ids)
        edges (vec
               (for [[from node] (net/net-graph n)
                     to (pgraph/node-output-ids node)
                     :when (and (contains? ids from)
                                (contains? ids to))]
                 [(node-id from) (node-id to)]))]
    {:nodes nodes
     :edges edges}))

(defn- owner-net
  [top-net owner-id]
  (let [entry (get (net/net-env top-net) owner-id)]
    (when (cell/cell? entry)
      (let [v (cell/cell-strongest entry)]
        (when (net/net? v) v)))))

(defn- combined-topology
  [top-net owner-limit]
  (let [top (network->vijual :top top-net)
        owner-ids (take owner-limit (application-owner-ids top-net))
        owners (map-indexed
                (fn [i owner-id]
                  (when-let [n (owner-net top-net owner-id)]
                    (network->vijual (keyword (str "owner" (inc i))) n)))
                owner-ids)
        pieces (cons top (remove nil? owners))]
    {:edges (vec (mapcat :edges pieces))
     :nodes (apply merge (map :nodes pieces))
     :owner-count (count owner-ids)}))

(defn- draw-string
  [edges nodes]
  (with-out-str
    (v/draw-stress-directed-graph edges nodes draw-opts)))

(defn- section-string
  [title {:keys [edges nodes]}]
  (str "=== " title " ===\n"
       "nodes=" (count nodes) " edges=" (count edges) "\n\n"
       (draw-string edges nodes)
       "\n"))

(defn- value-summary
  [v]
  (cond
    (net/net? v) (str "Net(nodes=" (count (net/net-env v))
                      ", edges=" (reduce + 0 (map (comp count pgraph/node-output-ids val)
                                                  (net/net-graph v)))
                      ")")
    :else (pr-str v)))

(defn- prop-op
  [n id]
  (or (:op (get (net/network-dict-entry n label-dict-key) id))
      :unknown-prop))

(defn- cell-category
  [n id]
  (let [label (or (first (get (dict-labels n) id)) "cell")]
    (cond
      (str/starts-with? label ":map-list") :map-list-frame-local-cell
      (str/starts-with? label ":double-v") :double-value-mapper-cell
      (str/starts-with? label "source.") :source-pcons-list-cell
      (str/starts-with? label "hop") :hop-output-cell
      (#{"map-list-closure" "double-value-closure" "acc-list"} label) :top-closure-or-arg-cell
      (or (str/starts-with? label "[:env/scope")
          (str/starts-with? label "[:gur/application")) :owner-mailbox-or-application-cell
      (= label "cell") :anonymous-literal-or-internal-cell
      :else :other-cell)))

(defn- section-summary
  [title n]
  (let [ids (graph-ids n)
        prop-ids (filter #(prop/prop? (get (net/net-env n) %)) ids)
        cell-ids (filter #(cell/cell? (get (net/net-env n) %)) ids)
        edge-count (reduce + 0 (map (comp count pgraph/node-output-ids val)
                                    (net/net-graph n)))]
    {:title title
     :nodes (count ids)
     :props (count prop-ids)
     :cells (count cell-ids)
     :edges edge-count
     :prop-ops (frequencies (map #(prop-op n %) prop-ids))
     :cell-categories (frequencies (map #(cell-category n %) cell-ids))}))

(defn- merge-counts
  [& maps]
  (apply merge-with + maps))

(defn- activation-summary
  [events]
  (let [empty-row {:considered 0 :skipped 0 :ran 0 :elapsed-ns 0}
        rows (reduce
              (fn [m {:keys [event op elapsed-ns]}]
                (let [op (or op :unknown-prop)]
                  (case event
                    :considered (update-in m [op :considered] (fnil inc 0))
                    :skipped (update-in m [op :skipped] (fnil inc 0))
                    :ran (-> m
                             (update-in [op :ran] (fnil inc 0))
                             (update-in [op :elapsed-ns] (fnil + 0) (or elapsed-ns 0)))
                    m)))
              {}
              events)]
    (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
          (map (fn [[op row]]
                 [op (merge empty-row row
                            {:elapsed-ms (double (/ (:elapsed-ns row 0)
                                                    1000000.0))})]))
          rows)))

(defn- topology-change-summary
  [events]
  (let [interesting #{'ctx/apply 'ctx/recur 'ctx/when}
        ran-events (filter #(and (= :ran (:event %))
                                 (contains? interesting (:op %)))
                           events)]
    {:by-op
     (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
           (map (fn [[op xs]]
                  [op {:ran (count xs)
                       :changed (count (filter :changed? xs))
                       :env-delta (reduce + 0 (map :env-delta xs))
                       :dict-delta (reduce + 0 (map :dict-delta xs))}]))
           (group-by :op ran-events))
     :by-task-cause
     (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
           (map (fn [[task-key xs]]
                  [task-key
                   (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                         (map (fn [[op ys]]
                                [op {:ran (count ys)
                                     :changed (count (filter :changed? ys))
                                     :env-delta (reduce + 0 (map :env-delta ys))
                                     :dict-delta (reduce + 0 (map :dict-delta ys))}]))
                         (group-by :op xs))]))
           (group-by #(get-in % [:task-fact :task-key]) ran-events))}))

(defn- task-kind
  [event]
  (let [task-key (get-in event [:task-fact :task-key])]
    (if (vector? task-key)
      (first task-key)
      task-key)))

(defn- activation-by-task-kind
  [events]
  (let [ran-events (filter #(= :ran (:event %)) events)
        rows (reduce
              (fn [m event]
                (let [kind (task-kind event)
                      op (:op event)]
                  (-> m
                      (update-in [kind op :ran] (fnil inc 0))
                      (update-in [kind op :elapsed-ns]
                                 (fnil + 0)
                                 (or (:elapsed-ns event) 0)))))
              {}
              ran-events)]
    (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
          (map (fn [[kind ops]]
                 [kind
                  (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                        (map (fn [[op row]]
                               [op (assoc row
                                          :elapsed-ms
                                          (double (/ (:elapsed-ns row)
                                                     1000000.0)))]))
                        ops)]))
          rows)))

(defn- network-slot-summary
  [events]
  (let [rows (reduce
              (fn [m {:keys [slot-key branch message-count source-count peer-count parent-count]}]
                (-> m
                    (update-in [slot-key branch :calls] (fnil inc 0))
                    (update-in [slot-key branch :messages] (fnil + 0) (or message-count 0))
                    (update-in [slot-key branch :source-messages] (fnil + 0) (or source-count 0))
                    (update-in [slot-key branch :peer-messages] (fnil + 0) (or peer-count 0))
                    (update-in [slot-key branch :parent-count] (fnil + 0) (or parent-count 0))))
              {}
              events)]
    (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
          (map (fn [[slot-key branches]]
                 [slot-key
                  (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                        branches)]))
          rows)))

(defn- phase-summary
  [events]
  (let [rows (reduce
              (fn [m {:keys [phase elapsed-ns]}]
                (-> m
                    (update-in [phase :calls] (fnil inc 0))
                    (update-in [phase :elapsed-ns] (fnil + 0) (or elapsed-ns 0))))
              {}
              events)]
    (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
          (map (fn [[phase row]]
                 [phase (assoc row
                               :elapsed-ms
                               (double (/ (:elapsed-ns row)
                                          1000000.0)))]))
          rows)))


(defn- topology-summary
  [top-net owner-limit]
  (let [owner-ids (take owner-limit (application-owner-ids top-net))
        owner-summaries
        (vec
         (keep-indexed
          (fn [i owner-id]
            (when-let [n (owner-net top-net owner-id)]
              (section-summary (str "accumulated owner " (inc i)) n)))
          owner-ids))
        summaries (vec (cons (section-summary "top-level HOP chain" top-net)
                             owner-summaries))]
    {:sections summaries
     :totals {:nodes (reduce + (map :nodes summaries))
              :props (reduce + (map :props summaries))
              :cells (reduce + (map :cells summaries))
              :edges (reduce + (map :edges summaries))
              :prop-ops (apply merge-counts (map :prop-ops summaries))
              :cell-categories (apply merge-counts (map :cell-categories summaries))}}))

(defn- print-summary!
  [summary]
  (let [{:keys [nodes props cells edges prop-ops cell-categories]} (:totals summary)]
    (println "summary nodes" nodes "props" props "cells" cells "edges" edges)
    (println "summary prop ops" (into (sorted-map) prop-ops))
    (println "summary cell categories" (into (sorted-map) cell-categories))
    (when-let [activation (:activation summary)]
      (println "summary activation" activation))))

(defn- topology-sections
  [top-net owner-limit]
  (let [owner-ids (take owner-limit (application-owner-ids top-net))
        top-section {:title "top-level HOP chain"
                     :graph (network->vijual :top top-net)}
        owner-sections
        (keep-indexed
         (fn [i owner-id]
           (when-let [n (owner-net top-net owner-id)]
             {:title (str "accumulated owner " (inc i))
              :graph (network->vijual (keyword (str "owner" (inc i))) n)}))
         owner-ids)]
    (vec (cons top-section owner-sections))))

(defn- app-key-parts
  [app-key]
  (when (and (vector? app-key)
             (= :gur/application (first app-key)))
    {:closure-id (second app-key)
     :arg-ids (vec (nth app-key 2))
     :out-id (nth app-key 3)}))

(defn- closure-name
  [n closure-id]
  (let [v (strongest n closure-id)]
    (or (:gur/name v)
        (role-label n closure-id))))

(defn- frame-detail-label
  [n app-key]
  (let [{:keys [closure-id arg-ids out-id]} (app-key-parts app-key)
        name (closure-name n closure-id)]
    (str name " "
         (str/join "," (map #(role-label n %) arg-ids))
         " -> "
         (role-label n out-id))))

(defn- frame-display-labels
  [n frames]
  (let [indexed
        (second
         (reduce
          (fn [[counts rows] app-key]
            (let [{:keys [closure-id]} (app-key-parts app-key)
                  name (str (closure-name n closure-id))
                  i (inc (get counts name 0))]
              [(assoc counts name i)
               (assoc rows app-key {:label (str name "[" i "]")
                                    :detail (frame-detail-label n app-key)})]))
          [{} {}]
          frames))]
    indexed))

(defn- frame-task-props
  [n]
  (let [tasks (or (net/network-dict-entry n acc/task-index-key) {})
        frame-props
        (reduce-kv
         (fn [m task-key {:keys [prop-ids]}]
           (if (and (vector? task-key)
                    (= :frame (first task-key)))
             (assoc m (second task-key) (set prop-ids))
             m))
         {}
         tasks)
        prop->frame
        (into {}
              (mapcat (fn [[app-key prop-ids]]
                        (map (fn [prop-id] [prop-id app-key]) prop-ids)))
              frame-props)]
    (reduce-kv
     (fn [m task-key {:keys [prop-ids]}]
       (if (and (vector? task-key)
                (= :when (first task-key)))
         (let [when-key (second task-key)
               when-prop-id (last when-key)]
           (if-let [app-key (get prop->frame when-prop-id)]
             (update m app-key #(into (or % #{}) prop-ids))
             m))
         m))
     frame-props
     tasks)))

(defn- frame-for-prop
  [frame-props prop-id]
  (some (fn [[app-key prop-ids]]
          (when (contains? prop-ids prop-id)
            app-key))
        frame-props))

(defn- matching-frame
  [frames closure-id arg-ids out-id]
  (let [arg-ids (vec arg-ids)]
    (some (fn [app-key]
            (let [parts (app-key-parts app-key)]
              (when (and (= closure-id (:closure-id parts))
                         (= arg-ids (:arg-ids parts))
                         (= out-id (:out-id parts)))
                app-key)))
          frames)))

(defn- frame-edge-label
  [op]
  (case op
    ctx/apply "apply"
    ctx/recur "recur"
    ctx/when "when"
    (friendly-op op)))

(defn- frame-graph
  [owner-label n]
  (let [frames (stable-ids (or (net/network-dict-entry n acc/frame-index-key) #{}))
        frame-set (set frames)
        frame-props (frame-task-props n)
        records (net/network-dict-entry n label-dict-key)
        display-labels (frame-display-labels n frames)
        nodes (into {}
                    (map (fn [app-key]
                           [(str (name owner-label) ":" (hash (pr-str app-key)))
                            (:label (get display-labels app-key))]))
                    frames)
        details (into {}
                      (map (fn [app-key]
                             [(str (name owner-label) ":" (hash (pr-str app-key)))
                              (:detail (get display-labels app-key))]))
                      frames)
        node-id (fn [app-key] (str (name owner-label) ":" (hash (pr-str app-key))))
        edges
        (reduce-kv
         (fn [edges prop-id {:keys [op args]}]
           (if-let [from-frame (frame-for-prop frame-props prop-id)]
             (case op
               ctx/recur
               (let [{:keys [closure-id]} (app-key-parts from-frame)
                     to-frame (matching-frame frame-set
                                              closure-id
                                              (butlast args)
                                              (last args))
                     to-id (if to-frame
                             (node-id to-frame)
                             (str (node-id from-frame)
                                  ":recur:"
                                  (hash (pr-str args))))]
                 (conj edges [(node-id from-frame)
                              to-id
                              (frame-edge-label op)]))

               ctx/apply
               (let [to-frame (matching-frame frame-set
                                              (first args)
                                              (butlast (rest args))
                                              (last args))]
                 (cond-> edges
                   to-frame (conj [(node-id from-frame)
                                   (node-id to-frame)
                                   (frame-edge-label op)])))

               ctx/when
               (conj edges [(node-id from-frame)
                            (str (node-id from-frame) ":when:" (hash (pr-str prop-id)))
                            (frame-edge-label op)])

               edges)
             edges))
         []
         records)
        when-nodes
        (into {}
              (keep (fn [[_from to label]]
                      (when (= "when" label)
                        [to "when"])))
              edges)
        recur-nodes
        (into {}
              (keep (fn [[_from to label]]
                      (when (and (= "recur" label)
                                 (not (contains? nodes to)))
                        [to "recur-request"])))
              edges)]
    {:nodes (merge nodes when-nodes recur-nodes)
     :details (merge details
                     (into {}
                           (map (fn [[id _label]]
                                  [id "when cdr-present"])
                                when-nodes))
                     (into {}
                           (map (fn [[id _label]]
                                  [id "unmatched ctx/recur request"])
                                recur-nodes)))
     :edges (mapv (fn [[from to _label]] [from to]) edges)
     :labeled-edges edges
     :frame-count (count frames)}))

(defn- frame-graph-section
  [title frame-graph]
  (str "=== " title " frame graph ===\n"
       "frames=" (:frame-count frame-graph)
       " nodes=" (count (:nodes frame-graph))
       " edges=" (count (:edges frame-graph))
       "\n\n"
       (draw-string (:edges frame-graph) (:nodes frame-graph))
       "\n"))

(defn- frame-graph-legend-section
  [title {:keys [nodes details labeled-edges]}]
  (str "=== " title " frame graph ===\n\n"
       "frames:\n"
       (if (seq nodes)
         (str/join "\n"
                   (map (fn [[id label]]
                          (str "  " label "  " (get details id)))
                        nodes))
         "  <none>")
       "\n\nlinks:\n"
       (if (seq labeled-edges)
         (str/join "\n"
                   (map (fn [[from to label]]
                          (str "  " (get nodes from from)
                               " --" label "--> "
                               (get nodes to to)))
                        labeled-edges))
         "  <none>")
       "\n\n"))

(defn- write-edn!
  [path data]
  (spit path (with-out-str (pprint/pprint data))))

(defn -main
  [& args]
  (let [depth (Long/parseLong (or (first args) "6"))
        owner-limit (Long/parseLong (or (second args) (str depth)))
        out-prefix (or (nth args 2 nil) "/tmp/gur-map-depth")
        summary-only? (= "summary-only" (nth args 3 nil))
        activation-events (atom [])
        phase-events (atom [])
        network-slot-events (atom [])
        {:keys [net out-value]}
        (binding [acc-runner/*prop-run-observer*
                  (fn [{:keys [net prop-id] :as event}]
                    (swap! activation-events
                           conj
                           (assoc event
                                  :net nil
                                  :op (:op (get (net/network-dict-entry net label-dict-key)
                                                prop-id)))))
                  acc-runner/*phase-observer*
                  (fn [event]
                    (swap! phase-events conj event))
                  network-slot/*network-slot-observer*
                  (fn [event]
                    (swap! network-slot-events conj event))]
          (run-list-hop-chain depth))
        owner-count (count (take owner-limit (application-owner-ids net)))
        topology (when-not summary-only?
                   (combined-topology net owner-limit))
        sections (when-not summary-only?
                   (topology-sections net owner-limit))
        txt-path (str out-prefix depth "-topology.txt")
        edn-path (str out-prefix depth "-topology.edn")
        legend-path (str out-prefix depth "-topology-legend.txt")
        frame-path (str out-prefix depth "-frame-graph.txt")
        frame-legend-path (str out-prefix depth "-frame-graph-legend.txt")
        summary-path (str out-prefix depth "-topology-summary.edn")
        summary (assoc (topology-summary net owner-limit)
                       :activation (activation-summary @activation-events)
                       :activation-by-task-kind
                       (activation-by-task-kind @activation-events)
                       :network-slot
                       (network-slot-summary @network-slot-events)
                       :phases
                       (phase-summary @phase-events)
                       :topology-change
                       (topology-change-summary @activation-events))
        drawing (when-not summary-only?
                  (apply str
                         (map (fn [{:keys [title graph]}]
                                (section-string title graph))
                              sections)))
        frame-graphs (when-not summary-only?
                       (vec
                        (keep-indexed
                         (fn [i owner-id]
                           (when-let [n (owner-net net owner-id)]
                             {:title (str "accumulated owner " (inc i))
                              :graph (frame-graph
                                      (keyword (str "owner" (inc i)))
                                      n)}))
                         (take owner-limit (application-owner-ids net)))))
        frame-drawing (when-not summary-only?
                        (apply str
                               (map (fn [{:keys [title graph]}]
                                      (frame-graph-section title graph))
                                    frame-graphs)))]
    (when-not summary-only?
      (let [{:keys [edges nodes owner-count] :as topology} topology]
        (spit txt-path
              (str "Accumulating GUR map-list topology\n"
                   "depth=" depth "\n"
                   "owner-count=" owner-count "\n"
                   "combined-node-count=" (count nodes) "\n"
                   "combined-edge-count=" (count edges) "\n"
                   "rendered-sections=" (count sections) "\n"
                   "out-value=" (value-summary out-value) "\n\n"
                   drawing))
        (write-edn! edn-path
                    (assoc (select-keys topology [:edges :nodes :owner-count])
                           :sections
                           (mapv (fn [{:keys [title graph]}]
                                   (assoc graph :title title))
                                 sections)))
        (spit legend-path
              (str "Accumulating GUR map-list topology legend\n"
                   "depth=" depth "\n"
                   "owner-count=" owner-count "\n"
                   "out-value=" (value-summary out-value) "\n\n"
                   (topology-legend-section "top-level HOP chain" :top net)
                   (apply str
                          (keep-indexed
                           (fn [i owner-id]
                             (when-let [n (owner-net net owner-id)]
                               (topology-legend-section
                                (str "accumulated owner " (inc i))
                                (keyword (str "owner" (inc i)))
                                n)))
                           (take owner-limit (application-owner-ids net))))))
        (spit frame-path
              (str "Accumulating GUR map-list frame graph\n"
                   "depth=" depth "\n"
                   "owner-count=" owner-count "\n"
                   "out-value=" (value-summary out-value) "\n\n"
                   frame-drawing))
        (spit frame-legend-path
              (str "Accumulating GUR map-list frame graph legend\n"
                   "depth=" depth "\n"
                   "owner-count=" owner-count "\n"
                   "out-value=" (value-summary out-value) "\n\n"
                   (apply str
                          (map (fn [{:keys [title graph]}]
                                 (frame-graph-legend-section title graph))
                               frame-graphs))))
        (println "wrote" txt-path)
        (println "wrote" edn-path)
        (println "wrote" legend-path)
        (println "wrote" frame-path)
        (println "wrote" frame-legend-path)))
    (write-edn! summary-path summary)
    (println "wrote" summary-path)
    (println "depth" depth "owners" owner-count)
    (println "out-value" (value-summary out-value))
    (print-summary! summary)))
