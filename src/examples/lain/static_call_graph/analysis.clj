(ns examples.lain.static-call-graph.analysis
  "Static project call graphs projected into the existing semantic graph data."
  (:require [clj-kondo.core :as kondo]
            [propagators.infra.semantic-trace :as semantic-trace])
  (:import [java.io File]))

(defn parse-target
  [target]
  (when (string? target)
    (let [separator (.lastIndexOf ^String target "/")]
      (when (and (pos? separator)
                 (< separator (dec (count target))))
        [(symbol (subs target 0 separator))
         (symbol (subs target (inc separator)))]))))

(defn- canonical-file
  [file]
  (.getCanonicalFile (File. (str file))))

(defn- descendant?
  [^File parent ^File child]
  (.startsWith (.toPath child) (.toPath parent)))

(defn resolve-source-roots
  [project-root source-roots]
  (let [root (canonical-file project-root)]
    (cond
      (not (.isDirectory root))
      (throw (ex-info "Call graph project root is not a directory"
                      {:project-root (.getPath root)}))

      (not (and (vector? source-roots)
                (seq source-roots)
                (every? string? source-roots)))
      (throw (ex-info "Call graph source roots must be a non-empty vector of paths"
                      {:source-roots source-roots}))

      :else
      (mapv
       (fn [source-root]
         (let [resolved (canonical-file (File. root source-root))]
           (cond
             (not (descendant? root resolved))
             (throw (ex-info "Call graph source root escapes the project root"
                             {:project-root (.getPath root)
                              :source-root (.getPath resolved)}))

             (not (.exists resolved))
             (throw (ex-info "Call graph source root does not exist"
                             {:source-root (.getPath resolved)}))

             :else
             (.getPath resolved))))
       source-roots))))

(defn- definition-key
  [{:keys [ns name]}]
  (when (and (symbol? ns) (symbol? name))
    [ns name]))

(defn- usage-edge
  [{:keys [from from-var to name]}]
  (when (and (symbol? from)
             (symbol? from-var)
             (symbol? to)
             (symbol? name))
    [[from from-var] [to name]]))

(defn- qualified-node-id
  [[namespace name]]
  (symbol (str namespace) (str name)))

(defn- reachable-nodes
  [edges target]
  (let [outgoing (reduce (fn [index [from to]]
                           (update index from (fnil conj #{}) to))
                         {}
                         edges)]
    (loop [seen #{target}
           pending [target]]
      (if-let [current (peek pending)]
        (let [children (get outgoing current #{})
              unseen (remove seen children)]
          (recur (into seen unseen)
                 (into (pop pending) unseen)))
        seen))))

(defn build-call-graph
  [analysis target]
  (let [target-key (parse-target target)
        definitions (->> (:var-definitions analysis)
                         (sort-by (juxt :filename :row :col))
                         (reduce (fn [index definition]
                                   (if-let [key (definition-key definition)]
                                     (if (contains? index key)
                                       index
                                       (assoc index key definition))
                                     index))
                                 {}))
        defined-keys (set (keys definitions))]
    (cond
      (nil? target-key)
      (throw (ex-info "Call graph target must be a qualified var string"
                      {:target target}))

      (not (contains? defined-keys target-key))
      (throw (ex-info "Call graph target was not found in the analyzed roots"
                      {:target target}))

      :else
      (let [edges (->> (:var-usages analysis)
                       (keep usage-edge)
                       (filter (fn [[from to]]
                                 (and (contains? defined-keys from)
                                      (contains? defined-keys to)
                                      (not= from to))))
                       set)
            reachable (reachable-nodes edges target-key)
            reachable-edges (->> edges
                                 (filter (fn [[from to]]
                                           (and (contains? reachable from)
                                                (contains? reachable to))))
                                 (mapv (fn [[from to]]
                                         [(qualified-node-id from)
                                          (qualified-node-id to)]))
                                 (sort-by pr-str)
                                 vec)
            ordered-nodes (sort-by pr-str reachable)]
        (semantic-trace/graph-union
         {:nodes (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                       (map (fn [key]
                              (let [id (qualified-node-id key)]
                                [id (str id)])))
                       ordered-nodes)
          :values (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                        (map (fn [key]
                               (let [definition (get definitions key)]
                                 [(qualified-node-id key)
                                  (select-keys definition
                                               [:filename :row :col
                                                :end-row :end-col])])))
                        ordered-nodes)
          :edges reachable-edges})))))

(defn analyze-call-graph
  [{:keys [project-root source-roots target]}]
  (let [resolved-roots (resolve-source-roots project-root source-roots)
        result (kondo/run! {:lint resolved-roots
                            :cache false
                            :repro true
                            :config {:output {:analysis true}}})
        graph (build-call-graph (:analysis result) target)]
    {:graph graph
     :diagnostics (vec (:findings result))
     :summary (assoc (:summary result)
                     :source-roots source-roots
                     :target target
                     :node-count (count (:nodes graph))
                     :edge-count (count (:edges graph)))}))
