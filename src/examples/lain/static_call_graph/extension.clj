(ns examples.lain.static-call-graph.extension
  "Lain session extension for boundary-owned Clojure call-graph analysis."
  (:require [clojure.java.io :as io]
            [examples.lain.static-call-graph.analysis :as analysis]
            [propagators.infra.cells.value :as value]
            [propagators.compiler.compiler.basis :as basis]
            [propagators.runtime.session.extension :as extension]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.semantic-trace :as semantic-trace]))

(def effect-kind :environment/clojure-call-graph)

(defn- ready
  [payload]
  {:status :ready :payload payload})

(defn- invalid
  [reason message data]
  {:status :invalid
   :reason reason
   :message message
   :data data})

(defn call-graph-effect
  [project-root]
  {:effect/symbol 'clojure-call-graph
   :boundary/port :environment
   :boundary/kind effect-kind
   :effect/arities #{2 3}
   :effect/handler-symbol
   'examples.lain.static-call-graph.extension/handle-call-graph
   :effect/normalize
   (fn [_context [source-root target revision]]
     (cond
       (not (and (string? source-root) (not-empty source-root)))
       (invalid :invalid-source-root
                "clojure-call-graph expects a source path string"
                {:source-root source-root})

       (nil? (analysis/parse-target target))
       (invalid :invalid-target
                "clojure-call-graph expects a qualified var string"
                {:target target})

       (and (some? revision) (not (integer? revision)))
       (invalid :invalid-revision
                "clojure-call-graph revision must be an integer"
                {:revision revision})

       :else
       (ready {:project-root project-root
               :source-roots [source-root]
               :target target
               :revision (or revision 0)})))
   :effect/identity-parts
   (juxt :project-root :source-roots :target :revision)})

(defn handle-call-graph
  [_services session request]
  (try
    (let [{:keys [graph diagnostics summary]}
          (analysis/analyze-call-graph (:boundary/payload request))]
      {:state session
       :receipt {:status :analyzed
                 :graph graph
                 :summary summary
                 :diagnostics diagnostics
                 :revision (get-in request [:boundary/payload :revision])}})
    (catch Throwable error
      {:state session
       :receipt {:status :failed
                 :diagnostics [{:message (ex-message error)
                                :data (ex-data error)
                                :class (some-> error class .getName)}]
                 :revision (get-in request [:boundary/payload :revision])}})))

(defn- receipt-values
  [receipt]
  (->> (obj/public-slot-keys receipt)
       (sort-by pr-str)
       (map #(obj/slot-value receipt %))
       (filter map?)
       (filter #(= effect-kind (:boundary/kind %)))))

(defn call-graph-result
  [receipt]
  (let [successful (->> (receipt-values receipt)
                        (filter #(= :analyzed (:boundary/status %)))
                        (filter #(semantic-trace/semantic-trace-graph?
                                  (:graph %)))
                        (sort-by (juxt #(or (:revision %) 0)
                                       #(pr-str (:boundary/id %)))))
        graph (:graph (last successful))]
    (if graph
      graph
      value/nothing)))

(defn extension
  [project-root]
  (let [canonical-root (.getPath (.getCanonicalFile (io/file project-root)))]
    (extension/extension-bundle
     {:id [:example/clojure-call-graph canonical-root]
      :bindings [['call-graph-result
                  (basis/primitive-operator call-graph-result)]]
      :effects [(call-graph-effect canonical-root)]})))
