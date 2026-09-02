(ns graph.vijual-compiler-2-demo
  "TUI rendering adapter for compiler semantic graph projections."
  (:require [clojure.string :as str]
            [graph.vijual :as v]
            [propagators.compiler-2.model.closure-value :as closure-value]
            [propagators.compiler-2.runtime.inspection.semantic-support :as support]))

(def source support/source)
(def draw-opts
  {:stress-node-spacing 1.7
   :stress-iterations 200
   :stress-refine-iterations 200
   :routing :shortest-path})
(def semantic-draw-opts (assoc draw-opts :arrow-position :end))

(def display-name support/display-name)
(def ast-label support/ast-label)
(def environment-labels support/environment-labels)
(def topology-binding-labels support/topology-binding-labels)
(def network-closure-values support/network-closure-values)
(def closure-labels support/closure-labels)
(def closure-key support/closure-key)
(def unique-network-closure-values support/unique-network-closure-values)
(def closure-env-labels support/closure-env-labels)
(def generated-labels support/generated-labels)
(def simple-cell-value? support/simple-cell-value?)
(def literal-cell-labels support/literal-cell-labels)
(def application-records support/application-records)
(def label-entry support/label-entry)
(def application-node-labels support/application-node-labels)
(def application-prop-labels support/application-prop-labels)
(def slot-prop-labels support/slot-prop-labels)
(def compiled-labels support/compiled-labels)
(def network-edges support/network-edges)
(def network-vijual-graph support/network-vijual-graph)
(def compiled-progression support/compiled-progression)
(def closure-body-progression support/closure-body-progression)
(def semantic-base-labels support/semantic-base-labels)
(def semantic-label support/semantic-label)
(def semantic-key support/semantic-key)
(def semantic-node support/semantic-node)
(def semantic-edge support/semantic-edge)
(def add-sync-semantic support/add-sync-semantic)
(def add-forward-sync-semantic support/add-forward-sync-semantic)
(def add-call-semantic support/add-call-semantic)
(def add-operator-semantic support/add-operator-semantic)
(def add-application-node-semantic support/add-application-node-semantic)
(def add-application-semantic support/add-application-semantic)
(def semantic-graph support/semantic-graph)
(def semantic-progression support/semantic-progression)

(defn draw-stage
  [{:keys [title compiled network]}]
  (let [{:keys [edges nodes]} (network-vijual-graph compiled network)]
    (println title)
    (println (str (count nodes) " nodes, " (count edges) " directed edges"))
    (println)
    (v/draw-stress-directed-graph edges nodes draw-opts)
    (println)))

(defn draw-semantic-stage
  [{:keys [title graph]}]
  (let [{:keys [edges nodes]} graph]
    (println title)
    (println (str (count nodes) " nodes, " (count edges) " semantic edges"))
    (doseq [[from to] edges]
      (println (str "  " (nodes from) " -> " (nodes to))))
    (println)
    (v/draw-stress-directed-graph edges nodes semantic-draw-opts)
    (println)))

(defn print-closures
  [network]
  (doseq [[index closure-info]
          (map-indexed vector (unique-network-closure-values network))]
    (println (str "closure " index ": inputs "
                  (pr-str (closure-value/closure-inputs closure-info))
                  ", output "
                  (pr-str (closure-value/closure-output closure-info))))))

(defn -main
  [& args]
  (let [source-text (if (seq args) (str/join " " args) source)
        stages (compiled-progression source-text)
        {:keys [compiled network]} (last stages)
        {:keys [id->vijual-id nodes]} (network-vijual-graph compiled network)]
    (println "compiler_2 source:")
    (println source-text)
    (println)
    (doseq [stage (semantic-progression compiled network)]
      (draw-semantic-stage stage))
    (doseq [stage (concat stages (closure-body-progression network))]
      (draw-stage stage))
    (print-closures network)
    (println (str "result cell: " (get nodes (id->vijual-id (:cell compiled)))))))
