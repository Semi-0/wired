(ns propagators.tui.graph.compiler-2-semantic-repl
  "Interactive TUI adapter for the pure runtime semantic graph projection."
  (:require [clojure.string :as str]
            [propagators.tui.graph.vijual :as v]
            [propagators.runtime.inspection.semantic-graph :as semantic]))

(def stress-opts
  {:stress-node-spacing 1.7
   :stress-iterations 200
   :stress-refine-iterations 200
   :routing :shortest-path
   :arrow-position :end})

(def display-cell-value semantic/display-cell-value)
(def compiled-semantic-graph semantic/compiled-semantic-graph)
(def compiled-application-semantic-graph
  semantic/compiled-application-semantic-graph)
(def expansion semantic/expansion)
(def semantic-graph semantic/semantic-graph)
(def edge-labels semantic/edge-labels)
(def value-labels semantic/value-labels)

(defn print-graph
  [graph]
  (println (str (count (:nodes graph)) " semantic nodes, "
                (count (:edges graph)) " semantic edges"))
  (doseq [[from to] (edge-labels graph)]
    (println (str "  " from " -> " to)))
  (if (seq (:values graph))
    (do
      (println "values:")
      (doseq [[node-label value-label] (sort-by first (value-labels graph))]
        (println (str "  " node-label " = " value-label))))
    nil)
  (println)
  (v/draw-stress-directed-graph (:edges graph) (:nodes graph) stress-opts))

(defn eval-and-print
  [source]
  (try
    (print-graph (semantic-graph source))
    (catch Throwable error
      (println (str "error: " (ex-message error))))))

(defn repl
  []
  (println "compiler-2 semantic graph REPL. One expression per line; :quit exits.")
  (loop []
    (print "sem> ")
    (flush)
    (let [line (read-line)]
      (if (nil? line)
        nil
        (let [trimmed (str/trim line)]
          (if (= trimmed ":quit")
            nil
            (do
              (if (str/blank? trimmed)
                nil
                (eval-and-print trimmed))
              (recur))))))))

(defn -main
  [& args]
  (if (seq args)
    (eval-and-print (str/join " " args))
    (repl)))
