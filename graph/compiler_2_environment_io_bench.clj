(ns graph.compiler-2-environment-io-bench
  "Diagnostic load/save receipt for plain S-expression environment IO."
  (:require [clojure.string :as str]
            [propagators.compiler-2.runtime.boundary.effects :as effects]
            [propagators.compiler-2.runtime.session.environment-io :as environment-io]
            [propagators.compiler-2.runtime.session.state :as state]
            [propagators.network :as net]
            [propagators.propagator :as prop])
  (:import [java.nio.file Files]))

(defn elapsed-ms [started]
  (/ (double (- (System/nanoTime) started)) 1000000.0))

(defn temp-file [suffix]
  (.toFile (Files/createTempFile "compiler-2-environment-bench-" suffix
                                 (make-array java.nio.file.attribute.FileAttribute 0))))

(defn topology-counts [runtime-state]
  (let [entries (vals (net/net-env (:program/net runtime-state)))]
    {:cells (count (remove prop/prop? entries))
     :propagators (count (filter prop/prop? entries))
     :retained-source-forms (count (:environment/source-ledger runtime-state))
     :primitive-imports (count (:environment/primitive-imports runtime-state))}))

(defn benchmark [form-count]
  (let [input (temp-file ".lain")
        output (temp-file ".lain")
        forms (map #(list 'def (symbol (str "value-" %)) %) (range form-count))
        source (str (str/join "\n" (map pr-str forms)) "\n")
        filesystem-started (System/nanoTime)
        _ (spit input source)
        filesystem-write-ms (elapsed-ms filesystem-started)
        load-started (System/nanoTime)
        loaded (environment-io/load-lain-state
                (state/empty-state)
                {:file (.getPath input) :revision 0}
                effects/drain-environment-effects)
        load-ms (elapsed-ms load-started)
        save-started (System/nanoTime)
        saved (environment-io/save-state
               (:state loaded)
               {:file (.getPath output)
                :mode :commit-supported
                :checkpoint-id (str "bench-" form-count)})
        save-ms (elapsed-ms save-started)]
    (merge {:forms form-count
            :load-ms load-ms
            :save-ms save-ms
            :filesystem-source-write-ms filesystem-write-ms
            :load-status (get-in loaded [:receipt :status])
            :save-status (get-in saved [:receipt :status])
            :exported-forms (get-in saved [:receipt :exported-form-count])}
           (topology-counts (:state loaded)))))

(defn -main [& _]
  (doseq [form-count [1 10 50]]
    (prn (benchmark form-count))))
