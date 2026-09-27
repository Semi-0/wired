(ns examples.lain.visualization-combinators.benchmark
  "Small, repeatable cold-topology experiment; not a production throughput claim."
  (:require [clojure.string :as str]
            [propagators.compiler.model.env :as env]
            [propagators.runtime.session.file-loader :as loader]
            [propagators.infra.core :as core]
            [propagators.infra.datastructures.dependency :as dependency]
            [propagators.infra.experimental.visualization.data :as data]
            [examples.lain.visualization-combinators.demo :as demo]
            [propagators.infra.message :refer [message]]
            [propagators.infra.network :as net]
            [propagators.infra.network-builder :as nb]
            [propagators.infra.propagator :as prop]
            [propagators.infra.runner :as runner]))

(defn- binding-id [state symbol]
  (env/resolve-binding-id (:program/net state) (:program/env state) symbol))

(defn- direct-source [n]
  (str "(def-cells " (str/join " " (map #(str "ref" %) (range n))) ")\n"
       (str/join "\n" (map #(str "(def out" % " (tracked+ (strongest-of ref" % ") 1))") (range n)))))

(defn- populate-direct [state n]
  (let [raw (binding-id state 'raw)
        updates (mapv (fn [index]
                        (message (binding-id state (symbol (str "ref" index)))
                                 (data/reference [:outer] raw (conj (vec (repeat index :cdr)) :car))))
                      (range n))
        [tasks network] (core/eval-cells updates (:program/net state))]
    (assoc state :program/net (nb/run-propagators network tasks))))

(defn- count-transitions [counter]
  (fn [advance]
    (fn [state continuations]
      (when-not ((:tasks-empty? runner/fifo-task-policy) (:tasks state))
        (swap! counter inc))
      (advance state continuations))))

(defn measure [mode n]
  (let [session (loader/load-session-from-source
                 (str "(def raw (list " (str/join " " (range n)) "))") demo/options)
        before (count (net/net-env (:program/net @session)))
        code (case mode
               :generic "(def result (map (:: [x] (tracked+ x 1)) raw))"
               :direct (direct-source n))
        activations (atom 0)
        start (System/nanoTime)]
    (binding [runner/*advance-transform* (count-transitions activations)]
      (loader/load-source! session code)
      (when (= :direct mode) (swap! session populate-direct n)))
    (let [elapsed (/ (- (System/nanoTime) start) 1e6)
          network (:program/net @session)
          rows (case mode
                 :generic (:items (data/resolve-collection network
                                    (net/network-cell-strongest network (binding-id @session 'result))))
                 :direct (mapv (fn [index]
                                 {:value (binding-id @session (symbol (str "out" index)))}) (range n)))
          projected (mapv (fn [row]
                            (let [v (net/network-cell-strongest network (:value row))]
                              {:value (data/payload v)
                               :sources (dependency/sources (data/evidence-value v))})) rows)
          rerun (nb/run-propagators network
                  (keep (fn [[id entry]] (when (prop/prop? entry) id)) (net/net-env network)))]
      {:mode mode :n n :milliseconds elapsed :activations @activations
       :source-characters (count code) :added-nodes (- (count (net/net-env network)) before)
       :rerun-added-nodes (- (count (net/net-env rerun)) (count (net/net-env network)))
       :projection projected})))

(defn -main [& sizes]
  ;; Warm namespace/runtime machinery before reporting either alternative.
  (doseq [mode [:direct :generic]] (measure mode 1))
  (doseq [n (if (seq sizes) (map parse-long sizes) [2 8 16])]
    (let [direct (measure :direct n)
          generic (measure :generic n)]
      (prn {:n n :equivalent? (= (:projection direct) (:projection generic))
            :direct (dissoc direct :projection) :generic (dissoc generic :projection)})))
  (shutdown-agents))
