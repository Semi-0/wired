(ns run-tests
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(def test-timeout-ms 2000)
(def timeout-result ::timeout)

(defn- test-files []
  (->> (file-seq (io/file "test"))
       (filter #(.isFile %))
       (filter #(str/ends-with? (.getName %) "_test.clj"))
       (sort-by #(.getPath %))))

(defn- test-namespace [file]
  (let [path (.getPath file)
        relative (subs path (count "test/"))]
    (-> relative
        (str/replace #"\.clj$" "")
        (str/replace "_" "-")
        (str/replace "/" ".")
        symbol)))

(defn- test-vars [namespace]
  (->> (ns-publics namespace)
       vals
       (filter #(-> % meta :test))
       (sort-by #(-> % meta :name str))))

(defn- run-test-var [test-var]
  (let [started (System/nanoTime)
        task (future
               (let [counters (ref test/*initial-report-counters*)]
                 (binding [test/*report-counters* counters]
                   (test/test-var test-var))
                 @counters))
        result (deref task test-timeout-ms timeout-result)
        elapsed-ms (/ (- (System/nanoTime) started) 1000000.0)]
    (cond
      (= timeout-result result)
      (do
        (future-cancel task)
        (println "TIMEOUT in" (-> test-var meta :ns ns-name)
                 "/" (-> test-var meta :name)
                 "after" test-timeout-ms "ms")
        (flush)
        (System/exit 124))

      :else
      (assoc result :elapsed-ms elapsed-ms))))

(defn- merge-counters [left right]
  {:test (+ (:test left 0) (:test right 0))
   :pass (+ (:pass left 0) (:pass right 0))
   :fail (+ (:fail left 0) (:fail right 0))
   :error (+ (:error left 0) (:error right 0))})

(defn -main [& _]
  (let [namespaces (mapv test-namespace (test-files))]
    (doseq [namespace namespaces]
      (require namespace))
    (let [{:keys [test pass fail error]}
          (reduce merge-counters
                  test/*initial-report-counters*
                  (mapcat (fn [namespace]
                            (println "\nTesting" namespace)
                            (mapv run-test-var (test-vars namespace)))
                          namespaces))]
      (println :tests test :assertions pass :failures fail :errors error)
      (cond
        (zero? (+ fail error))
        nil

        :else
        (System/exit 1)))))
