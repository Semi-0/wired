(ns graph.compiler-2-versioned-tui.trace-render-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [graph.compiler-2-versioned-tui :as tui]
            [propagators.compiler-2.runtime :as runtime]
            [propagators.semantic-trace :as semantic-trace]))

(defn- commit!
  [session index expected-version source]
  (runtime/handle-command!
   session
   {:op :tui/commit-version
    :client-id "B"
    :commit-id (str (random-uuid))
    :index index
    :expected-version expected-version
    :text source}))

(defn- commit-next!
  [session source]
  (let [block (peek (:blocks (runtime/read-tui-view
                              @session {:client-id "B"})))]
    (commit! session (:index block) (:version block) source)))

(defn- wait-until
  [pred]
  (loop [attempt 0]
    (cond
      (pred) true
      (= attempt 200) false
      :else (do
              (Thread/sleep 10)
              (recur (inc attempt))))))

(deftest versioned-tui-renders-published-trace-as-graph
  (let [session (runtime/new-session)]
    (runtime/handle-command! session {:op :tui/register
                                      :client-id "B"
                                      :mode :versioned-premise})
    (doseq [source ["(def a)"
                    "(<-> (+ 1 2) a)"
                    "(def g)"
                    "(trace a :upstream g)"]]
      (is (:ok (commit-next! session source))))
    (is (wait-until #(pos? (long (or (:trace/published-results @session) 0)))))
    (let [view (runtime/read-tui-view @session {:client-id "B"})
          graph (get-in view [:blocks 4 :value])
          rendered (tui/render-blocks view 0)]
      (is (semantic-trace/semantic-trace-graph? graph))
      (is (str/includes? rendered "| a |"))
      (is (not (str/includes? rendered "\n=> ")))
      (is (not (str/includes? rendered "propagators.network.Net@"))))
    (let [published (:trace/published-results @session)]
      (is (:ok (commit! session 1 0 "(<-> (+ 10 20) a)")))
      (is (wait-until #(> (long (or (:trace/published-results @session) 0))
                          published))))
    (let [view (runtime/read-tui-view @session {:client-id "B"})
          graph (get-in view [:blocks 4 :value])
          labels (set (vals (:nodes graph)))
          rendered (tui/render-blocks view 0)]
      (is (semantic-trace/semantic-trace-graph? graph))
      (is (contains? labels "10"))
      (is (contains? labels "20"))
      (is (str/includes? rendered "| a |")))))

(deftest versioned-io-xr-launches-published-trace
  (let [session (runtime/new-session)]
    (runtime/handle-command! session {:op :tui/register
                                      :client-id "B"
                                      :mode :versioned-premise})
    (doseq [source ["(def a)"
                    "(<-> (+ 1 2) a)"
                    "(def g)"
                    "(trace a :upstream g)"]]
      (is (:ok (commit-next! session source))))
    (is (wait-until #(pos? (long (or (:trace/published-results @session) 0)))))
    (is (:ok (commit-next! session "(io:xr g)")))
    (is (wait-until #(pos? (count (get-in @session [:xr :effects])))))
    (let [effect (peek (get-in @session [:xr :effects]))
          graph (get-in effect [:boundary/payload :graph])]
      (is (= :xr/launch-trace (:boundary/kind effect)))
      (is (semantic-trace/semantic-trace-graph? graph))
      (is (contains? (set (vals (:nodes graph))) "a")))))
