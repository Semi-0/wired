(ns propagators.tui.experimental.view-xr-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [propagators.tui.graph.xr-runtime :as xr]
            [propagators.runtime.session.file-loader :as loader]
            [propagators.tui.adapters.extensions.visualization :as extension]
            [propagators.infra.experimental.visualization.data :as data]
            [propagators.runtime.experimental.visualization.interaction :as interaction]
            [propagators.runtime.experimental.visualization.layered-primitives :as primitives]
            [propagators.tui.experimental.view-collections-test :as collections]
            [propagators.tui.experimental.visualization-composition-test :as fixture]
            [propagators.infra.network :as net]
            [propagators.infra.propagator :as prop]
            [propagators.infra.relationship-observer :as observer]))

(def options {:extensions [primitives/session-extension extension/extension]})
(def source
  "(def-cells selection)
   (def raw (list 2 3))
   (def values (map (:: [x] (tracked+ x 1)) raw))
   (def zoom (map (:: [ref] (strongest-of ref)) (sources-of (focus values selection))))
   (xr:io (juxtapose (selectable values selection) zoom))")

(defn command [session index]
  (let [view (first (filter :view/selection-cell (interaction/published-views @session)))
        json (xr/view->json view)]
    {:op :xr/view-select :view-id (:id json) :epoch (:epoch json)
     :generation (:generation json)
     :revision (:revision json) :item-id (:id (nth (:items json) index))}))

(deftest published-view-is-still-composable
  (let [session (loader/load-session-from-source source options)
        before @session
        raw-id (fixture/binding-id before 'raw)
        raw (net/network-cell-content (:program/net before) raw-id)
        views (mapv xr/view->json (interaction/published-views before))]
    (is (= ["collection" "collection"] (mapv :type views)))
    (is (= [true false] (mapv :selectable views)))
    (is (= 2 (count (:items (first views)))))
    (is (empty? (:items (collections/result before 'zoom))))
    (xr/handle-command! session (command session 0))
    (is (= [2] (mapv :payload (:items (collections/result @session 'zoom)))))
    (xr/handle-command! session (command session 1))
    (is (= [3] (mapv :payload (:items (collections/result @session 'zoom)))))
    (is (= raw (net/network-cell-content (:program/net @session) raw-id)))))

(deftest reject-unpublished-and-stale-selection
  (let [session (loader/load-session-from-source source options)
        select (command session 0)]
    (doseq [bad [(assoc select :item-id "unknown")
                 (assoc select :view-id "unpublished")
                 (assoc select :epoch -1)
                 (assoc select :revision "old")]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Stale or unpublished"
                           (xr/handle-command! session bad))))
    (is (empty? (:items (collections/result @session 'zoom))))))

(deftest full-environment-reload-clears-selection
  (let [session (loader/load-session-from-source source options)
        old-command (command session 0)]
    (xr/handle-command! session old-command)
    (loader/replace-session-from-source! session source options)
    (is (empty? (:items (collections/result @session 'zoom))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Stale or unpublished"
                         (xr/handle-command! session old-command)))
    (xr/handle-command! session (command session 1))
    (is (= [3] (mapv :payload (:items (collections/result @session 'zoom)))))))

(deftest executable-chain-example
  (let [session (loader/load-session-from-file
                 "examples/lain/visualization_combinators/chain.lain" options)]
    (is (= #{5 7} (set (map :payload (:items (collections/result @session 'values))))))
    (is (= 2 (count (:items (collections/result @session 'boundary)))))
    (let [views (mapv xr/view->json (interaction/published-views @session))]
      (is (= ["graph" "collection" "collection" "collection"
              "collection" "collection" "collection" "graph"]
             (mapv :type views)))
      (is (= [false false false true false false false false]
             (mapv (comp boolean :selectable) views)))
      (is (seq (get-in views [0 :graph :nodes])))
      (is (seq (get-in views [7 :graph :edges])))
      (xr/handle-command! session (command session 0))
      (is (seq (:items (collections/result @session 'zoom)))))
    (let [graph (net/network-cell-strongest (:program/net @session)
                 (fixture/binding-id @session 'composition-graph))]
      (is (seq (:nodes graph)))
      (is (seq (:edges graph))))
    (let [network (:program/net @session)
          selected (data/payload (net/network-cell-strongest network
                                  (fixture/binding-id @session 'selected)))
          parent [(:source/path selected) (:source/cell selected)]
          children (disj ((observer/child-node-keys parent) network) parent)
          graph (observer/snapshot network (conj children parent))]
      (is (seq children))
      (is (some #(prop/prop? (observer/node-entry network %)) children))
      (is (some (fn [[from to]] (and (= from parent) (contains? children to))) (:edges graph))))))

(deftest external-file-write-recompiles-the-entire-environment
  (let [file (java.io.File/createTempFile "view-reload-" ".lain")
        replacement (java.io.File/createTempFile "view-replacement-" ".lain")]
    (try
      (spit file source)
      (spit replacement (str/replace source "(list 2 3)" "(list 8 9)"))
      (let [session (loader/load-session-from-file file options)
            old-command (command session 0)
            loaded (promise)
            watcher (loader/watch-file! session file
                       (assoc options :interval-ms 25 :on-reload #(deliver loaded %)
                                      :on-error #(deliver loaded %)))]
        (try
          (xr/handle-command! session old-command)
          ;; A separate OS process writes the source, just as an editor would.
          (let [process (.start (ProcessBuilder. ^java.util.List
                                  ["cp" (.getPath replacement) (.getPath file)]))]
            (is (= 0 (.waitFor process))))
          (let [result (deref loaded 20000 :timeout)]
            (is (map? result) (str result)))
          (is (= [9 10] (mapv :payload (:items (collections/result @session 'values)))))
          (is (empty? (:items (collections/result @session 'zoom))))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Stale or unpublished"
                               (xr/handle-command! session old-command)))
          (finally ((:close watcher)))))
      (finally (.delete file) (.delete replacement)))))
