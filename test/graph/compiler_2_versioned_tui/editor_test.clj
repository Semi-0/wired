(ns graph.compiler-2-versioned-tui.editor-test
  (:require [charm.message :as msg]
            [clojure.test :refer [deftest is testing]]
            [graph.compiler-2-versioned-tui :as tui]
            [graph.compiler-2-versioned-tui.editor :as editor]
            [propagators.cells.value :as value]))

(def view0
  {:blocks [{:index 0 :version 0 :source "1" :value 1}
            {:index 1 :version nil :source nil :value :bool4/nothing}]})

(def model0 {:client-id "A" :view view0 :selected 0})

(deftest selection-and-key-actions-are-bounded
  (is (= :select-up (editor/action (msg/key-press :up))))
  (is (= :select-down (editor/action (msg/key-press :down))))
  (is (= :edit (editor/action (msg/key-press :enter))))
  ;; Retain the original shortcuts as compatibility aliases.
  (is (= :select-up (editor/action (msg/key-press :up :ctrl true))))
  (is (= :select-down (editor/action (msg/key-press :down :ctrl true))))
  (is (= :edit (editor/action (msg/key-press " "))))
  (is (= :commit (editor/action (msg/key-press "t" :ctrl true))))
  (is (= :cancel (editor/action (msg/key-press :esc))))
  (is (= 0 (:selected (editor/select model0 -1))))
  (is (= 1 (:selected (editor/select model0 8)))))

(deftest plain-navigation-and-enter-drive-view-mode
  (let [model (-> (tui/init-model {:client-id "A"})
                  (assoc :view view0)
                  tui/configure-viewport)
        [selected _] (tui/update-fn model (msg/key-press :down))
        [editing _] (tui/update-fn selected (msg/key-press :enter))
        [still-editing _] (tui/update-fn editing (msg/key-press :up))
        [cancelled _] (tui/update-fn still-editing (msg/key-press :esc))]
    (is (= 1 (:selected selected)))
    (is (:editing? editing))
    (is (= "" (apply str (get-in editing [:input :value] [])))
        "the trailing block starts as new code")
    (is (:editing? still-editing) "editing owns arrow keys")
    (is (not (:editing? cancelled)))))

(deftest editing-loads-source-and-commit-does-not-create-an-edit
  (let [model (-> (tui/init-model {:client-id "A"})
                  (assoc :view view0)
                  tui/configure-viewport)
        [editing _] (tui/update-fn model (msg/key-press :enter))
        [unchanged cmd] (tui/update-fn model
                                       (msg/key-press "t" :ctrl true))]
    (is (= "1" (apply str (get-in editing [:input :value]))))
    (is (:editing? editing))
    (is (= model unchanged))
    (is (nil? cmd))))

(deftest draft-is-local-retryable-and-stale-aware
  (let [editing (editor/begin-edit model0 "commit-0")
        attempted (editor/mark-attempted editing)
        changed (editor/edit-draft attempted "2" "commit-1")
        polled (editor/apply-poll changed
                                  (assoc-in view0 [:blocks 0 :version] 1))]
    (is (= "1" (:draft editing)))
    (is (= "commit-0" (:commit-id
                         (editor/commit-request editing))))
    (is (= "commit-1" (:draft-commit-id changed)))
    (is (false? (:attempted? changed)))
    (is (= "2" (:draft polled)) "polling never overwrites a local draft")
    (is (:stale? polled))
    (is (nil? (editor/commit-request polled)))
    (is (not (:editing? (editor/cancel-edit polled))))))

(deftest preview-opens-the-trailing-blank-and-render-marks-selection
  (let [editing (editor/open-trailing-blank model0 "blank-commit")]
    (is (= 1 (:selected editing)))
    (is (= "" (:draft editing)))
    (is (nil? (:expected-version editing)))
    (is (re-find #"> \[1 v-\]" (tui/render-blocks view0 1)))
    (is (not (re-find #"=> nothing" (tui/render-blocks view0 1))))))

(deftest rendering-keeps-source-blocks-simple-and-shows-output-blocks
  (let [rendered (tui/render-blocks
                  {:blocks [{:index 0 :version 0 :source "(+ 1 2)"
                             :value "(+ 1 2)"}
                            {:index 1 :version nil :source nil :value 3}
                            {:index 2 :version nil :source nil
                             :value value/nothing}]}
                  0)]
    (is (= 1 (count (re-seq #"=>" rendered))))
    (is (re-find #"\[1 v-\]\n=> 3" rendered))))
