(ns graph.compiler-2-versioned-tui.layout-test
  (:require [charm.components.viewport :as viewport]
            [charm.message :as msg]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [graph.compiler-2-versioned-tui :as tui]
            [graph.compiler-2-versioned-tui.layout :as layout]))

(defn- blocks
  [n]
  (mapv (fn [index]
          {:index index
           :version 0
           :source (str "block-" index)
           :value (str "block-" index)})
        (range n)))

(defn- model-with-blocks
  [n]
  (-> (tui/init-model {:client-id "resize-test" :poll-ms 10})
      (assoc :view {:blocks (blocks n)})
      tui/configure-viewport))

(defn- resize
  [model width height]
  (first (tui/update-fn model (msg/window-size width height))))

(deftest resize-updates-versioned-viewport-immediately
  (let [model (model-with-blocks 3)
        resized (resize model 40 8)]
    (is (= {:width 40 :height 8} (:window-size resized)))
    (is (= {:width 40 :height 5}
           (select-keys (:viewport resized) [:width :height])))
    (is (= (:selected model) (:selected resized)))
    (is (nil? (:resize/target resized)))
    (is (nil? (:resize/suppress? resized)))))

(deftest consecutive-resizes-use-each-current-size
  (let [wide (resize (model-with-blocks 4) 100 30)
        narrow (resize wide 24 7)]
    (is (= {:width 100 :height 27}
           (select-keys (:viewport wide) [:width :height])))
    (is (= {:width 24 :height 4}
           (select-keys (:viewport narrow) [:width :height])))))

(deftest chrome-height-accounts-for-editing-stale-and-error-state
  (is (= 3 (layout/chrome-height {})))
  (is (= 5 (layout/chrome-height {:editing? true})))
  (is (= 7 (layout/chrome-height {:editing? true
                                  :stale? true
                                  :error "failed"})))
  (let [model (-> (model-with-blocks 2)
                  (assoc :editing? true :stale? true :error "failed"))
        resized (resize model 10 4)]
    (is (= {:width 10 :height 1}
           (select-keys (:viewport resized) [:width :height])))))

(deftest resize-preserves-editor-state-and-truncates-long-lines
  (let [model (-> (model-with-blocks 2)
                  (assoc :selected 0)
                  tui/configure-viewport)
        [editing _] (tui/update-fn model (msg/key-press :enter))
        [drafted _] (tui/update-fn editing (msg/key-press "x"))
        resized (resize drafted 12 8)
        visible (viewport/viewport-view (:viewport resized))]
    (is (= (select-keys drafted [:editing? :draft :draft-commit-id
                                 :expected-version :selected])
           (select-keys resized [:editing? :draft :draft-commit-id
                                 :expected-version :selected])))
    (is (= 3 (get-in resized [:viewport :height])))
    (is (every? #(<= (count %) 12) (str/split-lines visible)))))

(deftest polling-preserves-scroll-unless-selection-leaves-the-window
  (let [view {:blocks (blocks 10)}
        model (-> (tui/init-model {:client-id "poll" :poll-ms 10})
                  (assoc :view view)
                  (resize 40 8)
                  (update :viewport viewport/viewport-scroll-to 4))
        original-viewport (:viewport model)
        [unchanged _] (tui/update-fn model {:type :runtime/view
                                             :response {:ok true :result view}})
        changed-view (assoc-in view [:blocks 9 :source] "changed")
        [changed _] (tui/update-fn unchanged {:type :runtime/view
                                               :response {:ok true
                                                          :result changed-view}})
        focused-view (assoc changed-view
                            :focus {:index 9 :sequence 1})
        [focused _] (tui/update-fn changed {:type :runtime/view
                                             :response {:ok true
                                                        :result focused-view}})
        focused-line (layout/selected-line-offset
                      (tui/rendered-blocks (:view focused) (:selected focused))
                      (:selected focused))]
    (is (identical? original-viewport (:viewport unchanged)))
    (is (= 4 (get-in changed [:viewport :y-offset])))
    (is (= 9 (:selected focused)))
    (is (layout/line-visible? (:viewport focused) focused-line))
    (is (< (get-in focused [:viewport :y-offset]) focused-line))))

(deftest visible-selection-does-not-get-pinned-to-the-top
  (let [model (resize (model-with-blocks 5) 40 12)
        [selected _] (tui/update-fn model (msg/key-press :down))
        selected-line (layout/selected-line-offset
                       (tui/rendered-blocks (:view selected)
                                            (:selected selected))
                       (:selected selected))]
    (is (pos? selected-line))
    (is (zero? (get-in selected [:viewport :y-offset])))
    (is (layout/line-visible? (:viewport selected) selected-line))))

(deftest versioned-tui-uses-alternate-screen
  (is (true? (:alt-screen
              (tui/program-options
               (tui/init-model {:client-id "screen-test"}))))))
