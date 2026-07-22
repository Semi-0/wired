(ns graph.compiler-2-versioned-tui.layout
  "Pure terminal layout for the premise-versioned TUI."
  (:require [charm.components.viewport :as viewport]
            [clojure.string :as str]))

(def default-viewport-width 0)
(def default-viewport-height 20)

(defn chrome-height
  "Lines outside the viewport for the current model state."
  [{:keys [editing? stale? error]}]
  (+ 3
     (if stale? 1 0)
     (if error 1 0)
     (if editing? 2 0)))

(defn viewport-dimensions
  [{:keys [window-size viewport] :as model}]
  (if window-size
    {:width (max 1 (long (:width window-size 1)))
     :height (max 1 (- (long (:height window-size 1))
                       (chrome-height model)))}
    {:width (long (or (:width viewport) default-viewport-width))
     :height (long (or (:height viewport) default-viewport-height))}))

(defn selected-line-offset
  "Return the first rendered line of the selected block."
  [rendered-blocks selected]
  (->> rendered-blocks
       (take (max 0 (long (or selected 0))))
       (map #(inc (count (str/split-lines %))))
       (reduce + 0)))

(defn- set-dimensions
  [vp {:keys [width height]}]
  (if (= [width height] [(:width vp) (:height vp)])
    vp
    (viewport/viewport-set-dimensions vp width height)))

(defn- set-content
  [vp content]
  (if (= content (viewport/viewport-content vp))
    vp
    (viewport/viewport-scroll-to
     (viewport/viewport-set-content vp content)
     (:y-offset vp 0))))

(defn line-visible?
  [vp line]
  (let [offset (:y-offset vp 0)
        height (max 1 (:height vp 1))]
    (<= offset line (dec (+ offset height)))))

(defn ensure-line-visible
  [vp line]
  (let [offset (:y-offset vp 0)
        height (max 1 (:height vp 1))]
    (cond
      (< line offset)
      (viewport/viewport-scroll-to vp line)

      (>= line (+ offset height))
      (viewport/viewport-scroll-to vp (inc (- line height)))

      :else vp)))

(defn configure-viewport
  "Apply dimensions and content, optionally revealing the selected block."
  [model content selected-line ensure-selection?]
  (let [vp (-> (:viewport model)
               (set-dimensions (viewport-dimensions model))
               (set-content content))
        vp (if ensure-selection?
             (ensure-line-visible vp selected-line)
             vp)]
    (assoc model :viewport vp)))

(defn apply-window-size
  [model window-size content selected-line]
  (-> model
      (assoc :window-size (select-keys window-size [:width :height]))
      (configure-viewport content selected-line true)))
