(ns graph.compiler-2-runtime.xr-projection
  "XR effect projection for compiler-2 runtime."
  (:require [graph.compiler-2-runtime.state :as state]))

(def require-state state/require-state)

(defn current-graph-values
  [state]
  (get-in state [:graph :values] {}))

(defn overlay-graph-values
  [graph values]
  (if (and graph (seq values))
    (update graph :values merge values)
    graph))

(defn project-xr-effect
  [state request]
  (if (= [(:boundary/port request) (:boundary/kind request)]
         [:xr :xr/launch-trace])
    (update-in request
               [:boundary/payload :graph]
               overlay-graph-values
               (current-graph-values state))
    request))

(defn project-xr-effects
  [state]
  {:effects (mapv #(project-xr-effect state %)
                  (sort-by (juxt :boundary/epoch
                                 (comp pr-str :boundary/id))
                           (get-in (require-state state) [:xr :effects] [])))
   :launched (vals (get-in state [:xr :launched] {}))
   :widgets (get-in state [:xr :widgets] {})
   :changed-cells (mapv pr-str (:runtime/changed-cells state))
   :changed-node-ids (mapv pr-str (:runtime/changed-node-ids state))
   :tui-effects (vec (get-in state [:tui :effects] []))})

(defn read-xr-effects
  [state]
  (project-xr-effects state))
