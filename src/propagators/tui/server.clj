(ns propagators.tui.server
  "Stable runtime server entrypoint."
  (:require [propagators.tui.graph.compiler-2-runtime-server :as server]))

(defn -main
  [& args]
  (apply server/-main args))
