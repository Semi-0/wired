(ns propagators.tui.main
  "Stable Charm TUI entrypoint."
  (:require [propagators.tui.graph.compiler-2-tui :as tui]))

(defn -main
  [& args]
  (apply tui/-main args))
