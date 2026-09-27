(ns examples.lain.visualization-combinators.demo
  (:require [propagators.tui.graph.xr-server :as server]
            [propagators.runtime :as runtime]
            [propagators.runtime.session.file-loader :as loader]
            [propagators.tui.adapters.extensions.visualization :as extension]
            [propagators.runtime.experimental.visualization.layered-primitives :as primitives]))

(def options {:extensions [primitives/session-extension extension/extension]})
(def example "examples/lain/visualization_combinators/chain.lain")

(defn start!
  ([file port] (start! file port server/default-host))
  ([file port host]
  (let [session (loader/load-session-from-file file options)
        http (server/start-server port session host)
        watcher (loader/watch-file! session file
                   (assoc options :on-reload #(println "Reloaded" (:file %))
                                  :on-error #(binding [*out* *err*] (println (ex-message %)))))]
    {:session session :server http
     :close (fn []
              ((:close watcher))
              ((:close http))
              (runtime/stop-clocks! session))})))

(defn -main [& [file port host]]
  (let [running (start! (or file example) (Long/parseLong (or port "45668"))
                        (or host server/default-host))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable (:close running)))
    (println (str "View composition: http://" (get-in running [:server :host])
                  ":" (get-in running [:server :port]) "/relationships"))
    @(promise)))
