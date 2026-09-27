(ns examples.lain.static-call-graph.main
  "Run the static call-graph Lain example with the existing XR renderer."
  (:require [clojure.java.io :as io]
            [examples.lain.static-call-graph.extension :as call-graph]
            [propagators.tui.graph.compiler-2-runtime-server :as runtime-server]
            [propagators.tui.adapters.extensions.relationship-xr :as relationship-xr]
            [propagators.runtime.session.file-loader :as file-loader]))

(def default-source "examples/lain/static_call_graph/program.lain")

(defn start-example
  ([] (start-example {}))
  ([{:keys [project-root source-file port xr-port]
     :or {project-root (System/getProperty "user.dir")
          port 45555
          xr-port 45666}}]
   (let [source-file (or source-file
                         (.getPath (io/file project-root default-source)))
         extensions [(relationship-xr/extension)
                     (call-graph/extension project-root)]
         loader-opts {:extensions extensions}
         server (runtime-server/start-server-with-xr port xr-port)]
     (try
       (file-loader/replace-session-from-file!
        (:session server) source-file loader-opts)
       (let [watcher
             (file-loader/watch-file!
              (:session server)
              source-file
              (assoc loader-opts
                     :on-reload
                     (fn [_]
                       (println (str "reloaded " source-file)))
                     :on-error
                     (fn [error]
                       (println (str "reload failed: " (ex-message error))))))]
         (reset! (:file-watch-state server) watcher)
         (assoc server
                :source-file source-file
                :project-root project-root))
       (catch Throwable error
         ((:close server))
         (throw error))))))

(defn -main
  [& [source-file]]
  (let [server (start-example (cond-> {}
                                source-file
                                (assoc :source-file source-file)))]
    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread. ^Runnable (:close server) "static-call-graph-shutdown"))
    (println (str "watching " (:source-file server)))
    (println (str "call graph: http://127.0.0.1:"
                  (get-in server [:xr :port])
                  "/relationships"))
    @(promise)))
