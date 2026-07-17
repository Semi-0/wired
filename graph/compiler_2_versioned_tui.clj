(ns graph.compiler-2-versioned-tui
  "Premise-versioned Charm client for the shared compiler-2 runtime."
  (:require [charm.components.text-input :as text-input]
            [charm.components.viewport :as viewport]
            [charm.program :as program]
            [clojure.string :as str]
            [graph.compiler-2-runtime-server :as server]
            [graph.compiler-2-tui :as legacy-tui]
            [graph.compiler-2-versioned-tui.editor :as editor]
            [propagators.cells.value :as value])
  (:import [java.util UUID]))

(defn new-commit-id [] (str (UUID/randomUUID)))

(defn render-blocks
  [{:keys [blocks]} selected]
  (str/join
   "\n\n"
   (map-indexed
    (fn [position {:keys [index version source value warnings]}]
      (str (if (= position selected) "> " "  ")
           "[" index " v" (or version "-") "]"
           (when (some? source) (str "\n" source))
           (when (seq warnings)
             (str "\n! " (str/join ", " (map (comp name :warning) warnings))))
           (when (and (not (value/nothing? value))
                      (not= source value))
             (str "\n=> " (legacy-tui/render-value value)))))
    blocks)))

(defn configure-viewport
  [model]
  (assoc model :viewport
         (viewport/viewport-set-content
          (:viewport model)
          (render-blocks (:view model) (:selected model)))))

(defn poll-cmd
  [{:keys [host port client-id poll-ms]}]
  (program/cmd
   (fn []
     (Thread/sleep (long (or poll-ms 0)))
     {:type :runtime/view
      :response (server/request host port {:op :tui/read-view
                                           :client-id client-id})})))

(defn commit-cmd
  [model]
  (let [request (editor/commit-request model)]
    (program/cmd
     (fn []
       {:type :runtime/commit
        :response (server/request (:host model) (:port model) request)}))))

(defn init-model
  [{:keys [host port client-id poll-ms]
    :or {host server/default-host port server/default-port
         client-id "versioned-1" poll-ms 1000}}]
  {:host host
   :port port
   :client-id client-id
   :poll-ms poll-ms
   :view {:blocks []}
   :selected 0
   :input (text-input/text-input :prompt "edit> ")
   :viewport (viewport/viewport "" :height 20)
   :error nil})

(defn update-fn
  [model message]
  (cond
    (= :runtime/view (:type message))
    (if (get-in message [:response :ok])
      [(-> model
           (editor/apply-poll (get-in message [:response :result]))
           (assoc :error nil)
           configure-viewport)
       (poll-cmd model)]
      [(assoc model :error (get-in message [:response :error]))
       (poll-cmd model)])

    (= :runtime/commit (:type message))
    (if (get-in message [:response :ok])
      [(-> model editor/accept-commit (assoc :error nil))
       (poll-cmd (assoc model :poll-ms 0))]
      [(assoc model
              :error (get-in message [:response :error])
              :stale? (= "stale block version"
                         (get-in message [:response :error])))
       nil])

    (= :quit (editor/action message))
    [model program/quit-cmd]

    (and (not (:editing? model))
         (= :select-up (editor/action message)))
    [(-> model (editor/select -1) configure-viewport) nil]

    (and (not (:editing? model))
         (= :select-down (editor/action message)))
    [(-> model (editor/select 1) configure-viewport) nil]

    (= :cancel (editor/action message))
    [(-> model editor/cancel-edit (update :input text-input/reset)) nil]

    (and (not (:editing? model))
         (= :edit (editor/action message)))
    (let [model (editor/begin-edit model (new-commit-id))]
      [(update model :input text-input/set-value (:draft model "")) nil])

    (= :commit (editor/action message))
    (if (:editing? model)
      (if (editor/commit-request model)
        [(editor/mark-attempted model) (commit-cmd model)]
        [model nil])
      [model nil])

    (:editing? model)
    (let [[input cmd] (text-input/text-input-update (:input model) message)
          text (text-input/value input)]
      [(cond-> (assoc model :input input)
         (not= text (:draft model))
         (editor/edit-draft text (new-commit-id)))
       cmd])

    :else
    (let [[viewport cmd] (viewport/viewport-update (:viewport model) message)]
      [(assoc model :viewport viewport) cmd])))

(defn view
  [{:keys [client-id viewport input editing? stale? error]}]
  (str "compiler-2 versioned " client-id "\n"
       "Up/Down select, Enter edit, Ctrl+T commit, Esc discard, Ctrl+C quit\n"
       (when stale? "STALE: refresh before committing\n")
       (when error (str "error: " error "\n"))
       "\n" (viewport/viewport-view viewport)
       (when editing? (str "\n\n" (text-input/text-input-view input)))))

(defn run-client
  [{:keys [host port client-id poll-ms] :as opts
    :or {host server/default-host port server/default-port
         client-id "versioned-1" poll-ms 1000}}]
  (let [registered (server/request host port {:op :tui/register
                                              :client-id client-id
                                              :mode :versioned-premise})]
    (when-not (:ok registered)
      (throw (ex-info "failed to register versioned TUI" {:response registered}))))
  (try
    (let [model (init-model (assoc opts :host host :port port
                                   :client-id client-id :poll-ms poll-ms))]
      (program/run {:init (fn [] [model (poll-cmd (assoc model :poll-ms 0))])
                    :update update-fn
                    :view view
                    :alt-screen false}))
    (finally
      (server/request host port {:op :tui/unregister :client-id client-id}))))

(defn- parse-args
  [args]
  (loop [args args opts {}]
    (if-let [arg (first args)]
      (case arg
        ("-name" "--name" "--client-id")
        (recur (nnext args) (assoc opts :client-id (second args)))
        ("-port" "--port")
        (recur (nnext args) (assoc opts :port (Long/parseLong (second args))))
        (recur (next args) (assoc opts :client-id arg)))
      opts)))

(defn -main [& args]
  (run-client (parse-args args)))
