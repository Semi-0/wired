(ns graph.compiler-2-versioned-tui
  "Premise-versioned Charm client for the shared compiler-2 runtime."
  (:require [charm.components.text-input :as text-input]
            [charm.components.viewport :as viewport]
            [charm.message :as msg]
            [charm.program :as program]
            [clojure.string :as str]
            [graph.compiler-2-runtime-server :as server]
            [graph.compiler-2-tui :as legacy-tui]
            [graph.compiler-2-versioned-tui.editor :as editor]
            [graph.compiler-2-versioned-tui.layout :as layout]
            [propagators.cells.value :as value])
  (:import [java.util UUID]))

(defn new-commit-id [] (str (UUID/randomUUID)))

(defn render-block
  [selected position {:keys [index version source value warnings]}]
  (str (if (= position selected) "> " "  ")
       "[" index " v" (or version "-") "]"
       (when (some? source) (str "\n" source))
       (when (seq warnings)
         (str "\n! " (str/join ", " (map (comp name :warning) warnings))))
       (when (and (not (value/nothing? value))
                  (not= source value))
         (str "\n=> " (legacy-tui/render-value value)))))

(defn rendered-blocks
  [{:keys [blocks]} selected]
  (mapv (partial render-block selected) (range) blocks))

(defn render-blocks
  [view selected]
  (str/join "\n\n" (rendered-blocks view selected)))

(defn- viewport-rendering
  [{:keys [view selected]}]
  (let [blocks (rendered-blocks view selected)]
    {:content (str/join "\n\n" blocks)
     :selected-line (layout/selected-line-offset blocks selected)}))

(defn configure-viewport
  ([model] (configure-viewport model true))
  ([model ensure-selection?]
   (let [{:keys [content selected-line]} (viewport-rendering model)]
     (layout/configure-viewport model content selected-line ensure-selection?))))

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
   :focus-seq 0
   :input (text-input/text-input :prompt "edit> ")
   :viewport (viewport/viewport "" :height 20)
   :error nil})

(defn- handle-runtime-view
  [model message]
  (when (= :runtime/view (:type message))
    (if (get-in message [:response :ok])
      (let [selected (:selected model)
            updated (-> model
                        (editor/apply-poll (get-in message [:response :result]))
                        (assoc :error nil))
            selection-changed? (not= selected (:selected updated))]
        [(configure-viewport updated selection-changed?)
         (poll-cmd updated)])
      (let [updated (assoc model :error (get-in message [:response :error]))]
        [(configure-viewport updated false)
         (poll-cmd updated)]))))

(defn- handle-runtime-commit
  [model message]
  (when (= :runtime/commit (:type message))
    (if (get-in message [:response :ok])
      (let [updated (-> model editor/accept-commit (assoc :error nil))]
        [(configure-viewport updated true)
         (poll-cmd (assoc updated :poll-ms 0))])
      (let [updated (assoc model
                           :error (get-in message [:response :error])
                           :stale? (= "stale block version"
                                      (get-in message [:response :error])))]
        [(configure-viewport updated false) nil]))))

(defn- handle-window-size
  [model message]
  (when (msg/window-size? message)
    (let [{:keys [content selected-line]} (viewport-rendering model)]
      [(layout/apply-window-size model message content selected-line) nil])))

(defn- begin-edit
  [model]
  (let [updated (editor/begin-edit model (new-commit-id))
        updated (update updated :input text-input/set-value (:draft updated ""))]
    (configure-viewport updated true)))

(defn- cancel-edit
  [model]
  (-> model
      editor/cancel-edit
      (update :input text-input/reset)
      (configure-viewport true)))

(defn- handle-editor-action
  [model message]
  (let [action (editor/action message)]
    (cond
      (= :quit action)
      [model program/quit-cmd]

      (and (not (:editing? model)) (= :select-up action))
      [(-> model (editor/select -1) configure-viewport) nil]

      (and (not (:editing? model)) (= :select-down action))
      [(-> model (editor/select 1) configure-viewport) nil]

      (= :cancel action)
      [(cancel-edit model) nil]

      (and (not (:editing? model)) (= :edit action))
      [(begin-edit model) nil]

      (= :commit action)
      (if (:editing? model)
        (if (editor/commit-request model)
          [(editor/mark-attempted model) (commit-cmd model)]
          [model nil])
        [model nil])

      :else nil)))

(defn- handle-editing-input
  [model message]
  (when (:editing? model)
    (let [[input cmd] (text-input/text-input-update (:input model) message)
          text (text-input/value input)]
      [(cond-> (assoc model :input input)
         (not= text (:draft model))
         (editor/edit-draft text (new-commit-id)))
       cmd])))

(defn- handle-viewport-input
  [model message]
  (let [[viewport cmd] (viewport/viewport-update (:viewport model) message)]
    [(assoc model :viewport viewport) cmd]))

(defn update-fn
  [model message]
  (or (handle-runtime-view model message)
      (handle-runtime-commit model message)
      (handle-window-size model message)
      (handle-editor-action model message)
      (handle-editing-input model message)
      (handle-viewport-input model message)))

(defn view
  [{:keys [client-id viewport input editing? stale? error]}]
  (str "compiler-2 versioned " client-id "\n"
       "Up/Down select, Enter edit, Ctrl+T commit, Esc discard, Ctrl+C quit\n"
       (when stale? "STALE: refresh before committing\n")
       (when error (str "error: " error "\n"))
       "\n" (viewport/viewport-view viewport)
       (when editing? (str "\n\n" (text-input/text-input-view input)))))

(defn program-options
  [model]
  {:init (fn [] [model (poll-cmd (assoc model :poll-ms 0))])
   :update update-fn
   :view view
   :alt-screen true})

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
      (program/run (program-options model)))
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
