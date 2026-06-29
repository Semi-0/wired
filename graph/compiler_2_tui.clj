(ns graph.compiler-2-tui
  "Charm TUI client for linked-list runtime blocks."
  (:require [clojure.string :as str]
            [charm.components.viewport :as viewport]
            [charm.components.text-input :as text-input]
            [charm.message :as msg]
            [charm.program :as program]
            [graph.compiler-2-runtime-server :as server]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [graph.vijual :as v]))

(defn graph-value?
  [x]
  (and (map? x)
       (map? (:nodes x))
       (vector? (:edges x))))

(defn- graph-render-opts
  [{:keys [width height]}]
  (cond-> semantic-repl/stress-opts
    (pos? (long (or width 0)))
    ;; ponytail: approximate node+route width; replace with measured fit if Vijual grows sizing support.
    (assoc :stress-grid-width (max 1 (quot (long width) 14)))

    (pos? (long (or height 0)))
    (assoc :stress-grid-height (max 1 (quot (long height) 7)))))

(defn render-value
  ([value] (render-value value nil))
  ([value viewport-size]
   (if (graph-value? value)
     (try
       (with-out-str
         (v/draw-stress-directed-graph (:edges value)
                                       (:nodes value)
                                       (graph-render-opts viewport-size)))
       (catch Throwable t
         (str "graph render error: " (ex-message t))))
     (str value))))

(defn render-view
  ([view] (render-view view nil))
  ([{:keys [blocks]} viewport-size]
   (str/join
    "\n\n"
    (map (fn [{:keys [index value]}]
           (str "[" index "]\n" (render-value value viewport-size)))
         blocks))))

(def ^:private viewport-keys
  {:line-up ["up"]
   :line-down ["down"]
   :half-page-up ["ctrl+u"]
   :half-page-down ["ctrl+d"]
   :page-up ["pgup"]
   :page-down ["pgdown"]
   :top ["home"]
   :bottom ["end"]})

(defn- new-viewport
  []
  (viewport/viewport "" :height 20 :keys viewport-keys))

(defn- viewport-height
  [{:keys [window-size error]}]
  (if-let [height (:height window-size)]
    (max 1 (- (long height)
              5
              (if error 2 0)))
    20))

(defn- viewport-width
  [{:keys [window-size]}]
  (or (:width window-size) 0))

(defn- viewport-size
  [state]
  {:width (viewport-width state)
   :height (viewport-height state)})

(defn- configure-viewport
  [state content]
  (let [vp0 (:viewport state)
        vp1 (if (= content (viewport/viewport-content vp0))
              vp0
              (viewport/viewport-set-content vp0 content))
        vp2 (viewport/viewport-set-dimensions vp1
                                               (viewport-width state)
                                               (viewport-height state))]
    (assoc state :viewport vp2)))

(defn poll-view
  [host port client-id]
  (server/request host port {:op :tui/read-view :client-id client-id}))

(defn init-model
  [{:keys [host port client-id poll-ms]
    :or {host server/default-host
         port server/default-port
         client-id "tui-1"
         poll-ms 1000}}]
  {:host host
   :port port
   :client-id client-id
   :poll-ms poll-ms
   :input (text-input/text-input :prompt "> ")
   :viewport (new-viewport)
   :window-size nil
   :view nil
   :error nil})

(defn refresh-cmd
  [{:keys [host port client-id poll-ms]}]
  (program/cmd
   (fn []
     (Thread/sleep (long (or poll-ms 0)))
     {:type :runtime/view
      :response (poll-view host port client-id)})))

(defn append-cmd
  [{:keys [host port client-id input]}]
  (program/cmd
   (fn []
     {:type :runtime/view
      :response (do
                  (server/request host port {:op :tui/append-block
                                             :client-id client-id
                                             :text (text-input/value input)})
                  (poll-view host port client-id))})))

(defn update-fn
  [state message]
  (cond
    (or (msg/key-match? message "q")
        (msg/key-match? message "ctrl+c"))
    [state program/quit-cmd]

    (= :runtime/view (:type message))
    (let [response (:response message)]
      (if (:ok response)
        (let [state' (assoc state :view (:result response) :error nil)
              content (render-view (:result response) (viewport-size state'))]
          [(configure-viewport state' content) (refresh-cmd state')])
        [(configure-viewport (assoc state :error (:error response))
                             (or (some-> (:view state)
                                         (render-view (viewport-size state)))
                                 ""))
         (refresh-cmd state)]))

    (msg/key-match? message :enter)
    (if (str/blank? (text-input/value (:input state)))
      [state nil]
      [(update state :input text-input/reset) (append-cmd state)])

    (msg/window-size? message)
    (let [state' (assoc state :window-size (select-keys message [:width :height]))]
      [(configure-viewport state'
                           (or (some-> (:view state')
                                       (render-view (viewport-size state')))
                               ""))
       nil])

    :else
    (let [[vp vp-cmd] (viewport/viewport-update (:viewport state) message)]
      (if (not= vp (:viewport state))
        [(assoc state :viewport vp) vp-cmd]
        (let [[input cmd] (text-input/text-input-update (:input state) message)]
          [(assoc state :input input) cmd])))))

(defn view
  [{:keys [client-id input viewport error]}]
  (str "compiler-2 " client-id "\n"
       "enter appends, arrows/page/home/end scroll, q quits\n\n"
       (when error (str "error: " error "\n\n"))
       (viewport/viewport-view viewport)
       "\n\n" (text-input/text-input-view input)))

(defn run-client
  [{:keys [host port client-id poll-ms]
    :or {host server/default-host
         port server/default-port
         client-id "tui-1"
         poll-ms 1000}}]
  (server/request host port {:op :tui/register :client-id client-id})
  (try
    (let [model (init-model {:host host
                             :port port
                             :client-id client-id
                             :poll-ms poll-ms})]
      (program/run {:init (fn [] [model (refresh-cmd (assoc model :poll-ms 0))])
                    :update update-fn
                    :view view
                    :alt-screen true}))
    (finally
      (server/request host port {:op :tui/unregister :client-id client-id}))))

(defn -main
  [& args]
  (let [[port client-id] args]
    (run-client {:port (if port (Long/parseLong port) server/default-port)
                 :client-id (or client-id "tui-1")})))
