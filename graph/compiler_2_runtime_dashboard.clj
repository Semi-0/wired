(ns graph.compiler-2-runtime-dashboard
  "Local Charm dashboard for a running compiler-2 runtime server."
  (:require [charm.components.viewport :as viewport]
            [charm.message :as msg]
            [charm.program :as program]
            [clojure.string :as str]
            [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-runtime.temperature-plot :as plot]))

(def views [:temperature :clients :xr])

(def tracked-phases
  [:commit/input
   :commit/total
   :effects/boundary
   :effects/tui-write-block
   :effects/tui-write-display
   :propagation/compile-topology
   :propagation/effect-only-compile
   :propagation/program-updates
   :propagation/publish-graph
   :propagation/tui-append
   :propagation/tui-edit
   :propagation/trace-install
   :propagation/trace-tick])

(def zero-stats
  {:samples 0
   :queue/avg 0
   :queue/p95 0
   :queue/max 0
   :ms/avg 0
   :ms/p95 0
   :ms/max 0})

(def rate-window-ms 1000)

(defn- cap-history
  [rows]
  (let [rows (vec rows)]
    (if (> (count rows) 48)
      (subvec rows (- (count rows) 48))
      rows)))

(defn- next-view
  [view]
  (nth views (mod (inc (.indexOf views view)) (count views))))

(defn- previous-view
  [view]
  (nth views (mod (dec (.indexOf views view)) (count views))))

(defn- phase-label
  [phase]
  (case phase
    :commit/input "commit input"
    :commit/total "commit total"
    :effects/boundary "boundary effects"
    :effects/tui-write-block "TUI block write effect"
    :effects/tui-write-display "TUI display write effect"
    :propagation/compile-topology "compile topology propagation"
    :propagation/effect-only-compile "effect-only compile propagation"
    :propagation/program-updates "program update propagation"
    :propagation/publish-graph "graph publish propagation"
    :propagation/tui-append "TUI append propagation"
    :propagation/tui-edit "TUI edit propagation"
    :propagation/trace-install "trace install propagation"
    :propagation/trace-tick "trace tick propagation"
    (name phase)))

(defn- append-history
  [history summary]
  (let [phase-set (set (concat tracked-phases
                               (keys history)
                               (keys (:phases summary))))]
    (reduce
     (fn [acc phase]
       (update acc
               phase
               (fn [rows]
                 (cap-history
                  (conj (vec rows)
                        (get-in summary [:phases phase]
                                zero-stats))))))
     history
     (sort-by name phase-set))))

(defn- append-rate-history
  [history samples]
  (let [history (vec history)
        previous-second (:second (peek history))
        next-second (if (number? previous-second)
                      (inc previous-second)
                      0)]
    (cap-history
     (conj history
           (assoc (plot/rate-row samples rate-window-ms)
                  :second next-second)))))

(defn- temperature-content
  [{:keys [history rate-history window-size]}]
  (str "temperature\n"
       "phase is the runtime work category; samples/s is activity in the last refresh\n\n"
       "runtime rates\n"
       (plot/gnuplot-rate-plot
       {:rows rate-history
         :window-ms rate-window-ms
         :width (max 60 (- (long (or (:width window-size) 80)) 4))
         :height 16})
       "\n\nphase details\n"
       (plot/phase-table history phase-label rate-window-ms)))

(defn- block-preview
  [view]
  (->> (:blocks view)
       (take-last 8)
       (map (fn [{:keys [index value]}]
              (str "[" index "] " value)))
       (str/join "\n")))

(defn- clients-content
  [{:keys [state]}]
  (let [clients (sort (keys (:tuis state)))]
    (if (seq clients)
      (str "TUI clients\n\n"
           (str/join
            "\n\n"
            (map (fn [client-id]
                   (str client-id "\n"
                        (block-preview
                         (runtime/read-tui-view state
                                                {:client-id client-id}))))
                 clients)))
      "TUI clients\n\nnone")))

(defn- xr-content
  [{:keys [state]}]
  (let [widgets (get-in state [:xr :widgets])
        effects (get-in state [:xr :effects])]
    (str "XR\n\n"
         "widgets: " (count widgets) "\n"
         (str/join "\n"
                   (map (fn [[id widget]]
                          (str "- " id " channels="
                               (str/join ", "
                                         (keys (:channels widget)))))
                        (sort-by key widgets)))
         "\n\n"
         "effects: " (count effects))))

(defn- current-content
  [model]
  (case (:view model)
    :clients (clients-content model)
    :xr (xr-content model)
    (temperature-content model)))

(defn- viewport-keys
  []
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
  (viewport/viewport "" :height 20 :keys (viewport-keys)))

(defn- viewport-height
  [model]
  (max 1 (- (long (or (get-in model [:window-size :height]) 24)) 5)))

(defn- viewport-width
  [model]
  (long (or (get-in model [:window-size :width]) 80)))

(defn- configure-viewport
  [model]
  (let [content (current-content model)
        vp (-> (:viewport model)
               (viewport/viewport-set-content content)
               (viewport/viewport-set-dimensions (viewport-width model)
                                                 (viewport-height model)))]
    (assoc model :viewport vp)))

(defn- sample
  [session]
  (locking session
    (let [state (runtime/ensure-session-state! session)
          samples (get-in state [:runtime :temperature :samples])
          summary (runtime/summarize-temperature samples)
          state' (assoc-in state [:runtime :temperature :samples] [])]
      (reset! session state')
      {:type :dashboard/sample
       :state state'
       :summary summary
       :samples samples})))

(defn- refresh-cmd
  [session poll-ms]
  (program/cmd
   (fn []
     (Thread/sleep (long poll-ms))
     (sample session))))

(defn init-model
  [server]
  {:session (:session server)
   :poll-ms 1000
   :view :temperature
   :state @(:session server)
   :summary {:sample-count 0 :phases {}}
   :rate-history []
   :history {}
   :viewport (new-viewport)
   :window-size nil})

(defn update-fn
  [model message]
  (cond
    (or (msg/key-match? message "q")
        (msg/key-match? message "ctrl+c"))
    [model program/quit-cmd]

    (or (msg/key-match? message "tab")
        (msg/key-match? message "right"))
    [(configure-viewport (update model :view next-view)) nil]

    (msg/key-match? message "left")
    [(configure-viewport (update model :view previous-view)) nil]

    (msg/window-size? message)
    [(configure-viewport
      (assoc model :window-size (select-keys message [:width :height])))
     nil]

    (= :dashboard/sample (:type message))
    (let [model' (-> model
                     (assoc :state (:state message)
                            :summary (:summary message))
                     (update :history append-history (:summary message))
                     (update :rate-history append-rate-history
                             (:samples message))
                     configure-viewport)]
      [model' (refresh-cmd (:session model') (:poll-ms model'))])

    :else
    (let [[vp vp-cmd] (viewport/viewport-update (:viewport model) message)]
      [(assoc model :viewport vp) vp-cmd])))

(defn view
  [model]
  (str "compiler-2 server dashboard | tab/right next, left previous, arrows scroll, q quits\n"
       "view: " (name (:view model)) "\n\n"
       (viewport/viewport-view (:viewport model))))

(defn run-dashboard
  [server]
  (let [model (configure-viewport (init-model server))]
    (program/run {:init (fn [] [model (refresh-cmd (:session model) 0)])
                  :update update-fn
                  :view view
                  :alt-screen false})))
