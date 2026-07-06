(ns graph.compiler-2-runtime.tui-session
  "TUI session and block operations for compiler-2 runtime."
  (:require [clojure.string :as str]
            [graph.compiler-2-runtime.block-model :as block-model]
            [graph.compiler-2-runtime.input :as input]
            [graph.compiler-2-runtime.program :as program]
            [graph.compiler-2-runtime.state :as state]
            [graph.compiler-2-runtime.temperature :as temperature]
            [graph.compiler-2-runtime.tui-annotations :as annotations]
            [propagators.cells.value :as value]
            [propagators.core :as core]
            [propagators.ids :as ids]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.network-builder :as nb]))

(def stable-node-id state/stable-node-id)
(def ensure-session-state! state/ensure-session-state!)
(def require-state state/require-state)
(def valid-client-id? block-model/valid-client-id?)
(def block-by-index block-model/block-by-index)
(def update-block block-model/update-block)
(def assign-source-order block-model/assign-source-order)
(def block-text block-model/block-text)
(def read-source-forms program/read-source-forms)
(def install-block-slots program/install-block-slots)
(def install-instance-slots program/install-instance-slots)
(def rebuild-program! input/rebuild-program!)
(def install-block-incremental! input/install-block-incremental!)
(def seed-appended-block-topology! input/seed-appended-block-topology!)

(declare prepare-block-targets! read-tui-view)

(defn validate-client-id!
  [client-id]
  (when-not client-id
    (throw (ex-info "missing client-id" {})))
  (when-not (valid-client-id? client-id)
    (throw (ex-info "invalid client-id for compiler env binding"
                    {:client-id client-id}))))

(defn create-tui
  [client-id]
  (let [view-id (stable-node-id :tui client-id :view)
        instance-id (stable-node-id :tui client-id :instance)
        blocks-id (stable-node-id :tui client-id :blocks)]
    {:client-id client-id, :view-id view-id, :instance-id instance-id,
     :blocks-id blocks-id, :head-id nil, :tail-id nil, :next-index 0,
     :client-count 0, :blocks []}))

(defn install-tui-instance
  [network {:keys [view-id instance-id blocks-id]}]
  (-> network
      (nb/ensure-cell view-id)
      (nb/install-cell instance-id instance-id instance-id)
      (nb/ensure-cell blocks-id)
      (install-instance-slots {:instance-id instance-id
                               :blocks-id blocks-id})))

(defn ensure-tui!
  [session client-id]
  (validate-client-id! client-id)
  (let [state (ensure-session-state! session)]
    (or (get-in state [:tuis client-id])
        (let [tui (create-tui client-id)
              n (install-tui-instance (:network state) tui)]
          (swap! session #(-> %
                              (assoc :network n)
                              (assoc-in [:tuis client-id] tui)))
          tui))))

(defn register-tui!
  [session {:keys [client-id]}]
  (let [tui (ensure-tui! session client-id)]
    (swap! session update-in [:tuis client-id :client-count] (fnil inc 0))
    (let [tui (get-in @session [:tuis client-id])]
      {:client-id (:client-id tui), :view-id (pr-str (:view-id tui)),
       :next-index (:next-index tui)})))

(defn tui!
  [session client-id]
  (ensure-tui! session client-id)
  (get-in @session [:tuis client-id]))

(defn append-tui-block!
  [session {:keys [client-id text rebuild?] :or {rebuild? true} :as command}]
  (let [tui (tui! session client-id)
        index (:next-index tui)
        has-text? (contains? command :text)
        block {:block-id (ids/new-node-id), :index-id (ids/new-node-id),
               :text-id (ids/new-node-id), :display-id (ids/new-node-id),
               :next-id (ids/new-node-id), :index index, :epoch 0}
        block (cond-> block
                has-text? (assoc :text-current text
                                 :text-current-source? true))
        state @session
        previous-tail (peek (:blocks tui))
        n0 (-> (:network state)
               (nb/install-cell (:block-id block))
               (nb/install-cell (:index-id block) index index)
               ((if has-text?
                  #(nb/install-cell % (:text-id block) text text)
                  #(nb/install-cell % (:text-id block))))
               (nb/install-cell (:display-id block))
               (nb/install-cell (:next-id block)))
        n1 (install-block-slots n0 block)
        messages (cond-> []
                   (nil? (:head-id tui)) (conj (message (:view-id tui)
                                                        (:block-id block))
                                                   (message (:blocks-id tui)
                                                            (:block-id block)))
                   (:tail-id tui) (conj (message (:next-id (peek (:blocks tui)))
                                                 (:block-id block))))
        [tasks n2] (core/eval-cells messages n1)
        [state' n3] (temperature/run-tasks state
                                           :propagation/tui-append
                                           tasks
                                           n2)
        tui' (-> tui
                 (assoc :head-id (or (:head-id tui) (:block-id block))
                        :tail-id (:block-id block))
                 (update :next-index inc)
                 (update :blocks conj block))]
    (swap! session #(-> %
                        (merge (select-keys state' [:runtime]))
                        (assoc :network n3)
                        (assoc-in [:tuis client-id] tui')))
    (seed-appended-block-topology! session client-id
                                   (assoc block :client-id client-id)
                                   previous-tail)
    (when has-text?
      (prepare-block-targets! session client-id index text)
      (swap! session assign-source-order client-id index))
    (when rebuild?
      (if has-text?
        (let [block' (assoc (block-by-index @session client-id index)
                            :client-id client-id)]
          (install-block-incremental! session block'))
        (rebuild-program! session)))
    {:client-id client-id, :index index,
     :block-id (pr-str (:block-id block)),
     :text-id (pr-str (:text-id block)),
     :display-id (pr-str (:display-id block))}))

(defn next-view-block-index
  [view]
  (or (some->> (:blocks view)
               (map :index)
               seq
               (apply max)
               inc)
      0))

(defn input-view-block-index
  [view]
  (let [blocks (:blocks view)
        last-block (peek blocks)]
    (if (and last-block
             (= value/nothing (:value last-block))
             (not (:referenced? last-block)))
      (:index last-block)
      (next-view-block-index view))))

(defn block-targets
  [source]
  (try
    (let [forms (read-source-forms source)]
      (into []
            (keep (fn [x]
                    (when (seq? x)
                      (case (first x)
                        block (let [[_ index] x]
                                (when (integer? index)
                                  {:index index :strict-past? true}))
                        be:block (let [[_ index] x]
                                   (when (integer? index)
                                     {:index index :strict-past? true}))
                        block-at (let [[_ _ index] x]
                                   (when (integer? index)
                                     {:index index :strict-past? false}))
                        be:block-at (let [[_ _ index] x]
                                      (when (integer? index)
                                        {:index index :strict-past? false}))
                        nil))))
            (mapcat #(tree-seq coll? seq %) forms)))
    (catch Throwable _
      [])))

(defn block-target-indexes
  [source]
  (set (map :index (block-targets source))))

(defn empty-block? [state block]
  (let [v (block-text state block)]
    (or (value/nothing? v)
        (and (string? v) (str/blank? v)))))

(defn ensure-block-index! [session client-id index]
  (loop []
    (let [tui (tui! session client-id)]
      (when (<= (:next-index tui) index)
        (append-tui-block! session {:client-id client-id
                                    :rebuild? false})
        (recur)))))

(defn prepare-block-targets!
  [session client-id source-index source]
  (doseq [{:keys [index strict-past?]} (sort-by :index (block-targets source))]
    (let [state @session
          target-block (block-by-index state client-id index)]
      (cond
        (and target-block
             strict-past?
             (< index source-index)
             (not (empty-block? state target-block)))
        (throw (ex-info "block target points to a non-empty past block"
                        {:client-id client-id
                         :source-index source-index
                         :target-index index}))

        (nil? target-block)
        (ensure-block-index! session client-id index)))))

(defn edit-tui-block!
  [session {:keys [client-id index text]}]
  (let [tui (tui! session client-id)
        block0 (some #(when (= index (:index %)) %) (:blocks tui))]
    (when-not block0
      (throw (ex-info "block not found" {:client-id client-id :index index})))
    (prepare-block-targets! session client-id index text)
    (let [block block0
          new-epoch (inc (or (:epoch block) 0))
          _ (swap! session update-block client-id index
                   #(assoc % :epoch new-epoch
                             :text-current text
                             :text-current-source? true))
          [tasks n1] (core/eval-cells [(message (:text-id block) text)]
                                      (:network @session))
          [state' n2] (temperature/run-tasks @session
                                             :propagation/tui-edit
                                             tasks
                                             n1)]
      (swap! session #(-> %
                          (merge (select-keys state' [:runtime]))
                          (assoc :network n2)))
      (when-not (some? (:order block))
        (swap! session assign-source-order client-id index))
      (rebuild-program! session)
      {:client-id client-id, :index index})))

(defn submit-tui-block!
  [session {:keys [client-id text]}]
  (let [view (read-tui-view @session {:client-id client-id})
        current-index (input-view-block-index view)
        has-current? (= current-index (some-> (:blocks view) peek :index))]
    (when-not has-current?
      (append-tui-block! session {:client-id client-id :rebuild? false}))
    (append-tui-block! session {:client-id client-id :rebuild? false})
    (edit-tui-block! session {:client-id client-id
                              :index current-index
                              :text text})
    (let [view (read-tui-view @session {:client-id client-id})]
      (when-not (and (seq (:blocks view))
                     (= value/nothing (-> view :blocks peek :value))
                     (not (-> view :blocks peek :referenced?)))
        (append-tui-block! session {:client-id client-id :rebuild? false})))
    (read-tui-view @session {:client-id client-id})))

(defn block-value
  [state block]
  (block-text state block))

(defn block-display-value
  [state block]
  (net/network-cell-strongest (:network state) (:display-id block)))

(defn block-display-content
  [state block]
  (net/network-cell-content (:network state) (:display-id block)))

(defn block-view-value
  [state block]
  (let [display (when (nil? (:order block))
                  (block-display-value state block))]
    (if (and display (not (value/unusable? display)))
      display
      (block-value state block))))

(defn block-view-content
  [state block]
  (let [display (when (nil? (:order block))
                  (block-display-value state block))]
    (if (and display (not (value/unusable? display)))
      (block-display-content state block)
      (block-value state block))))

(defn referenced-block-indexes
  [state blocks]
  (reduce (fn [indexes block]
            (let [v (block-value state block)]
              (if (string? v)
                (into indexes (block-target-indexes v))
                indexes)))
          #{}
          blocks))

(defn project-tui-view
  [state {:keys [client-id]}]
  (let [tui (get-in (require-state state) [:tuis client-id])]
    (when-not tui
      (throw (ex-info "tui client not found" {:client-id client-id})))
    (let [referenced-indexes (referenced-block-indexes state (:blocks tui))]
      {:client-id client-id
       :view-id (pr-str (:view-id tui))
       :changed-cells (mapv pr-str (:runtime/changed-cells state))
       :changed-node-ids (mapv pr-str (:runtime/changed-node-ids state))
       :blocks (mapv (fn [block]
                       (let [raw-value (block-view-content state block)]
                         (annotations/annotate-block-view
                          {:index (:index block)
                           :block-id (pr-str (:block-id block))
                           :text-id (pr-str (:text-id block))
                           :display-id (pr-str (:display-id block))
                           :referenced? (contains? referenced-indexes
                                                   (:index block))
                           :value (annotations/project-value
                                   (block-view-value state block))}
                          raw-value)))
                     (:blocks tui))})))

(defn read-tui-view
  [state command]
  (project-tui-view state command))

(defn read-agent-blocks
  [state {:keys [client-id indexes]}]
  (let [view (project-tui-view state {:client-id client-id})
        wanted (when indexes (set indexes))]
    (cond-> view
      wanted (update :blocks #(vec (filter (fn [block]
                                             (contains? wanted (:index block)))
                                           %))))))

(defn read-agent-block
  [state {:keys [client-id index] :as command}]
  (when-not (integer? index)
    (throw (ex-info "agent block read expects integer index"
                    {:command command})))
  (let [blocks (:blocks (read-agent-blocks state {:client-id client-id
                                                  :indexes [index]}))]
    (or (first blocks)
        (throw (ex-info "block not found" {:client-id client-id
                                           :index index})))))

(defn send-agent-block!
  [session {:keys [client-id text mode] :or {mode :submit} :as command}]
  (when-not (string? text)
    (throw (ex-info "agent block send expects text"
                    {:command command})))
  (tui! session client-id)
  (case mode
    :append (append-tui-block! session {:client-id client-id
                                        :text text})
    "append" (append-tui-block! session {:client-id client-id
                                         :text text})
    :submit (submit-tui-block! session {:client-id client-id
                                        :text text})
    "submit" (submit-tui-block! session {:client-id client-id
                                         :text text})
    (throw (ex-info "unknown agent block send mode"
                    {:mode mode
                     :command command}))))

(defn unregister-tui!
  [session {:keys [client-id]}]
  (let [remaining (atom nil)]
    (swap! session
           (fn [state]
             (let [count (get-in state [:tuis client-id :client-count] 0)
                   next-count (max 0 (dec count))]
               (reset! remaining next-count)
               (if (pos? next-count)
                 (assoc-in state [:tuis client-id :client-count] next-count)
                 (update state :tuis dissoc client-id)))))
    {:client-id client-id, :remaining-clients @remaining,
     :unregistered true}))
