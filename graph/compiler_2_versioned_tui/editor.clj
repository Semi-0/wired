(ns graph.compiler-2-versioned-tui.editor
  "Pure state transitions for the premise-versioned block editor."
  (:require [charm.message :as msg]))

(defn selected-block
  [model]
  (get (:blocks (:view model)) (:selected model 0)))

(defn clamp-selection
  [model]
  (let [last-index (max 0 (dec (count (get-in model [:view :blocks]))))]
    (update model :selected #(min last-index (max 0 (long (or % 0)))))))

(defn select
  [model delta]
  (if (:editing? model)
    model
    (clamp-selection (update model :selected (fnil + 0) delta))))

(defn begin-edit
  [model commit-id]
  (if-let [block (selected-block model)]
    (assoc model
           :editing? true
           :draft (or (:source block) "")
           :draft-commit-id commit-id
           :expected-version (:version block)
           :attempted? false
           :stale? false)
    model))

(defn cancel-edit
  [model]
  (dissoc model :editing? :draft :draft-commit-id :expected-version
          :attempted? :stale?))

(defn edit-draft
  [model text new-commit-id]
  (cond-> (assoc model :draft text)
    (:attempted? model)
    (assoc :draft-commit-id new-commit-id :attempted? false)))

(defn commit-request
  [model]
  (when (and (:editing? model) (not (:stale? model)))
    {:op :tui/commit-version
     :commit-id (:draft-commit-id model)
     :client-id (:client-id model)
     :index (:index (selected-block model))
     :expected-version (:expected-version model)
     :text (:draft model)}))

(defn mark-attempted
  [model]
  (assoc model :attempted? true))

(defn accept-commit
  [model]
  (cancel-edit model))

(defn apply-poll
  [model view]
  (let [remote-version (some-> view :blocks (get (:selected model 0)) :version)
        stale? (and (:editing? model)
                    (not= remote-version (:expected-version model)))]
    (cond-> (assoc model :view view)
      (:editing? model) (assoc :stale? stale?)
      true clamp-selection)))

(defn open-trailing-blank
  [model commit-id]
  (let [last-index (max 0 (dec (count (get-in model [:view :blocks]))))]
    (begin-edit (assoc model :selected last-index) commit-id)))

(defn action
  [message]
  (cond
    (msg/key-match? message "ctrl+c") :quit
    (msg/key-match? message "ctrl+up") :select-up
    (msg/key-match? message "ctrl+down") :select-down
    (msg/key-match? message "ctrl+t") :commit
    (msg/key-match? message :esc) :cancel
    (msg/key-match? message " ") :edit
    :else nil))
