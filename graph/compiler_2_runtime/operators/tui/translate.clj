(ns graph.compiler-2-runtime.operators.tui.translate
  "Small translation demo compiler-2 operator."
  (:require [propagators.compiler-2.helpers :as compiler-helpers]
            [propagators.message :refer [message]]
            [propagators.network :as net]
            [propagators.propagator :as prop]))

(def ^:private jp->en
  {"猫" "cat"
   "犬" "dog"
   "水" "water"})

(def ^:private en->jp
  (into {} (map (fn [[jp en]] [en jp]) jp->en)))

(defn translate-messages
  [network jp-id en-id]
  (let [jp (net/network-cell-strongest network jp-id)
        en (net/network-cell-strongest network en-id)
        en' (when (string? jp) (get jp->en jp))
        jp' (when (string? en) (get en->jp en))]
    (cond-> []
      en' (conj (message en-id en'))
      jp' (conj (message jp-id jp')))))

(defn translate-operator []
  (with-meta
    (fn [network arg-ids out-id]
      (let [[jp-id en-id] (vec arg-ids)]
        (when-not (and jp-id en-id (= 2 (count arg-ids)))
          (throw (ex-info "translate expects Japanese and English cells"
                          {:arg-ids arg-ids})))
        (let [[prop-id n]
              ((prop/construct-propagator
                (prop/concrete-propagator
                 (fn [_inputs _outputs current-net]
                   (translate-messages current-net jp-id en-id)))
                [jp-id en-id]
                [jp-id en-id])
               network)]
          [n [prop-id] (or out-id en-id)])))
    {compiler-helpers/output-selector-key
     (fn [arg-ids fallback-id]
       (or (second (vec arg-ids)) fallback-id))
     compiler-helpers/application-activate-key
     (fn [current-net _context-id arg-ids _out-id]
       (let [[jp-id en-id] (vec arg-ids)]
         (when-not (and jp-id en-id (= 2 (count arg-ids)))
           (throw (ex-info "translate expects Japanese and English cells"
                           {:arg-ids arg-ids})))
         (translate-messages current-net jp-id en-id)))}))
