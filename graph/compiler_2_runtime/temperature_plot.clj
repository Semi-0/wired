(ns graph.compiler-2-runtime.temperature-plot
  "Terminal-native temperature plots."
  (:require [charm.components.table :as table]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def spark-chars [\▁ \▂ \▃ \▄ \▅ \▆ \▇ \█])

(def rate-categories
  [{:id :commit
    :label "commit/ms"
    :phase? #(= "commit" (namespace %))}
   {:id :propagation
    :label "propagation/ms"
    :phase? #(= "propagation" (namespace %))}
   {:id :effects
    :label "effects/ms"
    :phase? #(= "effects" (namespace %))}
   {:id :xr
    :label "xr runtime/ms"
    :phase? #(= "xr" (namespace %))}])

(declare sparkline)

(def gnuplot-candidates
  ["/opt/homebrew/bin/gnuplot"
   "/usr/local/bin/gnuplot"
   "/usr/bin/gnuplot"])

(defn finite-number
  [x]
  (when (and (number? x)
             (not (Double/isNaN (double x)))
             (not (Double/isInfinite (double x))))
    (double x)))

(defn round1
  [x]
  (when-let [x (finite-number x)]
    (/ (Math/round (* 10.0 x)) 10.0)))

(defn round4
  [x]
  (when-let [x (finite-number x)]
    (/ (Math/round (* 10000.0 x)) 10000.0)))

(defn- sample-category
  [sample]
  (some (fn [{:keys [id phase?]}]
          (when (phase? (:phase sample)) id))
        rate-categories))

(defn- bucket-index
  [started-at window-ms sample]
  (long (quot (max 0 (- (long (:at sample)) started-at))
              window-ms)))

(defn rate-buckets
  ([samples] (rate-buckets samples 10000))
  ([samples window-ms]
   (let [samples (vec (filter #(and (:at %) (:phase %)) samples))
         started-at (if (seq samples)
                      (apply min (map :at samples))
                      0)
         buckets (reduce
                  (fn [acc sample]
                    (if-let [category (sample-category sample)]
                      (update-in acc
                                 [(bucket-index started-at window-ms sample)
                                  category]
                                 (fnil inc 0))
                      acc))
                  {}
                  samples)
         last-bucket (if (seq buckets) (apply max (keys buckets)) 0)]
     (mapv
      (fn [bucket]
        (let [counts (get buckets bucket {})]
          (merge {:second (round1 (/ (* bucket window-ms) 1000.0))}
                 (into {}
                       (map (fn [{:keys [id]}]
                              [id (round4 (/ (double (get counts id 0))
                                             (double window-ms)))]))
                       rate-categories))))
      (range (inc last-bucket))))))

(defn rate-plot-data
  [samples window-ms]
  (->> (rate-buckets samples window-ms)
       (map (fn [row]
              (str/join " "
                        (cons (:second row)
                              (map (fn [{:keys [id]}]
                                     (get row id 0.0))
                                   rate-categories)))))
       (str/join "\n")))

(defn rate-row
  [samples window-ms]
  (let [counts (reduce
                (fn [acc sample]
                  (if-let [category (sample-category sample)]
                    (update acc category (fnil inc 0))
                    acc))
                {}
                samples)]
    (into {:second 0.0}
          (map (fn [{:keys [id]}]
                 [id (round4 (/ (double (get counts id 0))
                                (double (max 1 window-ms))))]))
          rate-categories)))

(defn rate-rows-plot-data
  [rows]
  (->> rows
       (map-indexed
        (fn [idx row]
          (str/join " "
                    (cons
                     (or (:second row) idx)
                     (map (fn [{:keys [id]}]
                            (get row id 0.0))
                          rate-categories)))))
       (str/join "\n")))

(defn- plot-rows
  [{:keys [rows samples window-ms]
    :or {window-ms 10000}}]
  (cond
    (seq rows) (vec rows)
    (seq samples) (rate-buckets samples window-ms)
    :else []))

(defn- rate-series
  [rows id]
  (mapv #(get % id 0.0) rows))

(defn text-rate-plot
  [{:keys [samples width] :as opts
    :or {width 80}}]
  (let [rows (plot-rows opts)
        trend-width (max 12 (min 48 (- (long width) 32)))]
    (if (seq rows)
      (str/join
       "\n"
       (map (fn [{:keys [id label]}]
              (let [values (rate-series rows id)]
                (format "%-16s latest %-8s max %-8s %s"
                        label
                        (str (round4 (peek values)))
                        (str (round4 (apply max values)))
                        (sparkline values trend-width))))
            rate-categories))
      (str "runtime rate samples pending\n"
           (str/join "\n"
                     (map (fn [{:keys [label]}]
                            (format "%-16s latest 0.0      max 0.0" label))
                          rate-categories))))))

(defn rate-plot-script
  [{:keys [samples window-ms width height rows]
    :or {window-ms 10000
         width 80
         height 18}}]
  (let [data (if rows
               (rate-rows-plot-data rows)
               (rate-plot-data samples window-ms))
        plot-lines (->> rate-categories
                        (map-indexed
                         (fn [idx {:keys [label]}]
                           (format "$runtime using 1:%d with lines title '%s'"
                                   (+ 2 idx)
                                   label)))
                        (str/join ", \\\n     "))]
    (str "set terminal dumb " width " " height "\n"
         "set title 'runtime temperature rates'\n"
         "set xlabel 'seconds'\n"
         "set ylabel 'events/ms'\n"
         "set key outside\n"
         "$runtime << EOD\n"
         data "\n"
         "EOD\n"
         "plot " plot-lines "\n")))

(defn- executable-file?
  [path]
  (let [file (io/file path)]
    (and (.exists file)
         (.isFile file)
         (.canExecute file))))

(defn- path-commands
  [command]
  (map #(str % java.io.File/separator command)
       (str/split (or (System/getenv "PATH") "") #":")))

(defn gnuplot-command
  []
  (some #(when (executable-file? %) %)
        (concat (path-commands "gnuplot")
                gnuplot-candidates)))

(defn gnuplot-rate-plot
  [opts]
  (if (seq (plot-rows opts))
    (if-let [gnuplot (gnuplot-command)]
      (let [{:keys [exit out err]} (shell/sh gnuplot
                                             :in (rate-plot-script opts))]
        (if (zero? exit)
          (str "gnuplot: " gnuplot "\n"
               (str/trim-newline out))
          (str "gnuplot failed: " (str/trim err) "\n"
               (text-rate-plot opts))))
      (str "gnuplot not found; using text runtime rate plot\n"
           (text-rate-plot opts)))
    (text-rate-plot opts)))

(defn sparkline
  ([xs] (sparkline xs 24))
  ([xs width]
   (let [values (vec (take-last width (keep finite-number xs)))]
    (if (empty? values)
      ""
      (let [lo (apply min values)
            hi (apply max values)
            span (max 1.0 (- hi lo))
            top (dec (count spark-chars))]
        (apply str
               (map (fn [x]
                      (nth spark-chars
                           (long (Math/round
                                  (* top (/ (- x lo) span))))))
                    values)))))))

(defn big-sparkline
  ([xs] (big-sparkline xs 3))
  ([xs height]
   (let [values (vec (keep finite-number xs))]
     (if (empty? values)
       ""
       (let [lo (apply min values)
             hi (apply max values)
             span (max 1.0 (- hi lo))
             levels (mapv (fn [x]
                            (long (Math/round
                                   (* (dec height)
                                      (/ (- x lo) span)))))
                          values)]
         (->> (range (dec height) -1 -1)
              (map (fn [row]
                     (apply str
                            (map #(if (>= % row) \█ \space)
                                 levels))))
              (str/join "\n")))))))

(defn number-line
  [label xs]
  (let [values (vec (keep finite-number xs))]
    (if (empty? values)
      (str label " none")
      (format "%-5s latest %-7s min %-7s max %-7s"
              label
              (str (round1 (peek values)))
              (str (round1 (apply min values)))
              (str (round1 (apply max values)))))))

(defn trend-line
  [label xs]
  (let [line (sparkline xs 24)]
    (if (str/blank? line)
      (str label " trend none")
      (str label " trend low " line " high"))))

(defn- samples-per-second
  [stats window-ms]
  (round1 (* 1000.0
             (/ (double (:samples stats 0))
                (double (max 1 window-ms))))))

(defn latest-row
  [phase-label window-ms phase rows]
  (let [latest (or (peek (vec rows)) {})]
    [(phase-label phase)
     (str (samples-per-second latest window-ms))
     (str (round1 (:queue/p95 latest 0)))
     (str (round1 (:queue/max latest 0)))
     (str (round1 (:ms/p95 latest 0)))
     (str (round1 (:ms/max latest 0)))]))

(defn phase-table
  ([history phase-label] (phase-table history phase-label 1000))
  ([history phase-label window-ms]
  (table/table-view
   (table/table
    [{:title "phase" :width 34}
     {:title "samples/s" :width 10}
     {:title "q p95" :width 8}
     {:title "q max" :width 8}
     {:title "ms p95" :width 8}
     {:title "ms max" :width 8}]
    (mapv (fn [[phase rows]]
            (latest-row phase-label window-ms phase rows))
          (sort-by (comp name key) history))
    :cursor nil)
   {:separator "  "})))

(defn phase-panel
  [{:keys [label queue ms]}]
  (str label "\n"
       (number-line "queue" queue) "\n"
       (trend-line "queue" queue) "\n"
       (number-line "ms" ms) "\n"
       (trend-line "ms" ms)))

(defn history->panels
  ([history phase-label] (history->panels history phase-label 1000))
  ([history phase-label window-ms]
  (str (phase-table history phase-label window-ms)
       "\n\nrecent load trends\n"
       (->> history
            (sort-by (comp name key))
            (map (fn [[phase rows]]
                   (str (phase-label phase) "\n"
                        "sample/s " (sparkline (map #(samples-per-second % window-ms) rows) 32) "\n"
                        "queue " (sparkline (map :queue/p95 rows) 32) "\n"
                        "ms    " (sparkline (map :ms/p95 rows) 32))))
            (str/join "\n\n")))))
