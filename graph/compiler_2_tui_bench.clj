(ns graph.compiler-2-tui-bench
  "Small benchmark for wired TUI trace rebuild/poll/render paths."
  (:require [graph.compiler-2-runtime-server :as server]
            [graph.compiler-2-tui :as tui]))

(def simple-trace-commands
  [{:op :tui/register :client-id "A"}
   {:op :tui/append-block :client-id "A" :text "(def a)"}
   {:op :tui/append-block :client-id "A" :text "(<-> 2 a)"}
   {:op :tui/append-block :client-id "A" :text "(def out)"}
   {:op :tui/append-block :client-id "A" :text "(-> (+ 1 a) out)"}
   {:op :tui/append-block :client-id "A" :text "(def g)"}
   {:op :tui/append-block :client-id "A" :text "(trace out g)"}])

(defn- elapsed-ms
  [f]
  (let [t (System/nanoTime)]
    (f)
    (/ (double (- (System/nanoTime) t)) 1000000.0)))

(defn- samples
  [n f]
  (doall (repeatedly n #(elapsed-ms f))))

(defn- percentile
  [p xs]
  (nth (vec (sort xs))
       (dec (int (Math/ceil (* p (count xs)))))))

(defn- summary
  [xs]
  {:avg-ms (/ (reduce + xs) (count xs))
   :p95-ms (percentile 0.95 xs)})

(defn- request!
  [port command]
  (server/request server/default-host port command))

(defn- timed-request!
  [port command]
  (let [t (System/nanoTime)
        response (request! port command)]
    [(keyword (or (:text command) (name (:op command))))
     (/ (double (- (System/nanoTime) t)) 1000000.0)
     response]))

(defn- read-view
  [port]
  (:result (request! port {:op :tui/read-view :client-id "A"})))

(defn- bench
  []
  (let [{:keys [port close]} (server/start-server 0)]
    (try
      (let [steps (mapv #(timed-request! port %) simple-trace-commands)]
      (let [view (read-view port)
              model (tui/init-model {:client-id "A" :port port})
            state (first (tui/update-fn model
                                        {:type :runtime/view
                                         :response {:ok true
                                                    :result view}}))]
          {:append-steps (mapv (fn [[label elapsed response]]
                                 {:step label
                                  :ms elapsed
                                  :ok (:ok response)})
                               steps)
           :view {:blocks (count (:blocks view))
                  :bytes (count (.getBytes (pr-str view) "UTF-8"))}
         :socket-read-view (summary (samples 20 #(read-view port)))
         :unchanged-update (summary
                            (samples 20
                                     #(tui/update-fn
                                       state
                                       {:type :runtime/view
                                        :response {:ok true
                                                   :result view}})))
         :raw-render (summary
                      (samples 5 #(tui/render-view view
                                                   {:width 120
                                                      :height 30})))}))
      (finally
        (close)))))

(defn -main
  [& _args]
  (prn (bench)))
