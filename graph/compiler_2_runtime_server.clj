(ns graph.compiler-2-runtime-server
  "TCP EDN server/client for the shared compiler-2 runtime."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [graph.xr-server :as xr-server]
            [propagators.ids :as ids])
  (:import [java.io BufferedReader InputStreamReader OutputStreamWriter
            PushbackReader]
           [java.lang Character$UnicodeBlock]
           [java.net ServerSocket Socket]))

(def default-host "127.0.0.1")
(def default-port 45555)

(defn- write-edn-line
  [writer value]
  (.write writer (pr-str value))
  (.write writer "\n")
  (.flush writer))

(defn- transport-value [value]
  (walk/postwalk
   (fn [x]
     (if (ids/node-id? x)
       (pr-str x)
       x))
   value))

(defn- read-edn-line
  [^BufferedReader reader]
  (when-let [line (.readLine reader)]
    (edn/read-string line)))

(defn- daemon-thread
  [name f]
  (let [thread (Thread. ^Runnable f name)]
    (.setDaemon thread true)
    (.start thread)
    thread))

(defn- xr-launch-effect?
  [effect]
  (and (= :xr (:boundary/port effect))
       (= :xr/launch-trace (:boundary/kind effect))))

(defn- xr-effects-present?
  [session]
  (some xr-launch-effect? (get-in @session [:xr :effects])))

(defn- ensure-xr-server!
  [{:keys [session xr-state xr-port]}]
  (when (and xr-state (xr-effects-present? session))
    (locking xr-state
      (when-not (:server @xr-state)
        (let [xr (xr-server/start-server (or xr-port xr-server/default-port)
                                         session)]
          (reset! xr-state {:server xr})
          (println (str "XR runtime on http://" xr-server/default-host ":"
                        (:port xr) "/")))))))

(defn- handle-runtime-command!
  [server command]
  (let [response (runtime/handle-command! (:session server) command)]
    (ensure-xr-server! server)
    response))

(defn- handle-client
  [server ^Socket socket]
  (daemon-thread
   "compiler-2-runtime-client"
   (fn []
     (with-open [s socket
                 reader (BufferedReader. (InputStreamReader. (.getInputStream s)))
                 writer (OutputStreamWriter. (.getOutputStream s))]
       (loop []
         (when-let [request (try
                              (read-edn-line reader)
                              (catch Throwable t
                                {:op :runtime/bad-edn
                                 :error (ex-message t)}))]
           (write-edn-line writer
                           (if (= :runtime/bad-edn (:op request))
                             {:ok false :error (:error request)}
                             (transport-value
                              (handle-runtime-command! server request))))
           (recur)))))))

(defn start-server
  ([] (start-server default-port))
  ([port] (start-server port xr-server/default-port))
  ([port xr-port]
   (let [session (runtime/new-session)
         xr-state (atom nil)
         server-state {:session session
                       :xr-state xr-state
                       :xr-port xr-port}
         server (ServerSocket. port 50 (java.net.InetAddress/getByName default-host))
         running? (atom true)
         accept-loop
         (daemon-thread
          "compiler-2-runtime-accept"
          (fn []
            (while @running?
              (try
                (handle-client server-state (.accept server))
                (catch java.net.SocketException _ nil)))))]
     {:server server
      :session session
      :xr-state xr-state
      :xr-port xr-port
      :port (.getLocalPort server)
      :close (fn []
               (reset! running? false)
               (when-let [xr (:server @xr-state)]
                 ((:close xr)))
               (.close server)
               (.interrupt ^Thread accept-loop))})))

(defn start-server-with-xr
  ([] (start-server-with-xr default-port xr-server/default-port))
  ([port] (start-server-with-xr port xr-server/default-port))
  ([port xr-port]
   (let [server (start-server port xr-port)
         xr (xr-server/start-server xr-port (:session server))
         close-server (:close server)
         xr-state (:xr-state server)]
     (reset! xr-state {:server xr})
     (assoc server
            :xr xr
            :close close-server))))

(defn request
  ([command] (request default-host default-port command))
  ([host port command]
   (with-open [socket (Socket. host port)
               writer (OutputStreamWriter. (.getOutputStream socket))
               reader (PushbackReader. (InputStreamReader. (.getInputStream socket)))]
     (write-edn-line writer command)
     (edn/read reader))))

(defn- parse-port
  [s]
  (if (str/blank? s)
    default-port
    (Long/parseLong s)))

(defn- numeric-string?
  [s]
  (boolean (and s (re-matches #"\d+" s))))

(defn- parse-server-args
  [args]
  (loop [args args
         opts {:port default-port
               :xr? false
               :xr-port xr-server/default-port}]
    (if-let [arg (first args)]
      (case arg
        ("--xr" "-xr")
        (recur (next args) (assoc opts :xr? true))

        ("--no-xr")
        (recur (next args) (assoc opts :xr? false))

        ("--xr-port" "-xr-port")
        (recur (nnext args)
               (assoc opts :xr-port (Long/parseLong (second args))))

        ("--port" "-port")
        (recur (nnext args)
               (assoc opts :port (Long/parseLong (second args))))

        (if (numeric-string? arg)
          (recur (next args) (assoc opts :port (Long/parseLong arg)))
          (throw (ex-info "unknown server argument" {:arg arg
                                                     :args args}))))
      opts)))

(defn- terminal-width
  []
  (try
    (Long/parseLong (or (System/getenv "COLUMNS") "80"))
    (catch Throwable _
      80)))

(defn- separator
  [width]
  (apply str (repeat (max 1 width) \=)))

(defn- wide-char?
  [^Character c]
  (let [block (Character$UnicodeBlock/of c)]
    (contains? #{Character$UnicodeBlock/CJK_UNIFIED_IDEOGRAPHS
                 Character$UnicodeBlock/CJK_SYMBOLS_AND_PUNCTUATION
                 Character$UnicodeBlock/HIRAGANA
                 Character$UnicodeBlock/KATAKANA
                 Character$UnicodeBlock/HALFWIDTH_AND_FULLWIDTH_FORMS}
               block)))

(defn- display-width
  [text]
  (reduce + (map #(if (wide-char? %) 2 1) text)))

(defn- center-text
  [text width]
  (let [pad (max 0 (quot (- width (display-width text)) 2))]
    (str (apply str (repeat pad \space)) text)))

(defn- print-launch-banner
  [server]
  (let [width (terminal-width)]
    (println (separator width))
    (println)
    (println)
    (println (center-text "lain-lang 0.2 / μ kernel" width))
    (println)
    (println (center-text "どこにでもいるということは、 どこにもいないということだ。" width))
    (println (center-text "神の果実は私たちの中にある。" width))
    (println (center-text "存在の終わりは、 存在の始まりにすでに書かれている。" width))
    (println)
    (println)
    (println (separator width))
    (println (str "lain-lang server on " default-host ":" (:port server)))
    (when-let [xr (:xr server)]
      (println (str "XR runtime on http://" xr-server/default-host ":"
                    (:port xr) "/")))))

(defn -main
  [& args]
  (case (first args)
    "server"
    (let [{:keys [port xr? xr-port]} (parse-server-args (next args))
          server (if xr?
                   (start-server-with-xr port xr-port)
                   (start-server port))]
      (print-launch-banner server)
      @(promise))

    "request"
    (let [[_ port source] args]
      (prn (request default-host (parse-port port) (edn/read-string source))))

    "graph"
    (let [[_ port] args
          response (request default-host (parse-port port) {:op :semantic/graph})]
      (if (:ok response)
        (semantic-repl/print-graph (:result response))
        (prn response)))

    "trace"
    (let [[_ port label] args
          response (request default-host
                            (parse-port port)
                            {:op :semantic/trace
                             :label label
                             :direction :upstream})]
      (if (:ok response)
        (semantic-repl/print-graph (:result response))
        (prn response)))

    (println "usage: server [port] [--xr] [--xr-port <port>] | request <port> '<edn>' | graph <port> | trace <port> <label>")))
