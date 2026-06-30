(ns graph.compiler-2-runtime-server
  "TCP EDN server/client for the shared compiler-2 runtime."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-semantic-repl :as semantic-repl]
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

(defn- handle-client
  [session ^Socket socket]
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
                              (runtime/handle-command! session request))))
           (recur)))))))

(defn start-server
  ([] (start-server default-port))
  ([port]
   (let [session (runtime/new-session)
         server (ServerSocket. port 50 (java.net.InetAddress/getByName default-host))
         running? (atom true)
         accept-loop
         (daemon-thread
          "compiler-2-runtime-accept"
          (fn []
            (while @running?
              (try
                (handle-client session (.accept server))
                (catch java.net.SocketException _ nil)))))]
     {:server server
      :session session
      :port (.getLocalPort server)
      :close (fn []
               (reset! running? false)
               (.close server)
               (.interrupt ^Thread accept-loop))})))

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
  [port]
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
    (println (str "lain-lang server on " default-host ":" port))))

(defn -main
  [& args]
  (case (first args)
    "server"
    (let [port (parse-port (second args))
          server (start-server port)]
      (print-launch-banner (:port server))
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

    (println "usage: server [port] | request <port> '<edn>' | graph <port> | trace <port> <label>")))
