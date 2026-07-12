(ns graph.compiler-2-runtime-server
  "TCP EDN server/client for the shared compiler-2 runtime."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [graph.compiler-2-runtime :as runtime]
            [graph.compiler-2-runtime.file-loader :as file-loader]
            [graph.compiler-2-semantic-repl :as semantic-repl]
            [graph.xr-server :as xr-server]
            [propagators.cells.cell :as cell]
            [propagators.graph :as pgraph]
            [propagators.ids :as ids]
            [propagators.network :as net])
  (:import [java.io BufferedReader InputStreamReader OutputStreamWriter
            PushbackReader]
           [java.lang Character$UnicodeBlock]
           [java.net DatagramPacket DatagramSocket InetAddress
            ServerSocket Socket]
           [java.nio.charset StandardCharsets]))

(def default-host "127.0.0.1")
(def default-port 45555)
(def default-udp-port default-port)
(def default-load-blocks 1)
(def ^:private max-udp-packet-size 65507)
(def ^:private temperature-log-interval-ms 5000)
(def ^:dynamic *temperature-logger-enabled?* true)

(defn- write-edn-line
  [writer value]
  (.write writer (pr-str value))
  (.write writer "\n")
  (.flush writer))

(defn- transport-value [value]
  (walk/postwalk
   (fn [x]
     (cond
       (or (ids/node-id? x)
           (pgraph/node? x))
       (pr-str x)

       (net/network? x)
       {:runtime/network true
        :cells (count (:env x))
        :props (count (get-in x [:graph :propagators]))}

       (cell/cell? x)
       {:content (:content x)
        :strongest (:strongest x)}

       :else
       x))
   value))

(defn- read-edn-line
  [^BufferedReader reader]
  (when-let [line (.readLine reader)]
    (edn/read-string line)))

(defn- bytes->edn
  [^bytes data offset length]
  (edn/read-string
   (String. data offset length StandardCharsets/UTF_8)))

(defn- edn->bytes
  [value]
  (.getBytes (pr-str value) StandardCharsets/UTF_8))

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
  [{:keys [session xr-state xr-port xr-host xr-tls]}]
  (when (and xr-state (xr-effects-present? session))
    (locking xr-state
      (when-not (:server @xr-state)
        (let [xr (xr-server/start-server (or xr-port xr-server/default-port)
                                         session
                                         (or xr-host xr-server/default-host)
                                         xr-tls)]
          (reset! xr-state {:server xr})
          (println (str "XR runtime on " (:scheme xr) "://" (:host xr) ":"
                        (:port xr) "/")))))))

(defn- ensure-xr-server-soon!
  [server]
  (let [xr-state (:xr-state server)]
    (when (and xr-state
               (nil? (:server @xr-state))
               (not (:watching? @xr-state)))
      (swap! xr-state assoc :watching? true)
      (daemon-thread
       "compiler-2-runtime-xr-lazy-start"
       (fn []
         (loop [remaining 80]
           (when (and (pos? remaining)
                      (nil? (:server @xr-state)))
             (ensure-xr-server! server)
             (when (nil? (:server @xr-state))
               (Thread/sleep 25)
               (recur (dec remaining)))))
         (swap! xr-state dissoc :watching?))))))

(defn- load-file-response!
  [server {:keys [file client-id] :as command}]
  (when-not file
    (throw (ex-info "compile/load-file requires :file" {:command command})))
  (let [loaded (file-loader/load-file! (:session server)
                                       file
                                       (cond-> {}
                                         client-id
                                         (assoc :client-id client-id)))]
    {:ok true
     :result (dissoc loaded :session)}))

(defn- prepare-load-client!
  [server {:keys [load-client-id load-blocks]}]
  (when load-client-id
    (runtime/register-tui! (:session server) {:client-id load-client-id})
    (dotimes [_ (max 0 (long (or load-blocks default-load-blocks)))]
      (runtime/append-tui-block! (:session server)
                                 {:client-id load-client-id
                                  :rebuild? false}))))

(defn- load-startup-file!
  [server {:keys [load-file load-client-id] :as opts}]
  (when load-file
    (prepare-load-client! server opts)
    (let [loaded (file-loader/load-file! (:session server)
                                         load-file
                                         (cond-> {}
                                           load-client-id
                                           (assoc :client-id load-client-id)))]
      (dissoc loaded :session))))

(defn- handle-command-response!
  [server command]
  (case (:op command)
    :compile/load-file (load-file-response! server command)
    (runtime/handle-command! (:session server) command)))

(defn- handle-runtime-command!
  [server command]
  (let [response (handle-command-response! server command)]
    (try
      (ensure-xr-server! server)
      (ensure-xr-server-soon! server)
      response
      (catch Throwable t
        (swap! (:xr-state server) assoc :last-error (ex-message t))
        (runtime/record-runtime-error! (:session server)
                                       {:op (:op command)
                                        :phase :xr/start}
                                       t)
        response))))

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

(defn- handle-udp-packet
  [server ^DatagramSocket socket ^DatagramPacket packet]
  (let [response (try
                   (let [request (bytes->edn (.getData packet)
                                             (.getOffset packet)
                                             (.getLength packet))]
                     (transport-value
                      (handle-runtime-command! server request)))
                   (catch Throwable t
                     {:ok false
                      :error (ex-message t)
                      :data (ex-data t)}))
        response-bytes (edn->bytes response)
        response-packet (DatagramPacket. response-bytes
                                         (alength response-bytes)
                                         (.getAddress packet)
                                         (.getPort packet))]
    (.send socket response-packet)))

(defn- start-udp-server
  [server-state udp-port]
  (let [socket (DatagramSocket. (int udp-port)
                                (InetAddress/getByName default-host))
        running? (atom true)
        loop-thread
        (daemon-thread
         "compiler-2-runtime-udp"
         (fn []
           (while @running?
             (try
               (let [buffer (byte-array max-udp-packet-size)
                     packet (DatagramPacket. buffer (alength buffer))]
                 (.receive socket packet)
                 (handle-udp-packet server-state socket packet))
               (catch java.net.SocketException _ nil)))))]
    {:socket socket
     :port (.getLocalPort socket)
     :close (fn []
              (reset! running? false)
              (.close socket)
              (.interrupt ^Thread loop-thread))}))

(defn- round1
  [x]
  (when (number? x)
    (/ (Math/round (* 10.0 (double x))) 10.0)))

(def ^:private ansi-reset "\u001b[0m")
(def ^:private ansi-dim "\u001b[2m")
(def ^:private ansi-cyan "\u001b[36m")
(def ^:private ansi-green "\u001b[32m")
(def ^:private ansi-yellow "\u001b[33m")

(defn- color
  [ansi text]
  (str ansi text ansi-reset))

(defn- temperature-window-seconds
  []
  (round1 (/ temperature-log-interval-ms 1000.0)))

(defn- phase-label
  [phase]
  (case phase
    :commit/input "commit input / 入力コミット"
    :commit/total "commit total / コミット全体"
    :effects/boundary "boundary effects / 境界エフェクト"
    :effects/tui-write-block "TUI block write effect / TUI ブロック書き込みエフェクト"
    :effects/tui-write-display "TUI display write effect / TUI 表示書き込みエフェクト"
    :propagation/compile-topology "compile topology propagation / トポロジコンパイル伝播"
    :propagation/effect-only-compile "effect-only compile propagation / エフェクト専用コンパイル伝播"
    :propagation/program-updates "program update propagation / プログラム更新伝播"
    :propagation/publish-graph "graph publish propagation / グラフ公開伝播"
    :propagation/tui-append "TUI append propagation / TUI 追加伝播"
    :propagation/tui-edit "TUI edit propagation / TUI 編集伝播"
    :propagation/trace-install "trace install propagation / トレース導入伝播"
    :propagation/trace-tick "trace tick propagation / トレース更新伝播"
    (str phase " / 未分類フェーズ")))

(defn- english-label
  [phase]
  (first (str/split (phase-label phase) #" / ")))

(defn- japanese-label
  [phase]
  (second (str/split (phase-label phase) #" / ")))

(defn- stat-triple
  [stats prefix]
  (color ansi-yellow
         (str (round1 (get stats (keyword (name prefix) "avg")))
              "/"
              (round1 (get stats (keyword (name prefix) "p95")))
              "/"
              (round1 (get stats (keyword (name prefix) "max"))))))

(defn- phase-temperature-block
  [phase stats]
  (let [samples (color ansi-green (:samples stats))
        queue (stat-triple stats :queue)
        ms (stat-triple stats :ms)
        id (color ansi-dim phase)]
    (str (color ansi-cyan "[EN]") " " (english-label phase)
         " " id
         " samples=" samples
         " queue(avg/p95/max)=" queue
         " ms(avg/p95/max)=" ms
         "\n"
         (color ansi-cyan "[JP]") " " (japanese-label phase)
         " " id
         " サンプル=" samples
         " キュー(平均/p95/最大)=" queue
         " 時間ms(平均/p95/最大)=" ms)))

(defn- temperature-log-line
  [summary]
  (let [phases (:phases summary)]
    (when (seq phases)
      (str (color ansi-cyan "[runtime temperature]")
           " window=" (color ansi-green (str (temperature-window-seconds) "s"))
           " samples=" (color ansi-green (:sample-count summary)) "\n"
           (color ansi-cyan "[ランタイム温度]")
           " 集計=" (color ansi-green (str (temperature-window-seconds) "秒"))
           " サンプル=" (color ansi-green (:sample-count summary)) "\n"
           (str/join "\n"
                     (map (fn [[phase stats]]
                            (phase-temperature-block phase stats))
                          (sort-by (comp name key) phases)))))))

(defn- start-temperature-logger
  [server-state]
  (if-not *temperature-logger-enabled?*
    {:close (fn [] nil)}
    (let [running? (atom true)
          loop-thread
          (daemon-thread
           "compiler-2-runtime-temperature"
           (fn []
             (while @running?
               (try
                 (Thread/sleep temperature-log-interval-ms)
                 (when-let [line (temperature-log-line
                                  (runtime/drain-temperature!
                                   (:session server-state)))]
                   (println line))
                 (catch InterruptedException _ nil)
                 (catch Throwable t
                   (println (str "[runtime-temperature] error: "
                                 (ex-message t))))))))]
      {:close (fn []
                (reset! running? false)
                (.interrupt ^Thread loop-thread))})))

(defn start-server
  ([] (start-server default-port))
  ([port] (start-server port xr-server/default-port))
  ([port xr-port] (start-server port xr-port port))
  ([port xr-port udp-port] (start-server port xr-port udp-port xr-server/default-host))
  ([port xr-port udp-port xr-host] (start-server port xr-port udp-port xr-host nil))
  ([port xr-port udp-port xr-host xr-tls]
   (let [session (runtime/new-session)
         xr-state (atom nil)
         server-state {:session session
                       :xr-state xr-state
                       :xr-port xr-port
                       :xr-host xr-host
                       :xr-tls xr-tls}
         server (ServerSocket. port 50 (java.net.InetAddress/getByName default-host))
         udp (start-udp-server server-state udp-port)
         temperature (start-temperature-logger server-state)
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
      :xr-host xr-host
      :xr-tls xr-tls
      :port (.getLocalPort server)
      :udp udp
      :udp-port (:port udp)
      :close (fn []
               (reset! running? false)
               (when-let [xr (:server @xr-state)]
                 ((:close xr)))
               ((:close temperature))
               ((:close udp))
               (.close server)
               (.interrupt ^Thread accept-loop))})))

(defn start-server-with-xr
  ([] (start-server-with-xr default-port xr-server/default-port))
  ([port] (start-server-with-xr port xr-server/default-port))
  ([port xr-port] (start-server-with-xr port xr-port port))
  ([port xr-port udp-port] (start-server-with-xr port xr-port udp-port xr-server/default-host))
  ([port xr-port udp-port xr-host] (start-server-with-xr port xr-port udp-port xr-host nil))
  ([port xr-port udp-port xr-host xr-tls]
   (let [server (start-server port xr-port udp-port xr-host xr-tls)
         xr (xr-server/start-server xr-port (:session server) xr-host xr-tls)
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

(defn udp-request
  ([command] (udp-request default-host default-udp-port command))
  ([host port command]
   (with-open [socket (DatagramSocket.)]
     (.setSoTimeout socket 3000)
     (let [payload (edn->bytes command)
           address (InetAddress/getByName host)
           request-packet (DatagramPacket. payload
                                           (alength payload)
                                           address
                                           (int port))
           buffer (byte-array max-udp-packet-size)
           response-packet (DatagramPacket. buffer (alength buffer))]
       (.send socket request-packet)
       (.receive socket response-packet)
       (bytes->edn (.getData response-packet)
                   (.getOffset response-packet)
                   (.getLength response-packet))))))

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
               :xr-port xr-server/default-port
               :xr-host xr-server/default-host
               :xr-tls {}
               :dashboard? true
               :load-file nil
               :load-client-id nil
               :load-blocks default-load-blocks
               :udp-port default-udp-port}]
    (if-let [arg (first args)]
      (case arg
        ("--xr" "-xr")
        (recur (next args) (assoc opts :xr? true))

        ("--no-xr")
        (recur (next args) (assoc opts :xr? false))

        ("--dashboard")
        (recur (next args) (assoc opts :dashboard? true))

        ("--no-dashboard")
        (recur (next args) (assoc opts :dashboard? false))

        ("--xr-port" "-xr-port")
        (recur (nnext args)
               (assoc opts :xr-port (Long/parseLong (second args))))

        ("--xr-host" "-xr-host")
        (recur (nnext args)
               (assoc opts :xr-host (second args)))

        ("--xr-lan" "-xr-lan")
        (recur (next args)
               (assoc opts :xr-host xr-server/lan-host))

        ("--xr-https" "-xr-https")
        (recur (next args)
               (assoc-in opts [:xr-tls :https?] true))

        ("--xr-keystore" "-xr-keystore")
        (recur (nnext args)
               (assoc-in opts [:xr-tls :keystore] (second args)))

        ("--xr-keystore-password" "-xr-keystore-password")
        (recur (nnext args)
               (assoc-in opts [:xr-tls :keystore-password] (second args)))

        ("--xr-keystore-type" "-xr-keystore-type")
        (recur (nnext args)
               (assoc-in opts [:xr-tls :keystore-type] (second args)))

        ("--port" "-port")
        (recur (nnext args)
               (assoc opts :port (Long/parseLong (second args))))

        ("--udp-port" "-udp-port")
        (recur (nnext args)
               (assoc opts :udp-port (Long/parseLong (second args))))

        ("--load" "--load-file" "-load")
        (recur (nnext args)
               (assoc opts :load-file (second args)))

        ("--load-client" "--load-client-id")
        (recur (nnext args)
               (assoc opts :load-client-id (second args)))

        ("--load-blocks")
        (recur (nnext args)
               (assoc opts :load-blocks (Long/parseLong (second args))))

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
    (println (str "agent UDP API on " default-host ":" (:udp-port server)))
    (when-let [xr (:xr server)]
      (println (str "XR runtime on " (:scheme xr) "://" (:host xr) ":"
                    (:port xr) "/")))))

(defn- run-server-dashboard
  [server]
  ((requiring-resolve 'graph.compiler-2-runtime-dashboard/run-dashboard)
   server))

(defn -main
  [& args]
  (case (first args)
    "server"
    (let [{:keys [port xr? xr-port xr-host xr-tls udp-port dashboard?]
           :as opts}
          (parse-server-args (next args))
          server (binding [*temperature-logger-enabled?* (not dashboard?)]
                   (if xr?
                     (start-server-with-xr port xr-port udp-port xr-host xr-tls)
                     (start-server port xr-port udp-port xr-host xr-tls)))
          close-on-finally? (atom dashboard?)]
      (try
        (print-launch-banner server)
        (when-let [loaded (load-startup-file! server opts)]
          (println (str "loaded .lain file " (:file loaded)
                        " as client " (:client-id loaded))))
        (if dashboard?
          (run-server-dashboard server)
          (do
            (reset! close-on-finally? false)
            @(promise)))
        (catch Throwable t
          (reset! close-on-finally? false)
          ((:close server))
          (throw t))
        (finally
          (when @close-on-finally?
            ((:close server))))))

    "request"
    (let [[_ port source] args]
      (prn (request default-host (parse-port port) (edn/read-string source))))

    "udp-request"
    (let [[_ port source] args]
      (prn (udp-request default-host (parse-port port) (edn/read-string source))))

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

    (println "usage: server [port] [--load <file.lain>] [--load-client <client-id> --load-blocks <n>] [--xr] [--no-dashboard] [--xr-port <port>] [--xr-host <host>|--xr-lan] [--xr-https --xr-keystore <path> --xr-keystore-password <password>] [--udp-port <port>] | request <port> '<edn>' | udp-request <port> '<edn>' | graph <port> | trace <port> <label>")))
