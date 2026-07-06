(ns graph.xr-server
  "HTTP/WebSocket server for the XR runtime projection."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [graph.json :as json]
            [graph.xr-runtime :as xr]
            [graph.compiler-2-runtime :as runtime])
  (:import [java.io BufferedInputStream BufferedOutputStream ByteArrayOutputStream]
           [java.net ServerSocket Socket URLDecoder]
           [java.nio ByteBuffer]
           [java.security MessageDigest]
           [java.util Base64]
           [java.util.concurrent Executors TimeUnit]))

(def default-host "127.0.0.1")
(def default-port 45666)
(def websocket-guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defn- daemon-thread
  [name f]
  (let [thread (Thread. ^Runnable f name)]
    (.setDaemon thread true)
    (.start thread)
    thread))

(defn- daemon-executor
  [name]
  (Executors/newSingleThreadScheduledExecutor
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r]
       (doto (Thread. ^Runnable r name)
         (.setDaemon true))))))

(defn- read-byte
  [^BufferedInputStream in]
  (.read in))

(defn- read-line*
  [^BufferedInputStream in]
  (let [out (ByteArrayOutputStream.)]
    (loop [prev -1]
      (let [b (read-byte in)]
        (cond
          (= b -1) (when (pos? (.size out))
                     (.toString out "UTF-8"))
          (and (= prev 13) (= b 10))
          (let [bytes (.toByteArray out)
                len (max 0 (- (alength bytes) 1))]
            (String. bytes 0 len "UTF-8"))
          :else
          (do (.write out b)
              (recur b)))))))

(defn- parse-request
  [^BufferedInputStream in]
  (when-let [request-line (read-line* in)]
    (let [[method path protocol] (str/split request-line #" " 3)
          headers (loop [m {}]
                    (let [line (read-line* in)]
                      (if (str/blank? line)
                        m
                        (let [[k v] (str/split line #":" 2)]
                          (recur (assoc m
                                        (str/lower-case k)
                                        (str/trim (or v ""))))))))]
      {:method method
       :path path
       :protocol protocol
       :headers headers})))

(defn- write-response
  [^BufferedOutputStream out status headers body]
  (let [body-bytes (if (bytes? body)
                     body
                     (.getBytes (str body) "UTF-8"))
        headers (merge {"Content-Length" (str (alength body-bytes))
                        "Connection" "close"}
                       headers)]
    (.write out (.getBytes (str "HTTP/1.1 " status "\r\n") "UTF-8"))
    (doseq [[k v] headers]
      (.write out (.getBytes (str k ": " v "\r\n") "UTF-8")))
    (.write out (.getBytes "\r\n" "UTF-8"))
    (.write out body-bytes)
    (.flush out)))

(defn- content-type
  [path]
  (let [path (if (or (= path "/") (= path "/xr"))
               "/index.html"
               (first (str/split path #"\?")))]
    (cond
      (str/ends-with? path ".html") "text/html; charset=utf-8"
      (str/ends-with? path ".js") "text/javascript; charset=utf-8"
      (str/ends-with? path ".css") "text/css; charset=utf-8"
      :else "application/octet-stream")))

(defn- safe-static-path
  [path]
  (let [path (first (str/split path #"\?"))
        path (URLDecoder/decode path "UTF-8")
        path (if (or (= path "/") (= path "/xr")) "/index.html" path)
        path (str/replace-first path #"^/" "")]
    (when-not (str/includes? path "..")
      (str "graph/xr_static/" path))))

(defn- static-bytes
  [path]
  (when-let [resource (some-> path safe-static-path io/resource)]
    (with-open [in (io/input-stream resource)
                out (ByteArrayOutputStream.)]
      (io/copy in out)
      (.toByteArray out))))

(defn- accept-key
  [client-key]
  (let [sha1 (MessageDigest/getInstance "SHA-1")
        bytes (.digest sha1 (.getBytes (str client-key websocket-guid) "UTF-8"))]
    (.encodeToString (Base64/getEncoder) bytes)))

(defn- write-websocket-handshake
  [^BufferedOutputStream out key]
  (.write out (.getBytes
               (str "HTTP/1.1 101 Switching Protocols\r\n"
                    "Upgrade: websocket\r\n"
                    "Connection: Upgrade\r\n"
                    "Sec-WebSocket-Accept: " (accept-key key) "\r\n"
                    "\r\n")
               "UTF-8"))
  (.flush out))

(defn- read-exact
  [^BufferedInputStream in n]
  (let [buf (byte-array n)]
    (loop [offset 0]
      (if (= offset n)
        buf
        (let [read (.read in buf offset (- n offset))]
          (when (= read -1)
            (throw (ex-info "websocket closed" {})))
          (recur (+ offset read)))))))

(defn- unsigned-byte
  [b]
  (bit-and b 0xff))

(defn- signed-byte
  [b]
  (unchecked-byte b))

(defn- read-length
  [^BufferedInputStream in len-code]
  (cond
    (< len-code 126) len-code
    (= len-code 126) (let [b (read-exact in 2)]
                       (bit-or (bit-shift-left (unsigned-byte (aget b 0)) 8)
                               (unsigned-byte (aget b 1))))
    :else (let [b (read-exact in 8)
                bb (ByteBuffer/wrap b)]
            (.getLong bb))))

(defn- read-frame
  [^BufferedInputStream in]
  (let [b0 (read-byte in)
        b1 (read-byte in)]
    (when (or (= b0 -1) (= b1 -1))
      (throw (ex-info "websocket closed" {})))
    (let [opcode (bit-and b0 0x0f)
          masked? (pos? (bit-and b1 0x80))
          len-code (bit-and b1 0x7f)
          len (read-length in len-code)
          mask-key (when masked? (read-exact in 4))
          payload (read-exact in (int len))]
      (when masked?
        (dotimes [i len]
          (aset-byte payload
                     i
                     (signed-byte
                      (bit-xor (unsigned-byte (aget payload i))
                               (unsigned-byte (aget mask-key (mod i 4))))))))
      {:opcode opcode
       :text (String. payload "UTF-8")})))

(defn- write-frame
  [^BufferedOutputStream out text]
  (let [payload (.getBytes text "UTF-8")
        len (alength payload)]
    (.write out 0x81)
    (cond
      (< len 126)
      (.write out len)

      (< len 65536)
      (do (.write out 126)
          (.write out (bit-and (bit-shift-right len 8) 0xff))
          (.write out (bit-and len 0xff)))

      :else
      (do (.write out 127)
          (.write out (.array (doto (ByteBuffer/allocate 8)
                                (.putLong len))))))
    (.write out payload)
    (.flush out)))

(defn- normalize-op
  [command]
  (update command :op (fn [op]
                        (cond
                          (keyword? op) op
                          (and (string? op) (str/starts-with? op ":"))
                          (keyword (subs op 1))
                          (string? op) (keyword op)
                          :else op))))

(defn- response
  [type payload]
  (json/write-json (merge {:type type} payload)))

(defn- handle-json-command
  [session command]
  (try
    {:ok true
     :result (xr/handle-command! session (normalize-op command))}
    (catch Throwable t
      {:ok false
       :error (ex-message t)
       :data (ex-data t)})))

(defn- start-trace-push!
  [session out {:keys [trace-id interval-ms]}]
  (let [interval-ms (long (or interval-ms 1000))
        executor (daemon-executor (str "xr-trace-" trace-id))]
    (.scheduleAtFixedRate
     executor
     (fn []
       (try
         (let [result (xr/handle-command! session {:op :xr/trace/read
                                                   :trace-id trace-id})]
           (locking out
             (write-frame out (response "xr/trace/update" {:ok true
                                                           :result result}))))
         (catch Throwable _ nil)))
     interval-ms
     interval-ms
     TimeUnit/MILLISECONDS)
    #(.shutdownNow executor)))

(defn- launch-effect-graph
  [effect]
  (when (and (= :xr (:boundary/port effect))
             (= :xr/launch-trace (:boundary/kind effect)))
    (get-in effect [:boundary/payload :graph])))

(defn- latest-effect-payload
  [session]
  (let [result (xr/handle-command! session {:op :xr/effects})]
    (when-let [graph (some launch-effect-graph (reverse (:effects result)))]
      {:graph (assoc (xr/graph->json
                      graph
                      {:changed-node-ids (:changed-node-ids result)
                       :changed-cell-ids (:changed-cells result)})
                     :widgets (vals (:widgets result)))})))

(defn- start-effect-push!
  [session out]
  (let [interval-ms 500
        last-payload (atom nil)
        executor (daemon-executor "xr-effects")]
    (try
      (when-let [payload (latest-effect-payload session)]
        (reset! last-payload payload)
        (locking out
          (write-frame out
                       (response "xr/effects/update"
                                 {:ok true
                                  :result payload}))))
      (catch Throwable _ nil))
    (.scheduleAtFixedRate
     executor
     (fn []
       (try
         (when-let [payload (latest-effect-payload session)]
           (when-not (= payload @last-payload)
             (reset! last-payload payload)
             (locking out
               (write-frame out
                            (response "xr/effects/update"
                                      {:ok true
                                       :result payload})))))
         (catch Throwable _ nil)))
     interval-ms
     interval-ms
     TimeUnit/MILLISECONDS)
    #(.shutdownNow executor)))

(defn- websocket-loop
  [session ^BufferedInputStream in ^BufferedOutputStream out]
  (loop [stops [(start-effect-push! session out)]]
    (let [{:keys [opcode text]} (read-frame in)]
      (case opcode
        8 (doseq [stop stops] (stop))
        1 (let [command (json/read-json text)
                result (handle-json-command session command)
                stop (when (and (:ok result)
                                (= (:op (normalize-op command))
                                   :xr/trace/install))
                       (start-trace-push! session
                                          out
                                          (select-keys (:result result)
                                                       [:trace-id :interval-ms])))]
            (locking out
              (write-frame out (response "xr/response" result)))
            (recur (cond-> stops stop (conj stop))))
        9 (do (locking out (write-frame out text))
              (recur stops))
        (recur stops)))))

(defn- websocket-request?
  [{:keys [headers]}]
  (= "websocket" (some-> (get headers "upgrade") str/lower-case)))

(defn- handle-socket
  [session ^Socket socket]
  (daemon-thread
   "xr-client"
   (fn []
     (with-open [s socket
                 in (BufferedInputStream. (.getInputStream s))
                 out (BufferedOutputStream. (.getOutputStream s))]
       (let [request (parse-request in)]
         (cond
           (nil? request) nil

           (and (= (:path request) "/ws")
                (websocket-request? request))
           (do (write-websocket-handshake out
                                          (get-in request [:headers
                                                           "sec-websocket-key"]))
               (websocket-loop session in out))

           (= "GET" (:method request))
           (if-let [body (static-bytes (:path request))]
             (write-response out
                             "200 OK"
                             {"Content-Type" (content-type (:path request))}
                             body)
             (write-response out
                             "404 Not Found"
                             {"Content-Type" "text/plain; charset=utf-8"}
                             "not found"))

           :else
           (write-response out
                           "405 Method Not Allowed"
                           {"Content-Type" "text/plain; charset=utf-8"}
                           "method not allowed")))))))

(defn start-server
  ([] (start-server default-port))
  ([port] (start-server port (runtime/new-session)))
  ([port session]
   (let [session (or session (runtime/new-session))
         server (ServerSocket. port 50 (java.net.InetAddress/getByName default-host))
         running? (atom true)
         accept-thread
         (daemon-thread
          "xr-accept"
          (fn []
            (while @running?
              (try
                (handle-socket session (.accept server))
                (catch java.net.SocketException _ nil)))))]
     {:server server
      :session session
      :port (.getLocalPort server)
      :close (fn []
               (reset! running? false)
               (.close server)
               (.interrupt ^Thread accept-thread))})))

(defn -main
  [& args]
  (let [port (if-let [p (first args)]
               (Long/parseLong p)
               default-port)
        server (start-server port)]
    (println (str "XR runtime on http://" default-host ":" (:port server) "/"))
    @(promise)))
