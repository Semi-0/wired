(ns graph.compiler-2-runtime-json-server
  "Local line-delimited JSON transport for compiler-2 instance debugging."
  (:require [graph.json :as json])
  (:import [java.io BufferedReader InputStreamReader OutputStreamWriter]
           [java.net InetAddress ServerSocket Socket]
           [java.nio.charset StandardCharsets]))

(def default-host "127.0.0.1")
(def default-port 45556)

(defn daemon-thread
  [name f]
  (doto (Thread. ^Runnable f name)
    (.setDaemon true)
    (.start)))

(defn normalize-command
  [command]
  (update command :op #(if (string? %) (keyword %) %)))

(defn write-json-line
  [writer value]
  (.write writer (json/write-json value))
  (.write writer "\n")
  (.flush writer))

(defn error-response
  [throwable]
  {:ok false
   :error (ex-message throwable)
   :data (ex-data throwable)})

(defn handle-client
  [handler ^Socket socket]
  (daemon-thread
   "compiler-2-runtime-json-client"
   (fn []
     (with-open [socket socket
                 reader (BufferedReader.
                         (InputStreamReader. (.getInputStream socket)
                                             StandardCharsets/UTF_8))
                 writer (OutputStreamWriter. (.getOutputStream socket)
                                             StandardCharsets/UTF_8)]
       (loop []
         (when-let [line (.readLine reader)]
           (write-json-line
            writer
            (try
              (handler (normalize-command (json/read-json line)))
              (catch Throwable t
                (error-response t))))
           (recur)))))))

(defn start-server
  ([handler]
   (start-server default-port handler))
  ([port handler]
   (let [socket (ServerSocket. (int port) 50
                               (InetAddress/getByName default-host))
         running? (atom true)
         accept-thread
         (daemon-thread
          "compiler-2-runtime-json-accept"
          (fn []
            (while @running?
              (try
                (handle-client handler (.accept socket))
                (catch java.net.SocketException _ nil)))))]
     {:server socket
      :port (.getLocalPort socket)
      :close (fn []
               (reset! running? false)
               (.close socket)
               (.interrupt ^Thread accept-thread))})))

(defn request
  ([command]
   (request default-host default-port command))
  ([host port command]
   (with-open [socket (Socket. host (int port))
               writer (OutputStreamWriter. (.getOutputStream socket)
                                           StandardCharsets/UTF_8)
               reader (BufferedReader.
                       (InputStreamReader. (.getInputStream socket)
                                           StandardCharsets/UTF_8))]
     (.setSoTimeout socket 5000)
     (write-json-line writer command)
     (if-let [line (.readLine reader)]
       (json/read-json line)
       (throw (ex-info "JSON runtime closed without a response"
                       {:host host :port port}))))))
