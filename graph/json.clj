(ns graph.json
  "Small JSON codec for the XR prototype.

  It intentionally covers ordinary JSON data only: maps, arrays, strings,
  numbers, booleans, and null."
  (:require [clojure.string :as str]))

(declare read-value)

(defn- escape-string
  [s]
  (str/escape s {\" "\\\""
                 \\ "\\\\"
                 \backspace "\\b"
                 \formfeed "\\f"
                 \newline "\\n"
                 \return "\\r"
                 \tab "\\t"}))

(defn write-json
  [x]
  (cond
    (nil? x) "null"
    (true? x) "true"
    (false? x) "false"
    (string? x) (str "\"" (escape-string x) "\"")
    (keyword? x) (write-json (subs (str x) 1))
    (symbol? x) (write-json (str x))
    (number? x) (str x)
    (map? x) (str "{"
                  (str/join
                   ","
                   (map (fn [[k v]]
                          (str (write-json (if (keyword? k)
                                             (subs (str k) 1)
                                             (str k)))
                               ":"
                               (write-json v)))
                        x))
                  "}")
    (sequential? x) (str "["
                         (str/join "," (map write-json x))
                         "]")
    (set? x) (write-json (vec x))
    :else (write-json (pr-str x))))

(defn- error
  [s idx message]
  (throw (ex-info message {:index idx :near (subs s idx (min (count s)
                                                             (+ idx 20)))})))

(defn- skip-ws
  [s idx]
  (loop [i idx]
    (if (and (< i (count s))
             (Character/isWhitespace ^char (.charAt s i)))
      (recur (inc i))
      i)))

(defn- expect-char
  [s idx ch]
  (let [idx (skip-ws s idx)]
    (if (and (< idx (count s)) (= ch (.charAt s idx)))
      (inc idx)
      (error s idx (str "expected " ch)))))

(defn- read-literal
  [s idx literal value]
  (if (.startsWith s literal idx)
    [value (+ idx (count literal))]
    (error s idx (str "expected " literal))))

(defn- read-string*
  [s idx]
  (let [idx (expect-char s idx \")]
    (loop [i idx
           out (StringBuilder.)]
      (when (>= i (count s))
        (error s i "unterminated string"))
      (let [ch (.charAt s i)]
        (cond
          (= ch \") [(str out) (inc i)]
          (= ch \\) (let [j (inc i)]
                      (when (>= j (count s))
                        (error s j "unterminated escape"))
                      (let [esc (.charAt s j)]
                        (case esc
                          \" (do (.append out \") (recur (+ i 2) out))
                          \\ (do (.append out \\) (recur (+ i 2) out))
                          \/ (do (.append out \/) (recur (+ i 2) out))
                          \b (do (.append out \backspace) (recur (+ i 2) out))
                          \f (do (.append out \formfeed) (recur (+ i 2) out))
                          \n (do (.append out \newline) (recur (+ i 2) out))
                          \r (do (.append out \return) (recur (+ i 2) out))
                          \t (do (.append out \tab) (recur (+ i 2) out))
                          (error s j "unsupported string escape"))))
          :else (do (.append out ch)
                    (recur (inc i) out)))))))

(defn- number-char?
  [ch]
  (or (Character/isDigit ^char ch)
      (contains? #{\- \+ \. \e \E} ch)))

(defn- read-number
  [s idx]
  (let [end (loop [i idx]
              (if (and (< i (count s))
                       (number-char? (.charAt s i)))
                (recur (inc i))
                i))
        token (subs s idx end)]
    [(if (or (str/includes? token ".")
             (str/includes? token "e")
             (str/includes? token "E"))
       (Double/parseDouble token)
       (Long/parseLong token))
     end]))

(defn- read-array
  [s idx]
  (let [idx (expect-char s idx \[)]
    (loop [i (skip-ws s idx)
           out []]
      (if (and (< i (count s)) (= \] (.charAt s i)))
        [out (inc i)]
        (let [[v j] (read-value s i)
              j (skip-ws s j)]
          (cond
            (and (< j (count s)) (= \, (.charAt s j)))
            (recur (skip-ws s (inc j)) (conj out v))

            (and (< j (count s)) (= \] (.charAt s j)))
            [(conj out v) (inc j)]

            :else (error s j "expected , or ]")))))))

(defn- key->keyword
  [k]
  (keyword k))

(defn- read-object
  [s idx]
  (let [idx (expect-char s idx \{)]
    (loop [i (skip-ws s idx)
           out {}]
      (if (and (< i (count s)) (= \} (.charAt s i)))
        [out (inc i)]
        (let [[k j] (read-string* s i)
              j (expect-char s j \:)
              [v j] (read-value s j)
              j (skip-ws s j)
              out (assoc out (key->keyword k) v)]
          (cond
            (and (< j (count s)) (= \, (.charAt s j)))
            (recur (skip-ws s (inc j)) out)

            (and (< j (count s)) (= \} (.charAt s j)))
            [out (inc j)]

            :else (error s j "expected , or }")))))))

(defn read-value
  [s idx]
  (let [idx (skip-ws s idx)]
    (when (>= idx (count s))
      (error s idx "expected JSON value"))
    (let [ch (.charAt s idx)]
      (cond
        (= ch \") (read-string* s idx)
        (= ch \{) (read-object s idx)
        (= ch \[) (read-array s idx)
        (= ch \t) (read-literal s idx "true" true)
        (= ch \f) (read-literal s idx "false" false)
        (= ch \n) (read-literal s idx "null" nil)
        (or (= ch \-) (Character/isDigit ^char ch)) (read-number s idx)
        :else (error s idx "expected JSON value")))))

(defn read-json
  [s]
  (let [[v idx] (read-value s 0)
        idx (skip-ws s idx)]
    (when-not (= idx (count s))
      (error s idx "unexpected trailing data"))
    v))
