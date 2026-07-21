(ns propagators.compiler-2.runtime.environment-io-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [graph.compiler-2-versioned-tui.editor :as editor]
            [propagators.compiler-2.runtime :as runtime]
            [propagators.compiler-2.runtime.boundary.effects :as effects]
            [propagators.compiler-2.runtime.session.environment-io :as environment-io]
            [propagators.compiler-2.runtime.session.state :as runtime-state])
  (:import [java.nio.file Files]
           [java.util UUID]))

(def fixture-module
  (.getCanonicalPath
   (io/file "test/propagators/compiler_2/runtime/fixtures/project_math.clj")))

(def fixture-entry
  :propagators.compiler-2.runtime.fixtures.project-math/primitive-bindings)

(defn temp-file
  [suffix]
  (.toFile (Files/createTempFile "compiler-2-environment-" suffix
                                 (make-array java.nio.file.attribute.FileAttribute 0))))

(defn commit!
  [session client-id index text]
  (runtime/commit-version!
   session
   {:commit-id (str (UUID/randomUUID))
    :client-id client-id
    :index index
    :expected-version nil
    :text text}))

(deftest primitive-module-extends-the-live-environment
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "live" :mode :versioned-premise})
    (commit! session "live" 0
             (pr-str (list 'load-primitive-environment
                           fixture-module fixture-entry 0)))
    (commit! session "live" 1 "(square 5)")
    (is (= 25 (get-in (runtime/read-tui-view @session {:client-id "live"})
                      [:blocks 2 :value])))
    (is (= 1 (count (:environment/primitive-imports @session))))))

(deftest lain-loads-one-form-at-a-time
  (let [file (temp-file ".lain")
        _ (spit file (str "(load-primitive-environment "
                          (pr-str fixture-module) " " fixture-entry " 0)\n"
                          "(def answer (square 6))\n"
                          "(+ answer 1)\n"))
        request {:boundary/kind :environment/load-lain
                 :boundary/payload {:file (.getPath file) :revision 0}}
        result (environment-io/perform-request
                (runtime-state/empty-state) request
                effects/drain-environment-effects)]
    (is (= :loaded (get-in result [:receipt :status])))
    (is (= 3 (get-in result [:receipt :installed-form-count])))
    (is (= 37 (get-in result [:state :program/results
                              [(str "file-"
                                    (subs (environment-io/file-digest file) 0 12))
                               2]
                              :result])))))

(deftest block-source-save-is-reloadable-s-expression-text
  (let [session (runtime/new-session)
        file (temp-file ".lain")]
    (runtime/register-tui! session {:client-id "save" :mode :versioned-premise})
    (commit! session "save" 0 "(def x 4)")
    (commit! session "save" 1 "  (+ x 3) ; keep spacing")
    (let [{next-state :state receipt :receipt}
          (environment-io/save-state
           @session {:file (.getPath file)
                     :mode :source
                     :selection :all
                     :checkpoint-id "save-1"
                     :client-id "save"})]
      (is (= :saved (:status receipt)))
      (is (= ['(def x 4) '(+ x 3)]
             (environment-io/lain-forms (slurp file))))
      (is (= "(def x 4)\n\n  (+ x 3) ; keep spacing\n"
             (slurp file)))
      (is (:replayed?
           (:receipt
            (environment-io/save-state
             next-state {:file (.getPath file)
                         :mode :source
                         :selection :all
                         :checkpoint-id "save-1"
                         :client-id "save"})))))))

(deftest focus-is-ephemeral-per-client-and-queued-while-editing
  (let [session (runtime/new-session)]
    (runtime/register-tui! session {:client-id "focus" :mode :versioned-premise})
    (commit! session "focus" 0 "1")
    (let [focus (runtime/focus-block! session {:client-id "focus"
                                               :index 1
                                               :request-id "r-1"})
          view (runtime/read-tui-view @session {:client-id "focus"})
          editing (-> (editor/apply-poll {:client-id "focus"
                                          :view view
                                          :selected 0
                                          :focus-seq 0}
                                         (dissoc view :focus))
                      (editor/begin-edit "commit"))
          queued (editor/apply-poll editing view)
          applied (editor/cancel-edit queued)]
      (is (= 1 (:index focus)))
      (is (= focus (:pending-focus queued)))
      (is (= 1 (:selected applied)))
      (is (= (:sequence focus) (:focus-seq applied))))))

(deftest nested-lain-cycles-stop-with-a-failed-prefix-receipt
  (let [a (temp-file ".lain")
        b (temp-file ".lain")]
    (spit a (pr-str (list 'load-lain (.getCanonicalPath b) 0)))
    (spit b (pr-str (list 'load-lain (.getCanonicalPath a) 0)))
    (let [result
          (environment-io/perform-request
           (runtime-state/empty-state)
           {:boundary/kind :environment/load-lain
            :boundary/payload {:file (.getPath a) :revision 0}}
           effects/drain-environment-effects)]
      (is (= :failed (get-in result [:receipt :status])))
      (is (re-find #"nested environment effect failed"
                   (pr-str (get-in result [:receipt :diagnostics])))))))

(deftest load-blocks-appends-one-versioned-block-per-form
  (let [session (runtime/new-session)
        file (temp-file ".lain")]
    (spit file "(def a 2)\n(+ a 3)\n")
    (runtime/register-tui! session {:client-id "blocks"
                                    :mode :versioned-premise})
    (let [result
          (environment-io/load-blocks-state
           @session {:file (.getPath file)
                     :revision 0
                     :client-id "blocks"}
           effects/drain-environment-effects)
          view (runtime/read-tui-view (:state result) {:client-id "blocks"})]
      (is (= :loaded (get-in result [:receipt :status])))
      (is (= ["(def a 2)" "(+ a 3)"]
             (mapv :source (take 2 (:blocks view)))))
      (is (= [0 0] (mapv :version (take 2 (:blocks view))))))))

(deftest commit-supported-selects-one-active-definition
  (let [session (runtime/new-session)
        file (temp-file ".lain")]
    (runtime/register-tui! session {:client-id "commit"
                                    :mode :versioned-premise})
    (runtime/commit-version!
     session {:commit-id "00000000-0000-0000-0000-000000000011"
              :client-id "commit" :index 0 :expected-version nil
              :text "(def-net f [x] [out] (-> (+ x 1) out))"})
    (runtime/commit-version!
     session {:commit-id "00000000-0000-0000-0000-000000000012"
              :client-id "commit" :index 0 :expected-version 0
              :text "(def-net f [x] [out] (-> (+ x 10) out))"})
    (let [result (environment-io/save-state
                  @session {:file (.getPath file)
                            :mode :commit-supported
                            :selection :all
                            :checkpoint-id "commit-1"
                            :client-id "commit"})]
      (is (= :saved (get-in result [:receipt :status])))
      (is (= ['(def-net f [x] [out] (-> (+ x 10) out))]
             (environment-io/lain-forms (slurp file)))))))

(deftest preserve-export-keeps-private-candidate-source-and-reports-gap
  (let [session (runtime/new-session)
        file (temp-file ".lain")]
    (runtime/register-tui! session {:client-id "preserve"
                                    :mode :versioned-premise})
    (runtime/commit-version!
     session {:commit-id "00000000-0000-0000-0000-000000000021"
              :client-id "preserve" :index 0 :expected-version nil
              :text "(def x 1)"})
    (runtime/commit-version!
     session {:commit-id "00000000-0000-0000-0000-000000000022"
              :client-id "preserve" :index 0 :expected-version 0
              :text "(def x 2)"})
    (let [result (environment-io/save-state
                  @session {:file (.getPath file)
                            :mode :preserve-premises
                            :selection :all
                            :checkpoint-id "preserve-1"
                            :client-id "preserve"})
          forms (environment-io/lain-forms (slurp file))]
      (is (= :saved (get-in result [:receipt :status])))
      (is (= 3 (count forms)))
      (is (= '(def x 2) (last forms)))
      (is (= :preserved-candidates-archived
             (get-in result [:receipt :diagnostics 0 :warning]))))))
