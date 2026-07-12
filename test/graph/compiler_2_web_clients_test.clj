(ns graph.compiler-2-web-clients-test
  (:require [clojure.test :refer [deftest is]]
            [graph.compiler-2-web-clients :as web]
            [propagators.cells.value :as value]))

(deftest web-clients-route-through-coordinator-model
  (let [system (web/new-system)
        _ (web/load-model-file! system)
        a (web/register-client! system {:client-id "A"})
        b (web/register-client! system {:client-id "B"})]
    (is (not= (:session a) (:session b)))
    (is (web/route-allowed? system "A" "B"))
    (is (web/route-allowed? system "B" "A"))
    (let [result (web/route-message! system
                                     {:from "A"
                                      :to "B"
                                      :text "hello"})]
      (is (:delivered? result))
      (is (value/unusable? (web/latest-client-view system "A")))
      (is (= {:message/from "A"
              :message/to "B"
              :message/text "hello"
              :message/pipe "message"}
             (web/latest-client-view system "B"))))
    (web/register-client! system {:client-id "C"})
    (is (web/route-allowed? system "B" "C"))
    (let [result (web/route-message! system
                                     {:from "B"
                                      :to "C"
                                      :text "yo"})]
      (is (:delivered? result))
      (is (= {:message/from "B"
              :message/to "C"
              :message/text "yo"
              :message/pipe "message"}
             (web/latest-client-view system "C"))))))
