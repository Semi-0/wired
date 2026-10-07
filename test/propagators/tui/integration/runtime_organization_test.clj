(ns propagators.tui.integration.runtime-organization-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [propagators.compiler.cps-core :as compiler]
            [propagators.compiler.compiler.declarations :as declarations]
            [propagators.compiler.compiler.handlers :as handlers]
            [propagators.compiler.compiler.predicates :as predicates]
            [propagators.compiler.main :as main]
            [propagators.runtime :as runtime]
            [propagators.tui.graph.compiler-2-runtime :as runtime-shim]))

(deftest cps-is-the-canonical-production-compiler
  (is (identical? main/default-compiler compiler/default-compiler))
  (is (identical? main/compile* compiler/compile*))
  (is (identical? main/compile-form compiler/compile-form))
  (is (identical? main/compile-program compiler/compile-program))
  (is (nil? (get (ns-aliases 'propagators.compiler.cps-core)
                 'predicate-core)))
  (is (= 'propagators.compiler.compiler.handlers
         (-> #'handlers/compile-application meta :ns ns-name)))
  (is (= 'propagators.compiler.compiler.predicates
         (-> #'predicates/literal? meta :ns ns-name))))

(deftest production-compiler-has-no-deprecated-evaluator-adapters
  (doseq [resource ["propagators/compiler/deprecated/legacy_core.clj"
                    "propagators/compiler/deprecated/compiler_core.clj"
                    "propagators/compiler/deprecated/core.clj"
                    "propagators/compiler/deprecated/synchronous.clj"
                    "propagators/compiler/predicate_core.clj"]]
    (is (nil? (io/resource resource))))
  (is (not (:deprecated (meta (find-ns
                               'propagators.compiler.cps-core)))))
  (is (fn? declarations/declare-closure)))

(deftest live-runtime-is-owned-by-propagators
  (is (not (:deprecated (meta (find-ns
                               'propagators.runtime)))))
  (is (:deprecated (meta (find-ns 'propagators.tui.graph.compiler-2-runtime))))
  (is (not (identical? runtime/new-session runtime-shim/new-session)))
  (is (= [:xr :widget :web-client]
         (get-in @(runtime-shim/new-session)
                 [:runtime/options :operator-groups])))
  (is (identical? runtime/compile-source! runtime-shim/compile-source!))
  (is (identical? runtime/commit-version! runtime-shim/commit-version!)))
