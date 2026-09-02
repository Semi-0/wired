(ns propagators.tui.integration.runtime-organization-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.compiler.cps-core :as compiler]
            [propagators.compiler.compiler.declarations :as declarations]
            [propagators.compiler.compiler.handlers :as handlers]
            [propagators.compiler.compiler.predicates :as predicates]
            [propagators.compiler.deprecated.legacy-core :as legacy-core]
            [propagators.compiler.deprecated.compiler-core :as compiler-shim]
            [propagators.compiler.deprecated.core :as deprecated-core]
            [propagators.compiler.deprecated.synchronous :as synchronous]
            [propagators.compiler.main :as main]
            [propagators.compiler.predicate-core :as predicate-shim]
            [propagators.runtime :as runtime]
            [propagators.tui.graph.compiler-2-runtime :as runtime-shim]))

(deftest cps-is-the-canonical-production-compiler
  (is (identical? main/default-compiler compiler/default-compiler))
  (is (identical? main/compile* compiler/compile*))
  (is (identical? compiler-shim/default-compiler compiler/default-compiler))
  (is (nil? (get (ns-aliases 'propagators.compiler.cps-core)
                 'predicate-core)))
  (is (= 'propagators.compiler.compiler.handlers
         (-> #'handlers/compile-application meta :ns ns-name)))
  (is (= 'propagators.compiler.compiler.predicates
         (-> #'predicates/literal? meta :ns ns-name))))

(deftest deprecated-compilers-remain-source-compatible
  (is (:deprecated
       (meta (find-ns 'propagators.compiler.deprecated.legacy-core))))
  (is (:deprecated (meta (find-ns
                          'propagators.compiler.deprecated.compiler-core))))
  (is (:deprecated (meta #'compiler-shim/default-compiler)))
  (is (:deprecated (meta (find-ns
                          'propagators.compiler.deprecated.core))))
  (is (not (:deprecated (meta (find-ns
                               'propagators.compiler.cps-core)))))
  (is (:deprecated (meta (find-ns
                          'propagators.compiler.predicate-core))))
  (is (:deprecated (meta (find-ns
                          'propagators.compiler.deprecated.synchronous))))
  (is (:deprecated (meta #'legacy-core/default-compiler)))
  (is (:deprecated (meta #'predicate-shim/default-compiler)))
  (is (identical? predicate-shim/default-compiler
                  legacy-core/default-compiler))
  (is (identical? legacy-core/default-compiler
                  deprecated-core/default-compiler))
  (is (identical? legacy-core/compile-literal synchronous/compile-literal))
  (is (identical? legacy-core/declare-closure
                  declarations/declare-closure))
  (is (instance? clojure.lang.MultiFn main/g:compile))
  (is (identical? main/g:compile legacy-core/g:compile))
  (is (identical? main/g:apply legacy-core/g:apply))
  (is (identical? main/g:advance legacy-core/g:advance)))

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
