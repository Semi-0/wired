(ns propagators.compiler-2-organization-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.compiler-2.cps-core :as compiler]
            [propagators.compiler-2.compiler.declarations :as declarations]
            [propagators.compiler-2.compiler.handlers :as handlers]
            [propagators.compiler-2.compiler.predicates :as predicates]
            [propagators.compiler-2.core :as core]
            [propagators.compiler-2.compiler.core :as compiler-shim]
            [propagators.compiler-2.deprecated.core :as deprecated-core]
            [propagators.compiler-2.deprecated.synchronous :as synchronous]
            [propagators.compiler-2.main :as main]
            [propagators.compiler-2.predicate-core :as predicate-shim]
            [propagators.compiler-2.runtime :as runtime]
            [graph.compiler-2-runtime :as runtime-shim]))

(deftest cps-is-the-canonical-production-compiler
  (is (identical? main/default-compiler compiler/default-compiler))
  (is (identical? main/compile* compiler/compile*))
  (is (identical? compiler-shim/default-compiler compiler/default-compiler))
  (is (nil? (get (ns-aliases 'propagators.compiler-2.cps-core)
                 'predicate-core)))
  (is (= 'propagators.compiler-2.compiler.handlers
         (-> #'handlers/compile-application meta :ns ns-name)))
  (is (= 'propagators.compiler-2.compiler.predicates
         (-> #'predicates/literal? meta :ns ns-name))))

(deftest deprecated-compilers-remain-source-compatible
  (is (:deprecated (meta (find-ns 'propagators.compiler-2.core))))
  (is (:deprecated (meta (find-ns
                          'propagators.compiler-2.compiler.core))))
  (is (:deprecated (meta #'compiler-shim/default-compiler)))
  (is (:deprecated (meta (find-ns
                          'propagators.compiler-2.deprecated.core))))
  (is (not (:deprecated (meta (find-ns
                               'propagators.compiler-2.cps-core)))))
  (is (:deprecated (meta (find-ns
                          'propagators.compiler-2.predicate-core))))
  (is (:deprecated (meta (find-ns
                          'propagators.compiler-2.deprecated.synchronous))))
  (is (:deprecated (meta #'core/default-compiler)))
  (is (:deprecated (meta #'predicate-shim/default-compiler)))
  (is (identical? predicate-shim/default-compiler core/default-compiler))
  (is (identical? core/default-compiler deprecated-core/default-compiler))
  (is (identical? core/compile-literal synchronous/compile-literal))
  (is (identical? core/declare-closure declarations/declare-closure))
  (is (instance? clojure.lang.MultiFn main/g:compile))
  (is (identical? main/g:compile core/g:compile))
  (is (identical? main/g:apply core/g:apply))
  (is (identical? main/g:advance core/g:advance)))

(deftest live-runtime-is-owned-by-propagators
  (is (not (:deprecated (meta (find-ns
                               'propagators.compiler-2.runtime)))))
  (is (:deprecated (meta (find-ns 'graph.compiler-2-runtime))))
  (is (identical? runtime/new-session runtime-shim/new-session))
  (is (identical? runtime/compile-source! runtime-shim/compile-source!))
  (is (identical? runtime/commit-version! runtime-shim/commit-version!)))
