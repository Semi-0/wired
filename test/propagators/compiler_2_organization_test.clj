(ns propagators.compiler-2-organization-test
  (:require [clojure.test :refer [deftest is]]
            [propagators.compiler-2.compiler.cps :as compiler]
            [propagators.compiler-2.compiler.declarations :as declarations]
            [propagators.compiler-2.compiler.handlers :as handlers]
            [propagators.compiler-2.compiler.predicates :as predicates]
            [propagators.compiler-2.core :as core]
            [propagators.compiler-2.cps-core :as cps-shim]
            [propagators.compiler-2.deprecated.synchronous :as synchronous]
            [propagators.compiler-2.main :as main]
            [propagators.compiler-2.predicate-core :as predicate-shim]))

(deftest cps-is-the-canonical-production-compiler
  (is (identical? main/default-compiler compiler/default-compiler))
  (is (identical? main/compile* compiler/compile*))
  (is (identical? cps-shim/default-compiler compiler/default-compiler))
  (is (nil? (get (ns-aliases 'propagators.compiler-2.compiler.cps)
                 'predicate-core)))
  (is (= 'propagators.compiler-2.compiler.handlers
         (-> #'handlers/compile-application meta :ns ns-name)))
  (is (= 'propagators.compiler-2.compiler.predicates
         (-> #'predicates/literal? meta :ns ns-name))))

(deftest deprecated-compilers-remain-source-compatible
  (is (:deprecated (meta (find-ns 'propagators.compiler-2.core))))
  (is (:deprecated (meta (find-ns 'propagators.compiler-2.cps-core))))
  (is (:deprecated (meta (find-ns
                          'propagators.compiler-2.predicate-core))))
  (is (:deprecated (meta (find-ns
                          'propagators.compiler-2.deprecated.synchronous))))
  (is (:deprecated (meta #'core/default-compiler)))
  (is (:deprecated (meta #'predicate-shim/default-compiler)))
  (is (identical? predicate-shim/default-compiler core/default-compiler))
  (is (identical? core/compile-literal synchronous/compile-literal))
  (is (identical? core/declare-closure declarations/declare-closure))
  (is (instance? clojure.lang.MultiFn main/g:compile))
  (is (identical? main/g:compile core/g:compile))
  (is (identical? main/g:apply core/g:apply))
  (is (identical? main/g:advance core/g:advance)))
