(ns propagators.compiler-2.runtime.fixtures.project-math
  (:require [propagators.compiler-2.compiler.basis :as basis]))

(defn primitive-bindings
  []
  [['square (basis/primitive-operator #(* % %))]
   ['average (basis/primitive-operator #(/ (+ %1 %2) 2))]])
