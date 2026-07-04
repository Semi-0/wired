(ns graph.compiler-2-runtime.operators
  "Compiler-2 runtime operator registry facade."
  (:require [graph.compiler-2-runtime.operators.trace :as trace]
            [graph.compiler-2-runtime.operators.tui :as tui]
            [graph.compiler-2-runtime.operators.xr :as xr]))

(def block-target-operator tui/block-target-operator)
(def block-at-operator tui/block-at-operator)
(def be-block-at-operator tui/be-block-at-operator)
(def be-block-target-operator tui/be-block-target-operator)
(def instance-operator tui/instance-operator)
(def translate-operator tui/translate-operator)

(def trace-target-operator tui/trace-target-operator)
(def trace-operator trace/trace-operator)

(def xr-io-operator xr/xr-io-operator)
(def io-xr-operator xr/io-xr-operator)
