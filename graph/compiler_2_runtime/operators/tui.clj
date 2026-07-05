(ns graph.compiler-2-runtime.operators.tui
  "TUI/block compiler-2 runtime operators."
  (:require [graph.compiler-2-runtime.operators.tui.effects :as effects]
            [graph.compiler-2-runtime.operators.tui.targets :as targets]
            [graph.compiler-2-runtime.operators.tui.translate :as translate]))

(def block-target-operator targets/block-target-operator)
(def trace-target-operator targets/trace-target-operator)
(def instance-operator targets/instance-operator)
(def block-at-operator effects/block-at-operator)
(def be-block-at-operator effects/be-block-at-operator)
(def be-block-target-operator effects/be-block-target-operator)
(def translate-operator translate/translate-operator)
