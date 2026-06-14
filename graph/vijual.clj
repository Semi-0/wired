;; Maintained from Conrad Barski's vijual idea, modernized for this repo.
(ns graph.vijual
  (:require [graph.vijual.layout :as layout]
            [graph.vijual.render :as render])
  (:import (java.io File)
           (javax.imageio ImageIO)))

(def ascii-dim render/ascii-dim)
(def image-dim render/image-dim)

(def integer-shapes render/integer-shapes)
(def graph-to-shapes render/graph-to-shapes)
(def draw-shapes render/draw-shapes)
(def draw-shapes-image render/draw-shapes-image)

(def layout-result layout/layout-result)
(def layout-graph layout/layout-graph)
(def layout-force-result layout/layout-force-result)
(def layout-force-graph layout/layout-force-graph)
(def layout-force-directed-result layout/layout-force-directed-result)
(def layout-force-directed-graph layout/layout-force-directed-graph)
(def layout-stress-result layout/layout-stress-result)
(def layout-stress-graph layout/layout-stress-graph)
(def layout-stress-directed-result layout/layout-stress-directed-result)
(def layout-stress-directed-graph layout/layout-stress-directed-graph)
(def layout-sugiyama-result layout/layout-sugiyama-result)
(def layout-sugiyama-graph layout/layout-sugiyama-graph)
(def label-text layout/label-text)

(declare draw-directed-graph)

(defn draw-graph
  "Draws an undirected graph to the console."
  ([edges nodes opts]
   (draw-shapes ascii-dim
                (integer-shapes
                 (graph-to-shapes ascii-dim
                                  (layout-graph ascii-dim edges nodes false opts)))))
  ([edges nodes]
   (draw-graph edges nodes {}))
  ([edges]
   (draw-graph edges {})))

(defn draw-force-graph
  "Draws an undirected graph using force-directed placement snapped to the ASCII grid."
  ([edges nodes opts]
   (draw-shapes ascii-dim
                (integer-shapes
                 (graph-to-shapes ascii-dim
                                  (layout-force-graph ascii-dim edges nodes opts)))))
  ([edges nodes]
   (draw-force-graph edges nodes {}))
  ([edges]
   (draw-force-graph edges {})))

(defn draw-force-directed-graph
  "Draws a directed graph using force-directed placement snapped to the ASCII grid."
  ([edges nodes opts]
   (draw-shapes ascii-dim
                (integer-shapes
                 (graph-to-shapes ascii-dim
                                  (layout-force-directed-graph ascii-dim edges nodes opts)))))
  ([edges nodes]
   (draw-force-directed-graph edges nodes {}))
  ([edges]
   (draw-force-directed-graph edges {})))

(defn draw-stress-graph
  "Draws an undirected graph using stress majorization with aspect-ratio constrained snapping."
  ([edges nodes opts]
   (draw-shapes ascii-dim
                (integer-shapes
                 (graph-to-shapes ascii-dim
                                  (layout-stress-graph ascii-dim edges nodes opts)))))
  ([edges nodes]
   (draw-stress-graph edges nodes {}))
  ([edges]
   (draw-stress-graph edges {})))

(defn draw-stress-directed-graph
  "Draws a directed graph using stress majorization with aspect-ratio constrained snapping."
  ([edges nodes opts]
   (draw-shapes ascii-dim
                (integer-shapes
                 (graph-to-shapes ascii-dim
                                  (layout-stress-directed-graph ascii-dim edges nodes opts)))))
  ([edges nodes]
   (draw-stress-directed-graph edges nodes {}))
  ([edges]
   (draw-stress-directed-graph edges {})))

(defn draw-sugiyama-graph
  "Draws a directed graph using Sugiyama-style layered placement."
  ([edges nodes opts]
   (draw-shapes ascii-dim
                (integer-shapes
                 (graph-to-shapes ascii-dim
                                  (layout-sugiyama-graph ascii-dim edges nodes opts)))))
  ([edges nodes]
   (draw-sugiyama-graph edges nodes {}))
  ([edges]
   (draw-sugiyama-graph edges {})))

(defn draw-graph-comparison
  "Prints grid-swap, force-directed snapped, and Sugiyama layered layouts for the same graph."
  ([edges nodes opts]
   (println "Current layout:")
   (draw-graph edges nodes opts)
   (println)
   (println "Force-directed snapped layout:")
   (draw-force-graph edges nodes opts)
   (println)
   (println "Stress-majorized balanced layout:")
   (draw-stress-graph edges nodes opts)
   (println)
   (println "Sugiyama layered layout:")
   (draw-sugiyama-graph edges nodes opts))
  ([edges nodes]
   (draw-graph-comparison edges nodes {}))
  ([edges]
   (draw-graph-comparison edges {})))

(defn draw-directed-graph-comparison
  "Prints current directed, force-directed, and Sugiyama layered layouts for the same directed graph."
  ([edges nodes opts]
   (println "Current directed layout:")
   (draw-directed-graph edges nodes opts)
   (println)
   (println "Force-directed snapped layout:")
   (draw-force-directed-graph edges nodes opts)
   (println)
   (println "Stress-majorized balanced layout:")
   (draw-stress-directed-graph edges nodes opts)
   (println)
   (println "Sugiyama layered layout:")
   (draw-sugiyama-graph edges nodes opts))
  ([edges nodes]
   (draw-directed-graph-comparison edges nodes {}))
  ([edges]
   (draw-directed-graph-comparison edges {})))

(defn draw-directed-graph
  "Draws a directed graph to the console."
  ([edges nodes opts]
   (draw-shapes ascii-dim
                (integer-shapes
                 (graph-to-shapes ascii-dim
                                  (layout-graph ascii-dim edges nodes true opts)))))
  ([edges nodes]
   (draw-directed-graph edges nodes {}))
  ([edges]
   (draw-directed-graph edges {})))

(defn draw-graph-image
  "Draws an undirected graph to a java image."
  ([edges nodes]
   (draw-shapes-image image-dim
                      (graph-to-shapes image-dim
                                       (layout-graph image-dim edges nodes false))))
  ([edges]
   (draw-graph-image edges {})))

(defn draw-directed-graph-image
  "Draws a directed graph to a java image."
  ([edges nodes]
   (draw-shapes-image image-dim
                      (graph-to-shapes image-dim
                                       (layout-graph image-dim edges nodes true))))
  ([edges]
   (draw-directed-graph-image edges {})))

(defn save-image [img name]
  (ImageIO/write img "png" (File. (str name ".png"))))

(defn draw-tree
  "Compatibility placeholder for the legacy tree renderer."
  [tree]
  (println (pr-str tree)))

(defn draw-binary-tree
  "Compatibility placeholder for the legacy binary tree renderer."
  [tree]
  (println (pr-str tree)))

(defn draw-tree-image
  "Compatibility placeholder returning a small image for tree input."
  [tree]
  (draw-graph-image [] {:tree (pr-str tree)}))
