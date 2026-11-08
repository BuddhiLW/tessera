(ns tessera.compose.loops
  "Gallery export of the spec-built loops: the new composed scenes and two
   generated examples, compiled to tessera.loops specs. The parity scenes
   (fabric, kaleido-quasicrystal) are not exported: they duplicate the
   approved monolith loops pixel for pixel."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [tessera.compose.generate :as gen]
            [tessera.compose.scene :as scene]
            [tessera.compose.scenes :as scenes]))

(def generated-seeds
  "Seeds of the generated examples the live generator produces for the
   gallery. generated-2 was approved before the layer-role rules existed;
   its scene is frozen as data in generated_2.edn so it stays as approved."
  [3])

(def frozen-scenes
  "Approved generated scenes kept as EDN data."
  [(edn/read-string (slurp (io/resource "tessera/compose/generated_2.edn")))])

(def loops
  "Spec-built loops by id, in gallery order."
  (into (array-map)
        (map (fn [s] (let [spec (scene/->spec s)] [(:id spec) spec])))
        (concat [scenes/temple scenes/serpent-garden scenes/mandala-tunnel]
                frozen-scenes
                (map gen/generate generated-seeds))))
