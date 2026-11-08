(ns tessera.compose.loops
  "Gallery export of the spec-built loops: the new composed scenes and two
   generated examples, compiled to tessera.loops specs. The parity scenes
   (fabric, kaleido-quasicrystal) are not exported: they duplicate the
   approved monolith loops pixel for pixel."
  (:require [tessera.compose.generate :as gen]
            [tessera.compose.scene :as scene]
            [tessera.compose.scenes :as scenes]))

(def generated-seeds
  "Seeds of the generated examples in the gallery."
  [2 3])

(def loops
  "Spec-built loops by id, in gallery order."
  (into (array-map)
        (map (fn [s] (let [spec (scene/->spec s)] [(:id spec) spec])))
        (concat [scenes/temple scenes/serpent-garden scenes/mandala-tunnel]
                (map gen/generate generated-seeds))))
