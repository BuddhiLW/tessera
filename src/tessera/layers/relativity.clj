(ns tessera.layers.relativity
  "Live relativity fields, kept on the layer side of the dependency graph:
   offline loops never require the live registry."
  (:require [tessera.relativity :as relativity]))

(def layers relativity/live-layers)
