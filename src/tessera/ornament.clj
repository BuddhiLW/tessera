(ns tessera.ornament
  "Girih patterns as frames, and lights that sweep across them."
  (:require [desargues.wallpaper.girih :as g]
            [desargues.wallpaper.loop :as lp]
            [tessera.palette :as pal]))

(set! *warn-on-reflection* true)

(defn- path [pts style]
  (merge {:node :path :points pts :opacity 1.0} style))

(defn girih-shapes
  "Frame shapes of a Hankin pattern `polys` (desargues.wallpaper.girih/pattern)
   at `scale` scene units per tiling unit: each polygon's ground, its rosette
   filled by polygon kind, then every strand as a wide ink band under a
   narrow strand line. opts: {:colors :band :line :xf}, where `xf` maps a
   polygon to {:offset [x y] :turn radians :opacity k}."
  [polys scale {:keys [colors band line xf] :or {colors pal/girih band 13 line 6}}]
  (let [sc (fn [p] (g/v* scale p))
        placed (vec (for [poly polys
                          :let [{:keys [offset turn opacity] :or {offset [0.0 0.0] turn 0.0 opacity 1.0}}
                                (if xf (xf poly) {})
                                c (:centre poly)
                                move (fn [p] (sc (g/v+ (g/v+ c (g/rot turn (g/v- p c))) offset)))]]
                      {:poly poly :move move :opacity opacity}))]
    (vec (concat
          (for [{:keys [poly move opacity]} placed]
            (path (mapv move (:vertices poly))
                  {:opts {:closed? true} :opacity opacity
                   :fill {:color (:ground colors) :opacity 1.0} :stroke {:color nil}}))
          (for [{:keys [poly move opacity]} placed]
            (path (mapv move (:rosette poly))
                  {:opts {:closed? true} :opacity opacity
                   :fill {:color (get colors (:kind poly) (:ground colors)) :opacity 0.85}
                   :stroke {:color nil}}))
          (for [{:keys [poly move opacity]} placed
                s (:strands poly)]
            (path (mapv move s) {:opacity opacity :stroke {:color (:ink colors) :width band}}))
          (for [{:keys [poly move opacity]} placed
                s (:strands poly)]
            (path (mapv move s) {:opacity opacity :stroke {:color (:strand colors) :width line}}))))))

(defn rect-of
  "The window [x0 x1 y0 y1] in tiling units that `view` (scene units) shows
   at `scale` scene units per tiling unit, centred on tiling point `c`."
  [[vw vh] scale [cx cy]]
  (let [hw (/ (double vw) 2.0 scale) hh (/ (double vh) 2.0 scale)]
    [(- cx hw) (+ cx hw) (- cy hh) (+ cy hh)]))

(defn sweep-light
  "A light at phase `u`: a soft band of brightness `peak` over `floor`,
   crossing the plane along `angle` with wavelength `lambda` (scene units),
   one wavelength per loop."
  [u {:keys [angle lambda width floor peak] :or {angle 0.6 lambda 9.0 width 0.11 floor 0.72 peak 0.55}}]
  (let [ca (Math/cos angle) sa (Math/sin angle)
        u (double u) lambda (double lambda) width (double width)
        floor (double floor) peak (double peak)]
    (fn ^double [^double x ^double y]
      (let [p (lp/frac (- (/ (+ (* x ca) (* y sa)) lambda) u))
            d (/ (- p 0.5) width)]
        (+ floor (* peak (Math/exp (- (* d d)))))))))
