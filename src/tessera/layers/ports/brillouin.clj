(ns tessera.layers.ports.brillouin
  "Field ports of the Brillouin-zone loops on the square, oblique and
   rectangular lattices (tessera.reciprocal/brillouin). Every reciprocal
   point within radius 9.5 is unrolled as one Bragg plane, as
   tessera.layers/brillouin-zones does for the hexagonal lattice: 292, 344
   and 456 planes, so these shaders are large (see tessera.layers.ports
   follow-ups)."
  (:require [clojure.walk :as walk]
            [tessera.layers.ports.forms :as f]
            [tessera.reciprocal :as recip]))

(defn- zone-layer
  "recip/brillouin on `lattice`, coloured by `scheme` walked there and back
   (pal/around) as the zone drift cycles."
  [lattice scheme id title]
  (let [gs (partition 3 (vec (recip/reciprocal-points lattice 9.5)))
        {:keys [bindings zone nearest]}
        (reduce (fn [{:keys [bindings zone nearest]} [i [gx gy r2]]]
                  (let [d (symbol (str "plane_" i))
                        z (symbol (str "zone_" i))
                        m (symbol (str "nearest_" i))]
                    {:bindings (into bindings
                                     [d (list '* (/ 1.0 (Math/sqrt r2))
                                              (list '- (list '+ (list '* 'x gx) (list '* 'y gy)) (* 0.5 r2)))
                                      ;; ceil(d) clipped: 1 iff d > 0, as (pos? d)
                                      z (list '+ zone (list 'clamp (list '- (list 'floor (list '- d))) 0.0 1.0))
                                      m (list 'min nearest (list 'abs d))])
                     :zone z :nearest m}))
                {:bindings [] :zone 1.0 :nearest 1.0e9}
                (map-indexed vector gs))
        [cr cg cb] (f/palette-channels scheme 'palette_t)
        tail (walk/postwalk-replace
              {'ZONE zone 'NEAREST nearest 'RED cr 'GREEN cg 'BLUE cb}
              '(let [q (/ NEAREST edge_width)
                     glow (exp (- (* q q)))
                     drift (- (/ ZONE pulse_zones) u)
                     pulse (+ 0.25 (* 0.75 (pow (wave (frac drift)) 3.0)))
                     palette_t (- 0.5 (* 0.5 (cos (* tau drift))))
                     cr RED cg GREEN cb BLUE
                     fill (* 0.32 pulse)
                     e (* glow (+ 0.4 (* 0.6 pulse)))]
                 [(+ (* cr fill) (* e (+ 0.5 (* 0.5 cr))))
                  (+ (* cg fill) (* e (+ 0.5 (* 0.5 cg))))
                  (+ (* cb fill) (* e (+ 0.5 (* 0.5 cb))))]))]
    {:id id :title title :period 20 :view [8.0 4.5] :blend :lighten
     :params [['edge_width {:default 0.012 :min 0.002 :max 0.12 :label "Bragg-plane glow width"}]
              ['pulse_zones {:default 12.0 :min 2.0 :max 48.0 :label "Zones per light pulse"}]]
     :form {:params '[u edge_width pulse_zones]
            :body (list 'let bindings tail)}}))

(def brillouin-square
  (zone-layer :square :green-blue-orange :brillouin_square
              "Brillouin zones of the square lattice: four-fold stained glass"))

(def brillouin-oblique
  (zone-layer :oblique :orange-magenta-blue :brillouin_oblique
              "Brillouin zones of an oblique lattice: only inversion survives"))

(def brillouin-rect
  (zone-layer :rect :yellow-green-blue :brillouin_rect
              "Brillouin zones of a rectangular lattice"))
