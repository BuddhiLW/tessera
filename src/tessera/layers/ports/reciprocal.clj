(ns tessera.layers.ports.reciprocal
  "Field ports of tessera.reciprocal loops: twisted bilayer graphene, the
   :fold-<n> quasiperiodic contours and the Brillouin zones of the square,
   oblique and rectangular lattices."
  (:require [clojure.walk :as walk]
            [tessera.layers.ports.forms :as f]
            [tessera.reciprocal :as recip]))

;; ── Twisted bilayer graphene ────────────────────────────────────────────

(defn- honeycomb-density [theta]
  (f/sum-of (for [i (range 3)
                  :let [off (* i (/ f/tau 3.0))]]
              (list 'let ['a (list '+ theta off)]
                    '(cos (* wave_number (+ (* x (cos a)) (* y (sin a)))))))))

(def twisted-bilayer
  (let [[cr cg cb] (f/palette-channels :yellow-purple-magenta 'palette_t)]
    {:id :twisted_bilayer
     :title "Twisted bilayer graphene: the moire superlattice, light pulsing through"
     :period 16 :view [8.0 4.5] :blend :lighten
     :params [['wave_number {:default 26.0 :min 4.0 :max 60.0 :label "Honeycomb wave number"}]
              ['twist {:default 0.11 :min 0.0 :max 0.6 :label "Twist angle (radians)"}]
              ['pulse_spacing {:default 5.0 :min 0.5 :max 20.0 :label "Light pulse spacing"}]]
     :form {:params '[u wave_number twist pulse_spacing]
            :body (walk/postwalk-replace
                   {'D1 (honeycomb-density 't1) 'D2 (honeycomb-density 't2)
                    'RED cr 'GREEN cg 'BLUE cb}
                   '(let [t1 (- (* 0.5 twist)) t2 (* 0.5 twist)
                          h1 (/ (- 1.5 D1) 4.5) h2 (/ (- 1.5 D2) 4.5)
                          both (* h1 h2)
                          ;; pow of a negative is NaN, which packs to 0 on the
                          ;; JVM; max(.,0) packs to the same 0 and is defined
                          ;; on a GPU.
                          atoms (pow (max both 0.0) 1.5)
                          r (sqrt (+ (* x x) (* y y)))
                          pulse (+ 0.4 (* 0.6 (pow (wave (frac (- (/ r pulse_spacing) u))) 2.0)))
                          t (+ (* 0.5 (+ h1 h2)) (* 0.08 r))
                          palette_t (- 0.5 (* 0.5 (cos (* tau t))))
                          cr RED cg GREEN cb BLUE
                          v (* atoms pulse 2.2)]
                      [(* v cr) (* v cg) (* v cb)]))}}))

;; ── n-fold quasiperiodic contours (recip/symmetry-family) ───────────────

(defn- fold-layer
  "The :fold-<n> contour loop: n - sum_j cos(a k.e_j) over angles j pi/n,
   viewed at an offset off the central rosette (now a pair of params)."
  [n scheme [ox oy]]
  (let [waves (for [j (range n)
                    :let [angle (/ (* Math/PI j) n)]]
                (list 'cos (list '* 'lattice_scale (list '+ (list '* 'mx (Math/cos angle))
                                                         (list '* 'my (Math/sin angle))))))
        [cr cg cb] (f/palette-channels scheme 'palette_t)]
    {:id (keyword (str "fold_" n)) :title (str n "-fold quasiperiodic contours")
     :period 16 :view [8.0 4.5] :blend :lighten
     :params [['fermi_level {:default 0.0 :min -4.0 :max 4.0 :label "Fermi level offset"}]
              ['contour_step {:default (* 0.1 n) :min 0.05 :max 4.0 :label "Energy contour spacing"}]
              ['sharpness {:default 24.0 :min 2.0 :max 160.0 :label "Contour sharpness"}]
              ['lattice_scale {:default 5.0 :min 0.5 :max 12.0 :label "Lattice wave number"}]
              ['offset_x {:default ox :min -40.0 :max 40.0 :label "View offset x"}]
              ['offset_y {:default oy :min -40.0 :max 40.0 :label "View offset y"}]]
     :form {:params '[u fermi_level contour_step sharpness lattice_scale offset_x offset_y]
            :body (walk/postwalk-replace
                   {'BAND (list '- (double n) (f/sum-of waves)) 'RANGE (* 2.0 n)
                    'RED cr 'GREEN cg 'BLUE cb}
                   '(let [mx (+ x offset_x) my (+ y offset_y)
                          e BAND
                          contour_phase (frac (- (/ (- e fermi_level) contour_step) u))
                          line (pow (wave contour_phase) sharpness)
                          dz (- e 0.0)
                          cusp (exp (* -40.0 dz dz))
                          palette_t (/ dz RANGE)
                          cr RED cg GREEN cb BLUE
                          k (+ 0.10 (* 0.95 line))]
                      [(+ (* cr k) (* 0.8 cusp)) (+ (* cg k) (* 0.8 cusp)) (+ (* cb k) (* 0.8 cusp))]))}}))

(def fold-9 (fold-layer 9 :yellow-magenta-cyan [7.3 2.1]))
(def fold-10 (fold-layer 10 :blue-magenta-orange [11.6 -4.4]))
(def fold-11 (fold-layer 11 :green-red [-9.2 6.7]))
(def fold-12 (fold-layer 12 :cyan-magenta [14.1 3.3]))
