(ns tessera.layers.ports.modes
  "Field ports of standing-wave loops: the 16 x 9 box modes and the
   Pythagorean drum (tessera.orbitals), and the sine-Gordon breather
   lattice (tessera.waves)."
  (:require [tessera.layers.ports.forms :as f]
            [tessera.waves :as waves]))

;; ── Box modes ───────────────────────────────────────────────────────────

(def box-modes
  (let [modes [[1 1] [2 1] [2 2] [1 3] [3 1]]
        coefficients [1.0 -0.82 0.72 0.62 -0.55]
        terms (for [[i [m n] a] (map vector (range) modes coefficients)
                    :let [s #(symbol (str % "_" i))]]
                [[(s "ang") (list '* (* f/tau (+ (* m m) (* n n))) 'u)
                  (s "sp") (list '* a (f/table-sine m 'qx 2048) (f/table-sine n 'qy 2048))]
                 (list '* (s "sp") (list 'cos (s "ang")))
                 (list '* (s "sp") (list 'sin (s "ang")))])
        [hr hg hb] (f/hue-wheel 'hue)]
    {:id :box_modes
     :title "Box modes: five commensurate well eigenstates, saturating |psi|^2 glow in phase colour with fine contours and well walls"
     :period 12 :view [16.0 9.0] :blend :lighten
     :params [['tone_k {:default (/ (- (Math/log (- 1.0 0.85))) 5.097170099097348)
                        :min 0.05 :max 2.0 :label "Tone-map saturation"}]
              ['contour_density {:default 3.0 :min 0.5 :max 12.0 :label "Iso-density contours per unit"}]]
     :form {:params '[u tone_k contour_density]
            :body (list 'let (vec (concat
                                   '[qx (+ 0.5 (/ x 16.0)) qy (+ 0.5 (/ y 9.0))]
                                   (mapcat first terms)
                                   ['re (f/sum-of (map second terms)) 'im (f/sum-of (map #(nth % 2) terms))]
                                   '[d (+ (* re re) (* im im))
                                     L (- 1.0 (exp (* (- tone_k) d)))
                                     hue (+ 0.58 (/ (atan2 im re) tau))]
                                   ['hr hr 'hg hg 'hb hb]
                                   '[w (min 1.0 (* 2.6 L))
                                     cr (+ (* w hr) (* (- 1.0 w) 0.20))
                                     cg (+ (* w hg) (* (- 1.0 w) 0.45))
                                     cb (+ (* w hb) (* (- 1.0 w) 0.80))
                                     lv (* d contour_density)
                                     fr (- lv (floor lv))
                                     dist (min fr (- 1.0 fr))
                                     line (* (exp (* -260.0 (* dist dist))) (min 1.0 (* 3.0 L)))
                                     wx (min qx (- 1.0 qx)) wy (min qy (- 1.0 qy))
                                     wall (+ (exp (* -4000.0 (* wx wx))) (exp (* -4000.0 (* wy wy))))]))
                        '[(+ 0.001 (* L cr) (* line 0.45) (* wall 0.10))
                          (+ 0.002 (* L cg) (* line 0.45) (* wall 0.13))
                          (+ 0.006 (* L cb) (* line 0.45) (* wall 0.20))])}}))

;; ── Drum ────────────────────────────────────────────────────────────────

(defn- odd-reflection
  "mod(c/tile, 2) folded into [0, 1]: Clojure's double mod is exactly
   n - 2 floor(n/2) here, and q <= 1 keeps q, else 2 - q."
  [c]
  (list 'let ['n (list '/ c 'tile_scale)
              'q '(- n (* 2.0 (floor (/ n 2.0))))
              'keep (f/ge '(- 1.0 q))]
        '(+ (* keep q) (* (- 1.0 keep) (- 2.0 q)))))

(def drum
  (let [modes [[3 4] [4 3] [6 8] [8 6] [9 12] [12 9] [12 16] [16 12]]
        coefficients [0.32 0.26 0.23 -0.19 0.18 0.16 -0.15 0.13]
        terms (for [[i [m n] a] (map vector (range) modes coefficients)
                    :let [k (quot (long (Math/sqrt (+ (* m m) (* n n)))) 5)
                          offset (if (even? i) 0.0 (* 0.5 Math/PI))]]
                (list '* 2.0 a (f/table-sine m 'qx 4096) (f/table-sine n 'qy 4096)
                      (list 'cos (list '+ (list '* (* f/tau k) 'u) offset))))]
    {:id :drum_modes
     :title "Drum: Pythagorean membrane modes, luminous Chladni lines in a 1:2:3:4 resonance"
     :period 12 :view [(/ 128.0 9.0) 8.0] :blend :lighten
     :params [['tile_scale {:default 4.0 :min 1.0 :max 12.0 :label "Membrane size (scene units)"}]
              ['node_sharpness {:default 42.0 :min 4.0 :max 200.0 :label "Nodal-line sharpness"}]]
     :form {:params '[u tile_scale node_sharpness]
            :body (list 'let ['qx (odd-reflection 'x) 'qy (odd-reflection 'y)
                              'displacement (f/sum-of terms)
                              'distance '(abs displacement)
                              'node '(exp (* (- node_sharpness) distance))
                              'body '(* 0.07 (min 1.0 (* 2.2 distance)))
                              'g '(+ body node)
                              'halo '(* 0.10 (exp (* -8.0 distance)))]
                        '[(+ (* 0.08 halo) (* 0.20 g) (* 0.42 body))
                          (+ (* 0.48 halo) (* 0.83 g) (* 0.25 body))
                          (+ (* 0.75 halo) (* 0.96 g) (* 0.75 body))])}}))
