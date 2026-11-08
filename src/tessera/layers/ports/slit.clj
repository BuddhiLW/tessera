(ns tessera.layers.ports.slit
  "Field port of :double-slit (tessera.quantum/slit-shader at {:k 10 :d 2.6
   :a 0.3}): twelve Huygens sources, barrier at x = -4.5, screen at x = 4.6,
   two wave periods per loop. The closure's per-frame fringe profile does not
   depend on u, so its peak is a constant here and each pixel re-evaluates
   the profile at the centre of its 1024-sample cell."
  (:require [tessera.layers.ports.forms :as f]
            [tessera.quantum :as quantum]))

(def ^:private k 10.0)
(def ^:private barrier-x -4.5)
(def ^:private screen-x 4.6)
(def ^:private half (/ 2.6 2.0))
(def ^:private half-width (/ 0.3 2.0))

(def ^:private sources
  (vec (partition 2 (vec (quantum/slit-sources barrier-x 2.6 0.3 6)))))

(def ^:private norm (/ 1.0 (* 0.6 (count sources))))

(defn- amplitude
  "[re im] of the normalized Huygens sum at (x, y), in the closure's order."
  [x y]
  (loop [[[sx sy] & more :as s] sources re 0.0 im 0.0]
    (if (empty? s)
      [(* norm re) (* norm im)]
      (let [dx (- x sx) dy (- y sy)
            rr (Math/sqrt (+ (* dx dx) (* dy dy) 1e-6))
            w (/ 1.0 (Math/sqrt rr))]
        (recur more (+ re (* w (Math/cos (* k rr)))) (+ im (* w (Math/sin (* k rr)))))))))

(def ^:private profile-max
  "The closure's imax: the peak of its 1024-sample fringe profile."
  (max 1e-9 (reduce max (for [i (range 1024)
                              :let [[a b] (amplitude screen-x (+ -4.5 (* 9.0 (/ (+ i 0.5) 1024))))]]
                          (+ (* a a) (* b b))))))

(defn- amplitude-form
  "Let bindings putting the Huygens sum at (ax, ay) into <pre>_re, <pre>_im."
  [pre ax ay]
  (let [terms (map-indexed
               (fn [i [sx sy]]
                 (let [s (fn [n] (symbol (str pre "_" n i)))]
                   [[(s "dx") (list '- ax sx) (s "dy") (list '- ay sy)
                     (s "rr") (list 'sqrt (list '+ (list '* (s "dx") (s "dx")) (list '* (s "dy") (s "dy")) 1e-6))
                     (s "w") (list '/ 1.0 (list 'sqrt (s "rr")))]
                    (list '* (s "w") (list 'cos (list '* k (s "rr"))))
                    (list '* (s "w") (list 'sin (list '* k (s "rr"))))]))
               sources)]
    (vec (concat (mapcat first terms)
                 [(symbol (str pre "_re")) (list '* norm (f/sum-of (map second terms)))
                  (symbol (str pre "_im")) (list '* norm (f/sum-of (map #(nth % 2) terms)))]))))

(defn- paint
  "[bindings [r g b]] of the closure's paint: crests red where re > 0,
   blue where re < 0, over a faint |psi| glow."
  [pre re mag]
  (let [s #(symbol (str pre "_" %))
        [pr pg pb] [1.0 (/ 0x2b 255.0) (/ 0x55 255.0)]
        [nr ng nb] [(/ 0x2f 255.0) (/ 0x6b 255.0) 1.0]]
    [[(s "s") (list '* (list 'min 1.0 mag) (list 'pow (list 'min 1.0 (list 'abs re)) 'sharpness))
      (s "glow") (list '* 0.10 (list 'min 1.0 mag))
      (s "pos") (list '- 1.0 (f/ge (list '- re)))]
     (f/select3 (s "pos")
                [(list '+ (s "glow") (list '* pr (s "s")))
                 (list '+ (list '* 0.6 (s "glow")) (list '* pg (s "s")))
                 (list '+ (s "glow") (list '* pb (s "s")))]
                [(list '+ (list '* 0.6 (s "glow")) (list '* nr (s "s")))
                 (list '+ (list '* 0.6 (s "glow")) (list '* ng (s "s")))
                 (list '+ (s "glow") (list '* nb (s "s")))])]))

(def double-slit
  (let [[left-bindings left-rgb] (paint "left" (list 'cos (list '- (list '* k 'x) 'wt)) 0.8)
        [mid-bindings mid-rgb] (paint "mid" '(/ mre (max 1.0e-9 mmag)) '(* 1.4 mmag))
        barrier (list '* (f/lt (list 'abs (list '- 'x barrier-x)) 0.05)
                      (list '- 1.0 (list 'max (f/lt (list 'abs (list '- 'y half)) half-width)
                                         (f/lt (list 'abs (list '+ 'y half)) half-width))))
        ;; the screen: the wall glow, then the intensity bar, then dim air
        screen-rgb (f/select3 'below_bar
                              '[(* 1.0 sa_g) (* 0.85 sa_g) (* 0.6 sa_g)]
                              (f/select3 'in_bar
                                         '[(+ (* sb_g 1.0) sb_tip) (+ (* sb_g (+ 0.35 (* 0.45 iy))) sb_tip)
                                           (+ (* sb_g 0.25) (* 0.8 sb_tip))]
                                         '[sc_g sc_g (* 1.4 sc_g)]))
        bindings (vec (concat
                       '[wt (* tau 2.0 u) cw (cos wt) sw (sin wt)
                         ;; the closure's fringe-profile cell and its centre
                         prof_i (clamp (floor (* 1024.0 (/ (- y -4.5) 9.0))) 0.0 1023.0)
                         prof_y (+ -4.5 (* 9.0 (/ (+ prof_i 0.5) 1024.0)))]
                       (amplitude-form "prof" screen-x 'prof_y)
                       ['iy (list '/ '(+ (* prof_re prof_re) (* prof_im prof_im)) profile-max)]
                       (amplitude-form "scr" screen-x 'y)
                       ['sre '(+ (* scr_re cw) (* scr_im sw))
                        'smag2 '(max 1.0e-12 (+ (* scr_re scr_re) (* scr_im scr_im)))
                        'hit '(/ (* sre sre) smag2)
                        'flash '(+ 0.45 (* 0.55 hit))
                        'rel (list '/ (list '- 'x screen-x) 'bar_length)
                        'below_bar (f/lt 'rel 0.03) 'in_bar (f/lt 'rel 'iy)
                        'sa_g '(* flash (sqrt iy))
                        'sb_g '(* flash (+ 0.25 (* 0.75 iy)) (- 1.0 (* 0.5 (/ rel (max 1.0e-6 iy)))))
                        'sb_tip '(exp (* -400.0 (pow (- rel iy) 2.0)))
                        'sc_g '(* 0.04 iy)]
                       (amplitude-form "mid" 'x 'y)
                       ['mre '(+ (* mid_re cw) (* mid_im sw))
                        'mmag '(sqrt (+ (* mid_re mid_re) (* mid_im mid_im)))]
                       left-bindings mid-bindings
                       ['barrier barrier
                        'left (f/lt 'x barrier-x)
                        'on_screen (f/ge (list '- 'x screen-x))]))]
    {:id :double_slit
     :title "Double slit: a plane wave through two slits, the fringe intensity standing out of the screen"
     :period 12 :view [(/ 128.0 9.0) 8.0] :blend :lighten
     ;; Wave number and slit geometry stay fixed: profile-max is a constant
     ;; of them, so they cannot be live sliders.
     :params [['sharpness {:default 4.0 :min 0.5 :max 16.0 :label "Crest sharpness"}]
              ['bar_length {:default 2.2 :min 0.3 :max 6.0 :label "Fringe graph length"}]]
     :form {:params '[u sharpness bar_length]
            :body (list 'let bindings
                        (f/select3 'barrier [0.35 0.36 0.45]
                                   (f/select3 'left left-rgb (f/select3 'on_screen screen-rgb mid-rgb))))}}))
