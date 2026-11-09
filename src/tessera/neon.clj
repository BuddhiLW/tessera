(ns tessera.neon
  "Neon khatam: eight-pointed stars and octagons in a checkerboard, every
   outline a string of glowing beads, on a tilted plane seen through bloom,
   a red/blue fringe and a shallow depth of field.

   Every motion is periodic in u: the plane drifts one cell diagonally (the
   checkerboard maps to itself), beads march one spacing, a ring of light
   travels one wavelength outward, inner outlines turn by a symmetry of
   their own shape (45 degrees for 8- and 24-fold, 90 for the 12-gon)."
  (:require [desargues.wallpaper.girih :as g]
            [desargues.wallpaper.loop :as lp]))

(def cell "Lattice spacing, scene units." 2.3)

(def colors {:blue "#3b63ff" :red "#ff2b45" :ice "#8ab8ff"})

(defn glow-form
  "Field form of a neon filament at signed distance `distance`: a tight
   Gaussian core and a finite-support halo, in the style of the bead strokes."
  [distance width]
  (list 'let ['q (list '/ distance width)
              'q2 '(* q q)
              'inside '(clamp (+ 1.0 (floor (- 36.0 q2))) 0.0 1.0)]
        '(* inside (+ (* 0.88 (exp (- q2))) (* 0.12 (exp (* -0.16 q2)))))))

(defn neon-form
  "Saturating, hue-preserving field exposure for channels `red green blue`.
   Bright cores retain their saturated hue rather than flattening to white."
  [red green blue]
  (list 'let ['red red 'green green 'blue blue
              'hi '(max red (max green blue)) 'lo '(min red (min green blue))
              'span '(- hi lo)
              'visible '(* (clamp (+ 1.0 (floor (- hi 0.003))) 0.0 1.0)
                           (clamp (+ 1.0 (floor (- span 1.0e-12))) 0.0 1.0))
              'v '(* 0.95 (- 1.0 (exp (* -1.65 hi))))
              'scale '(/ (* 0.98 v) (max span 1.0e-12))
              'base '(* 0.02 v)]
        '[(* visible (+ base (* scale (- red lo))))
          (* visible (+ base (* scale (- green lo))))
          (* visible (+ base (* scale (- blue lo))))]))

(defn star
  "The 2n vertices of an n-pointed star about `c`, points at radius `rout`,
   notches at `rin`, the first point at angle `turn`."
  [c n rout rin turn]
  (vec (for [k (range (* 2 n))
             :let [a (+ turn (/ (* Math/PI k) n))
                   r (if (even? k) rout rin)]]
         (g/v+ c [(* r (Math/cos a)) (* r (Math/sin a))]))))

(defn tilt
  "Point [x y] of the plane seen tilted away from the viewer: depth grows
   with y by `k`; returns [[x' y'] scale]."
  [[x y] k]
  (let [d (+ 1.0 (* k y))]
    [[(/ x d) (/ y d)] (/ 1.0 d)]))

(defn outlines
  "[polygon colour bead-spacing] of the cell centred at `c`. Nothing turns:
   the loop moves by light and marching beads."
  [c star? _u]
  (let [o (/ Math/PI 8)
        {:keys [blue red ice]} colors]
    (if star?
      [[(star c 8 1.32 0.98 o) blue 0.075]
       [(star c 8 1.12 0.84 o) red 0.07]
       [(star c 8 0.62 0.47 o) blue 0.06]
       [(g/regular-polygon 24 c 0.30 0.0) red 0.05]
       [(g/regular-polygon 12 c 0.14 0.0) ice 0.045]]
      [[(g/regular-polygon 8 c 0.92 o) red 0.075]
       [(g/regular-polygon 8 c 0.72 o) blue 0.07]
       [(g/regular-polygon 24 c 0.36 0.0) ice 0.055]])))

(defn frame
  "The bead frame at phase `u`: cells i0 <= i < i1 across, beads kept
   within |x| < xmax (wider for a wider frame)."
  [u & [{:keys [i0 i1 xmax] :or {i0 -5 i1 7 xmax 8.5}}]]
  (let [shift (g/v* (* u cell) [1.0 1.0])
        k 0.07]
    {:background "#000000"
     :shapes (vec (for [i (range i0 i1) j (range -4 7)
                        :let [c (g/v- [(* i cell) (* j cell)] shift)
                              glow (+ 0.42 (* 0.58 (Math/pow (lp/wave (lp/frac (- u (/ (g/vlen c) 8.0)))) 2.0)))]
                        [pts col spacing] (outlines c (even? (+ i j)) u)
                        p (g/beads pts spacing u true)
                        :let [[[x y] s] (tilt p k)]
                        :when (and (< (- xmax) x xmax) (< -5.0 y 5.0))]
                    {:node :dot :at [x y] :opacity (min 1.0 glow) :opts {:radius (* 0.024 s)}
                     :fill {:color col :opacity 1.0}}))}))

(def neon-khatam
  {:id :neon-khatam
   :title "Neon khatam: bead outlines, bloom, red/blue fringe, tilt-shift"
   :seconds 16 :fps 30 :size [1920 1080]
   :frame frame
   :post (fn [_] [{:type :bloom :radius 14 :strength 1.3}
                  {:type :chroma :shift 4}
                  {:type :dof :focus 0.55 :band 0.14 :radius 9 :fade 0.3}
                  {:type :vignette :strength 0.7}])})

;; ── Neon pulse: closer, denser, beating ───────────────────────────────────

(defn beat
  "Brightness 0..1 at phase `u`: `n` beats per loop, each a short smooth
   swell (the first 6% of the beat) and a slow decay back to it, with one
   deep dip centred at `dip`. Continuous in u, the wrap included."
  ^double [^double u n dip]
  (let [ph (lp/frac (* (double n) u))
        rise 0.06
        floor 0.45
        low (+ floor (* (- 1.0 floor) (Math/exp (* -5.0 (- 1.0 rise)))))
        env (if (< ph rise)
              (+ low (* (- 1.0 low) (lp/smoothstep (/ ph rise))))
              (+ floor (* (- 1.0 floor) (Math/exp (* -5.0 (- ph rise))))))
        d (lp/window u (- dip 0.05) (+ dip 0.05))]
    (* env (- 1.0 (* 0.92 d)))))

(defn pulse-outlines
  "[polygon colour bead-spacing weight] of the cell centred at `c` at phase
   `u`; `k` staggers the breathing of the inner rosettes between cells.
   Breathing runs twice per loop; nothing turns."
  [c star? u k]
  (let [o (/ Math/PI 8)
        br (+ 1.0 (* 0.12 (Math/sin (* lp/tau (+ (* 2 u) (* 0.07 k))))))
        {:keys [blue red ice]} colors]
    (if star?
      [[(star c 8 1.62 1.18 o) blue 0.085 1.0]
       [(star c 8 1.48 1.08 o) red 0.08 0.9]
       [(star c 8 (* br 0.98) (* br 0.72) o) blue 0.07 0.8]
       [(star c 8 (* br 0.52) (* br 0.40) o) red 0.06 1.0]
       [(g/regular-polygon 24 c (* br 0.30) 0.0) blue 0.05 0.9]
       [(g/regular-polygon 12 c (* br 0.15) 0.0) ice 0.045 0.8]]
      [[(g/regular-polygon 8 c 1.05 o) red 0.085 1.0]
       [(g/regular-polygon 8 c 0.92 o) blue 0.08 0.9]
       [(g/regular-polygon 8 c 0.78 o) red 0.075 0.6]
       [(g/regular-polygon 24 c (* br 0.46) 0.0) blue 0.06 1.0]
       [(g/regular-polygon 24 c (* br 0.32) 0.0) ice 0.05 0.8]])))

(defn pulse-frame
  "The neon-pulse bead frame at phase `u`: the plane drifts one cell, the
   camera dollies in and out once, light beats four times with one dip.
   Cells i0 <= i < i1 across, beads kept within |x| < xmax."
  [u & [{:keys [i0 i1 xmax] :or {i0 -3 i1 5 xmax 4.3}}]]
  (let [shift (g/v* (* u cell) [1.0 1.0])
        zoom (+ 1.0 (* 0.06 (Math/sin (* lp/tau u))))
        k 0.12
        b (beat u 4 0.62)
        place (fn [p] (tilt p k))]
    {:background "#000000"
     :shapes (vec (concat
                   (for [i (range i0 i1) j (range -3 5)
                         :let [c (g/v* zoom (g/v- [(* i cell) (* j cell)] shift))
                               ring (Math/pow (lp/wave (lp/frac (- u (/ (g/vlen c) 6.0)))) 2.0)
                               glow (* b (+ 0.7 (* 0.3 ring)))]
                         ;; Stagger by i - j. Over one loop the drift carries
                         ;; cell (i, j) to where (i-1, j-1) began, and that keeps
                         ;; i - j. With i + j the stagger moved by 2 (0.14 turn
                         ;; of breathing), so the rosettes jumped at the wrap.
                         [pts col spacing w] (pulse-outlines c (even? (+ i j)) u (- i j))
                         p (g/beads pts (* zoom spacing) u true)
                         :let [[[x y] s] (place p)]
                         :when (and (< (- xmax) x xmax) (< -2.5 y 2.5))]
                     {:node :dot :at [x y] :opacity (min 1.0 (* w glow 1.15)) :opts {:radius (* zoom 0.03 s)}
                      :fill {:color col :opacity 1.0}})
                   (for [i (range i0 i1) j (range -3 5)
                         dy [-0.12 0.0 0.12]
                         :let [c (g/v* zoom (g/v- [(* (+ i 0.5) cell) (* (+ j 0.5) cell)] shift))
                               [[x y] s] (place (g/v+ c [0.0 dy]))]
                         :when (and (< (- xmax) x xmax) (< -2.5 y 2.5))]
                     {:node :dot :at [x y] :opacity (* 0.9 b) :opts {:radius (* 0.03 s)}
                      :fill {:color (:blue colors) :opacity 1.0}})))}))

(def neon-pulse
  {:id :neon-pulse
   :title "Neon pulse: beating light, breathing rosettes, dolly and rack focus"
   :seconds 16 :fps 30 :size [1920 1080] :view [(/ 64.0 9.0) 4.0]
   :frame pulse-frame
   :post (fn [u]
           (let [b (beat u 4 0.62)]
             [{:type :bloom :radius 18 :strength (+ 0.9 (* 1.1 b))}
              {:type :chroma :shift 5}
              {:type :dof :focus (+ 0.5 (* 0.14 (Math/sin (* lp/tau u))))
               :band (+ 0.08 (* 0.06 (lp/wave u))) :radius 12 :fade 0.32}
              {:type :vignette :strength 0.65}]))})
