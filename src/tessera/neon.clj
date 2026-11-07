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
  "[polygon colour bead-spacing] of the cell centred at `c` at phase `u`."
  [c star? u]
  (let [t (* u (/ Math/PI 4))
        o (/ Math/PI 8)
        {:keys [blue red ice]} colors]
    (if star?
      [[(star c 8 1.32 0.98 o) blue 0.075]
       [(star c 8 1.12 0.84 o) red 0.07]
       [(star c 8 0.62 0.47 (+ o t)) blue 0.06]
       [(g/regular-polygon 24 c 0.30 (- t)) red 0.05]
       [(g/regular-polygon 12 c 0.14 (* 2 t)) ice 0.045]]
      [[(g/regular-polygon 8 c 0.92 o) red 0.075]
       [(g/regular-polygon 8 c 0.72 o) blue 0.07]
       [(g/regular-polygon 24 c 0.36 t) ice 0.055]])))

(defn frame
  "The bead frame at phase `u`."
  [u]
  (let [shift (g/v* (* u cell) [1.0 1.0])
        k 0.07]
    {:background "#000000"
     :shapes (vec (for [i (range -5 7) j (range -4 7)
                        :let [c (g/v- [(* i cell) (* j cell)] shift)
                              glow (+ 0.42 (* 0.58 (Math/pow (lp/wave (lp/frac (- u (/ (g/vlen c) 8.0)))) 2.0)))]
                        [pts col spacing] (outlines c (even? (+ i j)) u)
                        p (g/beads pts spacing u true)
                        :let [[[x y] s] (tilt p k)]
                        :when (and (< -8.5 x 8.5) (< -5.0 y 5.0))]
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
