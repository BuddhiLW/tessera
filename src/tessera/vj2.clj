(ns tessera.vj2
  "Seeded UV assemblages and self-similar star tunnels, on genuine black.
   Screen-space SDFs only: no ray tracer and no animated coordinate rotation.
   Fabric is a geometric invention, not a reproduction of a cultural design."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [tessera.vj :as vj])
  (:import (java.util Random)))

(set! *warn-on-reflection* true)

(defn- filament
  "The approved narrow Gaussian / finite-support halo profile."
  ^double [^double distance ^double width]
  (let [q (/ distance width) q2 (* q q)]
    (if (> q2 36.0) 0.0
        (+ (* 0.88 (Math/exp (- q2)))
           (* 0.12 (Math/exp (* -0.16 q2)))))))

(defn- cover ^double [^double d]
  (max 0.0 (min 1.0 (- 0.5 (* 160.0 d)))))

(defn- blend
  "Bounded source-over, never additive white. All sources peak below .95."
  ^long [^long background ^long foreground ^double alpha]
  (let [a (max 0.0 (min 1.0 alpha)) b (- 1.0 a)]
    (bit-or
     (bit-shift-left (long (+ (* b (bit-and 255 (bit-shift-right background 16)))
                               (* a (bit-and 255 (bit-shift-right foreground 16))))) 16)
     (bit-shift-left (long (+ (* b (bit-and 255 (bit-shift-right background 8)))
                               (* a (bit-and 255 (bit-shift-right foreground 8))))) 8)
     (long (+ (* b (bit-and 255 background)) (* a (bit-and 255 foreground)))))))

(defn- uv
  "Hue-preserving neon exposure, with no white core or grey pedestal."
  ^long [^double red ^double green ^double blue]
  (let [hi (max red green blue) lo (min red green blue) span (- hi lo)]
    (if (or (< hi 0.003) (< span 1.0e-12)) 0
        (let [v (* 0.94 (- 1.0 (Math/exp (* -1.8 hi))))
              k (/ (* 0.99 v) span) floor (* 0.01 v)]
          (r/rgb (+ floor (* k (- red lo)))
                 (+ floor (* k (- green lo)))
                 (+ floor (* k (- blue lo))))))))

(defn travel-phase
  "One texture period, gently paced: C2 speed is minimal at the wrap."
  ^double [^double u]
  (- u (* 0.72 (/ (Math/sin (* lp/tau u)) lp/tau))))

(defn poisson-sites
  "Deterministic dart throwing with a strict minimum distance. Exhausted
   packing returns the accepted set, never a fallback that breaks exclusion."
  [seed target distance]
  (let [rng (Random. (long seed)) d2 (* (double distance) (double distance))]
    (loop [sites [] attempts 0]
      (if (or (= (count sites) target) (= attempts 40000)) sites
          (let [x (- (* 8.8 (.nextDouble rng)) 4.4)
                y (- (* 5.1 (.nextDouble rng)) 2.55)
                okay? (every? (fn [[a b]]
                                (let [dx (- x (double a)) dy (- y (double b))]
                                  (>= (+ (* dx dx) (* dy dy)) d2))) sites)]
            (recur (if okay? (conj sites [x y]) sites) (inc attempts)))))))

(def fabric-sites (poisson-sites 73129 100 0.46))

(defn fabric-blocks
  "Isometric stepped-cross relief. Three offset silhouettes form the side
   walls; the upper face carries cyan/violet double outlines, inset crosses,
   stepped meanders and chevrons. Small interstitial diamonds fill the gaps.
   A stationary lattice carries travelling illumination, never screen spin."
  [u]
  (let [phase (* lp/tau (double u))]
    (fn ^long [^double x ^double y]
      (let [gx (+ (/ x 0.74) (/ y 0.43)) gy (+ (/ (- x) 0.74) (/ y 0.43))
            i (Math/floor gx) j (Math/floor gy)
            a (- (lp/frac gx) 0.5) b (- (lp/frac gy) 0.5)
            h (+ 0.045 (* 0.018 (Math/sin (+ (* 1.7 i) (* 2.3 j)))))
            top (vj/stepped-cross (+ a h) (+ b h))
            middle (vj/stepped-cross a b)
            bottom (vj/stepped-cross (- a h) (- b h))
            solid (min top middle bottom)
            edge (filament (- solid 0.010) 0.012)
            upper (filament top 0.014)
            inset (filament (+ top 0.060) 0.010)
            face (* 0.20 (cover top))
            side (* 0.24 (cover solid) (- 1.0 (cover top)))
            ax (Math/abs (+ a h)) by (Math/abs (+ b h))
            hatch (if (< top -0.083)
                    (filament (- (lp/frac (+ (* 7.0 ax) (* 7.0 by))) 0.5) 0.065) 0.0)
            nested (filament (- (+ ax by) 0.115) 0.012)
            chevron (if (> solid 0.025)
                      (filament (- (lp/frac (+ (* 6.0 (Math/abs a)) (* 8.0 b))) 0.5) 0.045) 0.0)
            corners (filament (- (+ (Math/abs a) (Math/abs b)) 0.86) 0.018)
            pulse (+ 0.72 (* 0.28 (Math/sin (+ (* 0.7 i) (* 0.45 j) phase))))
            cyan (* pulse (+ upper (* 0.45 nested)))
            violet (+ (* 0.95 inset) (* 0.35 edge) (* 0.45 corners))
            blue (+ face side (* 0.36 hatch) (* 0.30 chevron))]
        (uv (+ (* 0.015 cyan) (* 0.62 violet) (* 0.02 blue))
            (+ (* 0.92 cyan) (* 0.025 violet) (* 0.14 blue))
            (+ cyan violet blue))))))

(defn ribbon-tables
  "Four periodic streamlines, densely sampled once. Each entry stores y,
   inverse normal length and integrated arclength; stripe advection therefore
   travels along the ribbon rather than sliding a screen-space overlay."
  []
  (mapv (fn [k]
          (let [n 2049 ys (double-array n) normals (double-array n) arcs (double-array n)
                cy (+ -1.65 (* k 1.1)) phase (* 1.73 k)]
            (dotimes [j n]
              (let [x (+ -5.0 (* 10.0 (/ j (double (dec n)))))
                    t (+ (* lp/tau (/ x 8.0)) phase)
                    y (+ cy (* 0.37 (Math/sin t)) (* 0.15 (Math/sin (+ (* 2.0 t) k))))
                    slope (* (/ lp/tau 8.0) (+ (* 0.37 (Math/cos t)) (* 0.30 (Math/cos (+ (* 2.0 t) k)))))
                    normal (/ 1.0 (Math/sqrt (+ 1.0 (* slope slope))))]
                (aset ys j y)
                (aset normals j normal)
                (when (pos? j)
                  (aset arcs j (+ (aget arcs (dec j)) (/ 10.0 (* (dec n) normal)))))))
            [ys normals arcs])) (range 4)))

(def ^:private ribbons (ribbon-tables))

(defn- ribbon-layer
  [u]
  (let [v (travel-phase (double u))]
    (fn ^long [^double x ^double y ^long base]
      (let [index (long (max 0.0 (min 2048.0 (* 204.8 (+ x 5.0)))))]
        (loop [k 0 color base]
          (if (= k 4) color
              (let [[^doubles ys ^doubles normals ^doubles arcs] (nth ribbons k)
                    normal (* (- y (aget ys index)) (aget normals index))
                    radius (+ 0.115 (* 0.020 (Math/sin (+ (* 1.8 x) k))))
                    d (- (Math/abs normal) radius)]
                (if (> d 0.035) (recur (inc k) color)
                    (let [s (- (/ (aget arcs index) 0.22) v)
                          across (/ normal radius)
                          stripe (if (even? k)
                                   (cover (* 0.10 (Math/sin (* lp/tau s))))
                                   (filament (- (lp/frac (+ s (* 0.65 (Math/abs across)))) 0.5) 0.12))
                          shade (+ 0.32 (* 0.68 (Math/sqrt (max 0.0 (- 1.0 (* across across))))))
                          rim (filament d 0.008)
                          core (if (even? k)
                                 (r/rgb (* shade (+ 0.025 (* 0.86 stripe)))
                                        (* shade (+ 0.025 (* 0.88 stripe)))
                                        (* shade (+ 0.045 (* 0.89 stripe))))
                                 (uv (* 0.02 stripe) (* shade (+ 0.08 (* 0.82 stripe))) (* shade (+ 0.22 (* 0.78 stripe)))))
                          outlined (blend core (r/rgb 0.13 0.69 0.94) (* 0.7 rim))]
                      (recur (inc k) (blend color outlined (cover d))))))))))))

(defn ornament-data
  "Poisson-distributed droplets, eyes, stars and bead chains. Positions use
   integer-frequency bobbing only. Flat primitive data feeds a spatial grid."
  [u]
  (let [u (double u) data (double-array (* 6 (count fabric-sites)))]
    (doseq [[i [x y]] (map-indexed vector fabric-sites)]
      (let [phase (* 2.399963229728653 i) kind (mod i 4)
            bob (* 0.055 (Math/sin (+ (* lp/tau (* (inc (mod i 3)) u)) phase)))
            o (* 6 i)]
        (aset data o (double x)) (aset data (inc o) (+ (double y) bob))
        (aset data (+ o 2) (+ 0.075 (* 0.025 (lp/frac (* i 0.61803398875)))))
        (aset data (+ o 3) (double kind)) (aset data (+ o 4) phase)
        (aset data (+ o 5) (+ 0.78 (* 0.20 (lp/wave (+ (* 2.0 u) (/ phase lp/tau))))))))
    data))

(defn- ornament-bins [^doubles data]
  (let [bins (object-array (* 30 20))]
    (dotimes [i (quot (alength data) 6)]
      (let [o (* i 6) x (aget data o) y (aget data (inc o))]
        (doseq [row (range (max 0 (long (Math/floor (* 2.0 (+ y 2.7)))))
                          (min 20 (inc (long (Math/floor (* 2.0 (+ y 3.5)))))))
                col (range (max 0 (long (Math/floor (* 2.0 (+ x 4.7)))))
                           (min 30 (inc (long (Math/floor (* 2.0 (+ x 5.3)))))))]
          (let [b (+ col (* row 30))] (aset bins b (conj (aget bins b) i))))))
    (dotimes [i (alength bins)] (aset bins i (int-array (aget bins i))))
    bins))

(defn- ornaments-layer
  [u]
  (let [^doubles data (ornament-data u) ^objects bins (ornament-bins data)]
    (fn ^long [^double x ^double y ^long base]
      (let [col (long (max 0.0 (min 29.0 (Math/floor (* 2.0 (+ x 5.0))))))
            row (long (max 0.0 (min 19.0 (Math/floor (* 2.0 (+ y 3.0))))))
            ^ints ids (aget bins (+ col (* row 30)))]
        (loop [j 0 color base]
          (if (= j (alength ids)) color
              (let [o (* 6 (aget ids j)) dx (- x (aget data o)) dy (- y (aget data (inc o)))
                    radius (aget data (+ o 2)) kind (long (aget data (+ o 3)))
                    phase (aget data (+ o 4)) pulse (aget data (+ o 5))
                    rr (Math/hypot dx dy)]
                (case kind
                  0 (let [stem-d (max (- (Math/abs (+ dx (* 0.018 (Math/sin (* 13.0 dy))))) 0.005)
                                           (- (Math/abs (+ dy 0.14)) 0.16))
                          stem (blend color (r/rgb 0.03 0.54 0.71) (cover stem-d))
                          width (* radius (+ 0.92 (* -0.22 (max -1.0 (min 1.0 (/ dy radius))))))
                          qx (/ dx width) qy (/ dy (* radius 1.25))
                          q (Math/hypot qx qy) d (* radius (- q 1.0))
                          z (Math/sqrt (max 0.0 (- 1.0 (* q q))))
                          hue (+ 0.01 (* 0.66 (min 1.0 (/ rr radius))))
                          red (+ 0.5 (* 0.5 (Math/cos (* lp/tau hue))))
                          green (+ 0.5 (* 0.5 (Math/cos (* lp/tau (+ hue 0.666666666667)))))
                          blue (+ 0.5 (* 0.5 (Math/cos (* lp/tau (+ hue 0.333333333333)))))
                          shade (* pulse (+ 0.25 (* 0.65 z)))
                          spec (* 0.56 (Math/exp (* -75.0 (+ (Math/pow (+ qx 0.30) 2.0) (Math/pow (- qy 0.35) 2.0)))))
                          drop (r/rgb (min 0.94 (+ (* shade red) spec))
                                      (min 0.94 (+ (* shade green) spec))
                                      (min 0.94 (+ (* shade blue) spec)))]
                      (recur (inc j) (blend stem drop (cover d))))
                  1 (let [ex (/ dx (* 1.40 radius)) ey (/ dy (* 0.66 radius))
                          almond (+ (Math/abs ey) (* ex ex)) d (* radius (- almond 1.0))
                          iris (Math/exp (* -6.0 (+ (* ex ex) (* ey ey))))
                          pupil (cover (- (Math/abs ex) 0.10)) rim (filament d 0.009)
                          eye (r/rgb (* pulse (+ 0.31 (* 0.60 iris))) (+ 0.025 (* 0.13 iris)) 0.018)
                          eye (blend eye 0 (* iris pupil))
                          eye (blend eye (r/rgb 0.94 0.26 0.025) (* 0.8 rim))]
                      (recur (inc j) (blend color eye (cover d))))
                  2 (let [angle (Math/atan2 dy dx)
                          boundary (* radius (+ 0.68 (* 0.28 (Math/cos (* 5.0 angle)))))
                          d (- rr boundary) rim (filament d 0.007) fill (* 0.15 (cover d))
                          star (uv (* 0.8 (+ rim fill)) (* 0.45 (+ rim fill)) (* 0.015 (+ rim fill)))]
                      (recur (inc j) (blend color star (min 1.0 (+ (* 0.6 (cover d)) rim)))))
                  3 (let [path (- dy (* 0.065 (Math/sin (+ (* 15.0 dx) phase))))
                          along (- (lp/frac (+ (* 17.0 dx) (double u))) 0.5)
                          d (max (- (Math/hypot (* 0.058 along) path) 0.014) (- (Math/abs dx) 0.21))
                          chain (r/rgb (* 0.88 pulse) (* 0.57 pulse) 0.02)]
                      (recur (inc j) (blend color chain (cover d))))))))))))

(defn fabric-shader
  "Dense blacklight assemblage: isometric relief, four scaly/striped ribbons,
   rainbow droplets on stems, ember eyes, stars and gold bead chains."
  [u]
  (let [blocks (fabric-blocks u) ribbon (ribbon-layer u) ornaments (ornaments-layer u)]
    (fn ^long [^double x ^double y]
      (ornaments x y (ribbon x y (blocks x y))))))

(def fabric
  {:id :fabric :title "UV fabric: isometric relief, serpent ribbons and rainbow droplets"
   :seconds 16 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2 :field fabric-shader})

(def loops
  "Separate export: the coordinator merges this map into the gallery."
  (array-map :fabric fabric))
