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

(def platform-sites
  "Thirty separated relief clusters, deliberately leaving true-black air."
  (poisson-sites 8192 30 0.99))

(defn- spatial-bins
  "Coarse finite-support index; no all-to-all SDF evaluation per pixel."
  [^doubles data stride radius]
  (let [bins (object-array 600) stride (long stride) radius (double radius)]
    (dotimes [i (quot (alength data) stride)]
      (let [o (* i stride) x (aget data o) y (aget data (inc o))
            r0 (max 0 (long (Math/floor (* 2.0 (- (+ y 3.0) radius)))))
            r1 (min 20 (inc (long (Math/floor (* 2.0 (+ y 3.0 radius))))))
            c0 (max 0 (long (Math/floor (* 2.0 (- (+ x 5.0) radius)))))
            c1 (min 30 (inc (long (Math/floor (* 2.0 (+ x 5.0 radius))))))]
        (doseq [row (range r0 r1) col (range c0 c1)]
          (let [b (+ col (* row 30))] (aset bins b (conj (aget bins b) i))))))
    (dotimes [i (alength bins)] (aset bins i (int-array (aget bins i))))
    bins))

(defn fabric-blocks
  "Discrete three-tier isometric platforms on BLACK, not a tiled backdrop.
   Each prism has a bright ornamented diamond top, dark violet west face and
   darker blue east face. Ornament is masked to faces; nothing fills the air."
  [u]
  (let [phase (* lp/tau (double u))
        data (double-array (mapcat (fn [i [x y]]
                                    [(double x) (double y)
                                     (+ 0.78 (* 0.48 (lp/frac (* 0.61803398875 i))))])
                                  (range) (sort-by second platform-sites)))
        ^objects bins (spatial-bins data 3 1.05)]
    (fn ^long [^double x ^double y]
      (let [col (long (max 0.0 (min 29.0 (Math/floor (* 2.0 (+ x 5.0))))))
            row (long (max 0.0 (min 19.0 (Math/floor (* 2.0 (+ y 3.0))))))
            ^ints ids (aget bins (+ col (* row 30)))]
        (loop [j 0 color 0]
          (if (= j (alength ids)) color
              (let [i (aget ids j) o (* 3 i) scale (aget data (+ o 2))
                    dx (/ (- x (aget data o)) scale) dy (/ (- y (aget data (inc o))) scale)
                    light (+ 0.77 (* 0.20 (Math/sin (+ phase (* 1.7 i)))))]
                (recur (inc j)
                       (loop [tier 0 c color]
                         (if (= tier 3) c
                             (let [s (- 0.39 (* tier 0.105)) elevation (+ -0.12 (* tier 0.16))
                                   yy (- dy elevation) a (+ (* 0.57735026919 dx) yy)
                                   b (+ (* -0.57735026919 dx) yy)
                                   top (* 0.5 (- (max (Math/abs a) (Math/abs b)) s))
                                   lower (+ (- s) (* 0.57735026919 (Math/abs dx)))
                                   side (max (* 0.5 (- (Math/abs dx) (* 1.73205080757 s)))
                                             (- (Math/abs (+ (- yy lower) 0.080)) 0.080))
                                   raw (vj/stepped-cross (/ (* 0.37 a) s) (/ (* 0.37 b) s))
                                   outline (filament raw 0.014)
                                   inline (filament (+ raw 0.060) 0.010)
                                   hatch (if (< raw -0.075)
                                           (filament (- (lp/frac (* 5.0 (+ (Math/abs (/ a s)) (Math/abs (/ b s))))) 0.5) 0.060) 0.0)
                                   border (filament top 0.008)
                                   west? (neg? dx)
                                   flutes (filament (- (lp/frac (+ (* 14.0 dx) (* 8.0 yy))) 0.5) 0.10)
                                   side-color (if west?
                                                (r/rgb (* light (+ 0.16 (* 0.14 flutes))) 0.012 (* light (+ 0.42 (* 0.16 flutes))))
                                                (r/rgb 0.008 (* light (+ 0.035 (* 0.10 flutes))) (* light (+ 0.16 (* 0.18 flutes)))))
                                   top-color (uv (+ 0.07 (* 0.55 inline) (* 0.018 outline))
                                                 (* light (+ 0.10 (* 0.92 outline) (* 0.20 hatch) (* 0.50 border)))
                                                 (* light (+ 0.40 outline (* 0.78 inline) (* 0.44 hatch) (* 0.74 border))))
                                   c (blend c side-color (cover side))]
                               (recur (inc tier) (blend c top-color (cover top))))))))))))))

(defn ribbon-tables
  "Finite diagonal serpents with two integer spatial harmonics; each stores
   a normal estimate and integrated arclength for one-period scale advection."
  []
  (mapv (fn [k [cx cy angle length kind]]
          (let [n 1537 ys (double-array n) normals (double-array n) arcs (double-array n)
                phase (* 1.21 k) omega (/ lp/tau (* 2.0 length))]
            (dotimes [j n]
              (let [x (* length (- (* 2.0 (/ j (double (dec n)))) 1.0))
                    t (+ (* omega x) phase)
                    y (+ (* 0.40 (Math/sin (* 2.0 t))) (* 0.18 (Math/sin (+ (* 3.0 t) k))))
                    slope (* omega (+ (* 0.80 (Math/cos (* 2.0 t))) (* 0.54 (Math/cos (+ (* 3.0 t) k)))))
                    normal (/ 1.0 (Math/sqrt (+ 1.0 (* slope slope))))]
                (aset ys j y) (aset normals j normal)
                (when (pos? j)
                  (aset arcs j (+ (aget arcs (dec j)) (/ (* 2.0 length) (* (dec n) normal)))))))
            [(double cx) (double cy) (Math/cos angle) (Math/sin angle)
             (double length) (long kind) ys normals arcs]))
        (range) [[-1.4 0.30 0.52 2.7 0] [1.1 -0.10 -0.67 2.9 1]
                 [-0.6 -1.15 0.12 2.8 2] [0.8 1.30 0.35 2.3 1]]))

(def ^:private ribbons (ribbon-tables))

(defn- ribbon-layer
  [u]
  (let [v (travel-phase (double u))]
    (fn ^long [^double x ^double y ^long base]
      (loop [k 0 color base]
        (if (= k (count ribbons)) color
            (let [[cx cy ca sa length kind ^doubles ys ^doubles normals ^doubles arcs] (nth ribbons k)
                  dx (- x (double cx)) dy (- y (double cy))
                  xx (+ (* dx (double ca)) (* dy (double sa)))
                  yy (- (* dy (double ca)) (* dx (double sa)))
                  length (double length) kind (long kind)]
              (if (> (Math/abs xx) length) (recur (inc k) color)
                  (let [index (long (* 1536.0 (/ (+ xx length) (* 2.0 length))))
                        normal (* (- yy (aget ys index)) (aget normals index))
                        taper (Math/pow (max 0.0 (- 1.0 (Math/pow (/ xx length) 2.0))) 0.55)
                        radius (* (+ 0.085 (* 0.055 (lp/wave (+ (* 0.45 xx) (* k 0.17))))) taper)
                        d (- (Math/abs normal) radius)]
                    (if (or (< radius 0.001) (> d 0.045)) (recur (inc k) color)
                        (let [s (- (/ (aget arcs index) 0.20) v) across (/ normal radius)
                              shade (+ 0.28 (* 0.70 (Math/sqrt (max 0.0 (- 1.0 (* across across))))))
                              stripe (cover (* 0.10 (Math/sin (* lp/tau s))))
                              scale (filament (- (lp/frac (+ s (* 0.70 (Math/abs across)))) 0.5) 0.13)
                              bead (Math/sqrt (+ (Math/pow (* 1.5 (- (lp/frac s) 0.5)) 2.0) (* across across)))
                              bead-d (* radius (- bead 0.97))
                              rim (filament d 0.007)
                              core (case kind
                                     0 (r/rgb (* shade (+ 0.012 (* 0.88 stripe)))
                                              (* shade (+ 0.015 (* 0.90 stripe)))
                                              (* shade (+ 0.028 (* 0.91 stripe))))
                                     1 (uv (* 0.018 scale) (* shade (+ 0.08 (* 0.87 scale)))
                                           (* shade (+ 0.21 (* 0.80 scale))))
                                     2 (r/rgb (* shade 0.94) (* shade (+ 0.06 (* 0.29 (Math/pow (lp/wave s) 4.0)))) 0.018))
                              core (if (= kind 2) core (blend core (r/rgb 0.06 0.67 0.94) (* 0.65 rim)))
                              d (if (= kind 2) (max d bead-d) d)]
                          (recur (inc k) (blend color core (cover d)))))))))))))

(defn ornament-data
  "Large rainbow droplets in three-headed bouquets, concentric eyes, warm
   four-point stars and bead trails. All bobbing frequencies are integers."
  [u]
  (let [u (double u)
        ornaments (mapcat (fn [i [x y]]
                           (let [kind (mod i 4) phase (* 2.399963229728653 i)
                                 radius (+ 0.080 (* 0.095 (lp/frac (* i 0.61803398875))))
                                 bob (* 0.045 (Math/sin (+ (* lp/tau (* (inc (mod i 3)) u)) phase)))
                                 pulse (+ 0.84 (* 0.14 (lp/wave (+ (* 2.0 u) (/ phase lp/tau)))))
                                 base [(double x) (+ (double y) bob) radius (double kind) phase pulse]]
                             (if (zero? kind)
                               [base [(+ (double x) 0.21) (+ (double y) bob -0.17) (* 0.72 radius) 0.0 (+ phase 0.6) pulse]
                                [(- (double x) 0.19) (+ (double y) bob -0.12) (* 0.62 radius) 0.0 (- phase 0.6) pulse]]
                               [base])))
                         (range) fabric-sites)]
    (double-array (mapcat identity ornaments))))

(defn- ornament-bins [^doubles data]
  (spatial-bins data 6 0.50))

(defn- rainbow
  "Full HSV rainbow, red at zero through yellow, green, cyan to blue at 2/3.
   Used radially rather than time-cycled; no discontinuous endpoint hue."
  ^long [^double hue ^double value]
  (let [t (* 6.0 (lp/frac hue))
        channel (fn ^double [^double offset]
                  (* value (max 0.0 (min 1.0 (- (Math/abs (- (mod (+ t offset) 6.0) 3.0)) 1.0)))))]
    (r/rgb (channel 0.0) (channel 4.0) (channel 2.0))))

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
                  0 (let [stem-d (max (- (Math/abs (+ dx (* 0.032 (Math/sin (* 9.0 dy))))) 0.006)
                                     (- (Math/abs (+ dy 0.22)) 0.23))
                          stem (blend color (r/rgb 0.08 0.81 0.26) (cover stem-d))
                          width (* radius (+ 0.93 (* -0.18 (max -1.0 (min 1.0 (/ dy radius))))))
                          qx (/ dx width) qy (/ dy (* radius 1.12))
                          q (Math/hypot qx qy) d (* radius (- q 1.0))
                          z (Math/sqrt (max 0.0 (- 1.0 (* q q))))
                          hue (* 0.666666666667 (max 0.0 (- 1.0 (min 1.0 q))))
                          shade (* pulse (+ 0.48 (* 0.46 z)))
                          highlight (Math/exp (* -85.0 (+ (Math/pow (+ qx 0.26) 2.0) (Math/pow (- qy 0.32) 2.0))))
                          drop (blend (rainbow hue shade) (r/rgb 0.94 0.94 0.94) highlight)]
                      (recur (inc j) (blend stem drop (cover d))))
                  1 (let [ex (/ dx (* 1.55 radius)) ey (/ dy (* 0.84 radius))
                          angle (Math/atan2 ey ex)
                          almond (+ (Math/abs ey) (* ex ex))
                          saw (* 0.24 (max 0.0 (- 1.0 (* 2.0 (lp/frac (* 14.0 (/ angle lp/tau)))))))
                          d (* radius (- almond (+ 1.0 saw)))
                          iris (/ rr radius) ring (Math/pow (lp/wave (* 2.4 iris)) 0.45)
                          pupil (cover (* radius (- iris 0.22)))
                          rim (filament (* radius (- almond 0.92)) 0.010)
                          eye (r/rgb (* pulse 0.94) (* pulse (+ 0.045 (* 0.60 ring))) 0.008)
                          eye (blend eye (r/rgb 0.94 0.71 0.025) (* 0.75 rim))
                          eye (blend eye 0 pupil)]
                      (recur (inc j) (blend color eye (cover d))))
                  2 (let [xx (/ (Math/abs dx) radius) yy (/ (Math/abs dy) radius)
                          d (* radius (- (+ (Math/sqrt xx) (Math/sqrt yy)) 1.0))
                          rim (filament d 0.008)
                          star (r/rgb (* pulse 0.94) (* pulse (+ 0.52 (* 0.32 rim))) 0.012)]
                      (recur (inc j) (blend color star (min 1.0 (+ (cover d) (* 0.24 rim))))))
                  3 (let [path (- dy (* 0.065 (Math/sin (+ (* 15.0 dx) phase))))
                          along (- (lp/frac (+ (* 17.0 dx) (travel-phase (double u)))) 0.5)
                          d (max (- (Math/hypot (* 0.058 along) path) 0.017) (- (Math/abs dx) 0.21))
                          chain (r/rgb (* 0.94 pulse) (* 0.67 pulse) 0.015)]
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

(defn hyperspace-seeds
  "A2 (hex) lattice sites in a fundamental D12 wedge and one dyadic annulus.
   Repetition by radius factors 2^k creates a discrete self-similar field."
  []
  (vec (for [i (range -20 21) j (range -20 21)
             :let [x (* 0.14 (+ i (* 0.5 j)))
                   y (* 0.14 0.866025403784 j)
                   radius (Math/hypot x y) angle (Math/atan2 y x)]
             :when (and (<= 1.0 radius) (< radius 2.0) (< 0.012 angle (/ Math/PI 12.0)))]
         {:x x :y y :q (/ (Math/log radius) (Math/log 2.0)) :angle angle})))

(defn hyperspace-coordinates
  "Stationary D12 fold followed by log2 radius. Advecting q by one moves
   every star outward at dr/du=log(2)*r; u+1 relabels the dyadic annuli."
  [x y u]
  (let [[a b] (vj/dihedral-fold 12 x y) radius (Math/hypot (double x) (double y))]
    [(- (/ (Math/log (max 1.0e-12 (/ radius 0.38))) (Math/log 2.0)) (travel-phase (double u)))
     (Math/atan2 (double b) (double a))]))

(defn hyperspace-shader
  "D12 hex-lattice warp with exactly three chromatic stops. Angular width
   and log-radius tail length are constant, so screen-space streaks grow in
   direct proportion to radius. Long tapered tails read as warp, not dots.
   A soft violet/cyan core replaces unresolved microscopic stars."
  [u]
  (let [v (travel-phase (double u)) half (/ Math/PI 12.0)
        seeds (hyperspace-seeds) n (count seeds) data (double-array (* 6 n))
        stops [[0.015 0.90 1.0] [0.62 0.015 1.0] [1.0 0.25 0.012]]]
    (doseq [[i {:keys [q angle]}] (map-indexed vector seeds)]
      (let [color (nth stops (mod i 3)) o (* 6 i)]
        (aset data o (double q)) (aset data (inc o) (double angle))
        (aset data (+ o 2) (+ 0.0050 (* 0.0030 (lp/frac (* i 0.61803398875)))))
        (dotimes [k 3] (aset data (+ o 3 k) (double (nth color k))))))
    (fn ^long [^double x ^double y]
      (let [radius (Math/hypot x y)
            angle (Math/abs (- (mod (+ (Math/atan2 y x) half) (* 2.0 half)) half))
            q (- (/ (Math/log (max 1.0e-12 (/ radius 0.38))) (Math/log 2.0)) v)
            fade (let [t (max 0.0 (min 1.0 (/ (- radius 0.20) 0.42)))]
                   (* t t (- 3.0 (* 2.0 t)) (- 1.0 (* 0.045 radius))))
            core (* 0.15 (Math/exp (* -24.0 radius radius)))
            icy (* 0.065 (Math/exp (* -90.0 radius radius)))]
        (loop [i 0 red 0.0 green 0.0 blue 0.0]
          (if (= i n)
            (uv (+ (* fade red) (* 0.45 core) (* 0.015 icy))
                (+ (* fade green) (* 0.025 core) (* 0.88 icy))
                (+ (* fade blue) core icy))
            (let [o (* 6 i) da (- angle (aget data (inc o))) width (aget data (+ o 2))]
              (if (> (Math/abs da) (* 4.0 width)) (recur (inc i) red green blue)
                  (let [dq (- (lp/frac (+ (- q (aget data o)) 0.5)) 0.5)
                        lateral (/ da width) across (Math/exp (- (* lateral lateral)))
                        head (* 1.50 (Math/exp (* -1600.0 dq dq)))
                        tail (if (<= -0.48 dq 0.0)
                               (* 0.94 (Math/exp (* 4.6 dq)) (min 1.0 (* 35.0 (+ dq 0.48)))) 0.0)
                        spark (* 0.40 (Math/exp (* -6000.0 dq dq))
                                 (Math/exp (* -0.10 lateral lateral)))
                        light (+ (* across (+ head tail)) spark)]
                    (recur (inc i)
                           (+ red (* light (aget data (+ o 3))))
                           (+ green (* light (aget data (+ o 4))))
                           (+ blue (* light (aget data (+ o 5))))))))))))))

(def hyperspace
  {:id :hyperspace :title "Hyperspace: D12 folded A2 stars in a dyadic log-radial warp"
   :seconds 18 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2 :field hyperspace-shader})

(defn flower-coordinates
  "An unrotated twelve-wedge log-radial lattice. One octave outward per loop;
   straight triangular facets are built in the local Cartesian wedge below."
  [x y u]
  (let [radius (Math/hypot (double x) (double y)) half (/ Math/PI 12.0)
        angle (- (mod (+ (Math/atan2 (double y) (double x)) half) (* 2.0 half)) half)]
    [(- (/ (Math/log (max 1.0e-12 (/ radius 0.48))) (Math/log 2.0)) (travel-phase (double u)))
     angle]))

(defn flower-shader
  "Straight dyadic triangular facets with magenta/blue double outlines.
   Interior ornament is continuous level-set worms plus parallel hatch
   blocks, never repeated glyphs. Around the central hole is a chain of
   eighteen overlapping dashed circles, enclosing a small RGB rosette.
   Geometry advects outward by one self-similar step; nothing rotates."
  [u]
  (let [v (travel-phase (double u)) half (/ Math/PI 12.0)
        pulse (+ 0.80 (* 0.16 (lp/wave (* 2.0 (double u)))))
        ring-centres (double-array (mapcat (fn [i]
                                            (let [a (* lp/tau (/ i 18.0))]
                                              [(* 0.34 (Math/cos a)) (* 0.34 (Math/sin a))])) (range 18)))]
    (fn ^long [^double x ^double y]
      (let [radius (Math/hypot x y) theta (Math/atan2 y x)
            wedge (long (Math/floor (/ (+ theta half) (* 2.0 half))))
            angle (- (mod (+ theta half) (* 2.0 half)) half)
            q (- (/ (Math/log (max 1.0e-12 (/ radius 0.48))) (Math/log 2.0)) v)
            octave (Math/floor q)
            fade (let [t (max 0.0 (min 1.0 (/ (- radius 0.35) 0.25)))] (* t t (- 3.0 (* 2.0 t))))
            fade (* fade (- 1.0 (* 0.035 radius)))
            color (loop [k -1 red 0.0 green 0.0 blue 0.0]
                    (if (> k 1) (uv (* fade red) (* fade green) (* fade blue))
                        (let [s (Math/pow 2.0 (- q octave k))
                              px (* s (Math/cos angle)) py (* s (Math/sin angle))
                              diagonal (/ (- (Math/abs py) (* 0.52688172043 (- px 1.0))) 1.130314)
                              d (max (- px 1.93) diagonal)
                              magenta (+ (filament (- d 0.014) 0.008)
                                         (* 0.80 (filament (+ d 0.045) 0.007)))
                              blue-line (+ (filament (- d 0.040) 0.006)
                                           (* 0.68 (filament (+ d 0.070) 0.006)))
                              direction (if (even? wedge) 1.0 -1.0)
                              a (+ (* 9.0 px) (* direction 6.0 py))
                              b (- (* 4.0 px) (* direction 13.0 py))
                              c (+ (* 15.0 px) (* direction 3.0 py))
                              field (+ (Math/sin a) (* 0.55 (Math/sin b)) (* 0.28 (Math/cos c)))
                              grad-x (+ (* 9.0 (Math/cos a)) (* 2.20 (Math/cos b)) (* -4.20 (Math/sin c)))
                              grad-y (* direction (+ (* 6.0 (Math/cos a)) (* -7.15 (Math/cos b)) (* -0.84 (Math/sin c))))
                              contour (/ (- (lp/frac (* 2.4 field)) 0.5)
                                         (max 6.0 (* 2.4 (Math/hypot grad-x grad-y))))
                              worms (filament contour 0.0038)
                              parallel (filament (/ (- (lp/frac (+ (* 14.0 px) (* direction 21.0 py))) 0.5) 25.24) 0.0035)
                              patch (lp/wave (+ (* 1.8 px) (* 2.1 py)))
                              hatch (if (< d -0.095)
                                      (* 0.76 (if (> patch 0.70) parallel worms)) 0.0)
                              gold (* hatch (+ 0.16 (* 0.70 (lp/wave (+ (* 2.0 px) py)))))
                              lime (- hatch gold)
                              nested (filament (+ d 0.14) 0.006)]
                          (recur (inc k)
                                 (+ red (* 0.98 magenta) (* 0.02 blue-line) (* 0.40 lime) gold (* 0.10 nested))
                                 (+ green (* 0.015 magenta) (* 0.12 blue-line) lime (* 0.76 gold) (* 0.38 nested))
                                 (+ blue (* 0.66 magenta) blue-line (* 0.015 hatch) (* 0.75 nested))))))
            ring (if (< 0.22 radius 0.47)
                   (loop [i 0 sum 0.0]
                     (if (= i 18) (min 1.0 sum)
                         (let [dx (- x (aget ring-centres (* 2 i))) dy (- y (aget ring-centres (inc (* 2 i))))
                               dist (Math/hypot dx dy)
                               dash (Math/pow (lp/wave (* 12.0 (/ (Math/atan2 dy dx) lp/tau))) 2.0)]
                           (recur (inc i) (+ sum (* pulse dash (filament (- dist 0.077) 0.007))))))) 0.0)
            rosette-r (+ 0.115 (* 0.022 (Math/cos (* 9.0 theta))))
            red-stroke (filament (- radius rosette-r) 0.007)
            green-stroke (filament (- radius (* 0.77 rosette-r)) 0.005)
            cyan-stroke (filament (- radius (* 0.53 rosette-r)) 0.004)
            rosette (uv (* 0.80 red-stroke) (+ (* 0.84 green-stroke) (* 0.72 cyan-stroke)) (* 0.92 cyan-stroke))
            color (if (< radius 0.18) rosette color)]
        (blend color (r/rgb 0.88 0.92 0.94) ring)))))

(def flower-mandala
  {:id :flower-mandala :title "Flower mandala: dyadic triangular labyrinth tunnel"
   :seconds 18 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2 :field flower-shader})

(def loops
  "Separate export: the coordinator merges this map into the gallery."
  (array-map :fabric fabric :hyperspace hyperspace :flower-mandala flower-mandala))
