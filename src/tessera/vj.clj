(ns tessera.vj
  "Black-ground UV ornament from periodic mathematical fields.

   Perspective and dihedral folds are coordinate maps, never a whole-frame
   rotation. Motifs are geometric constructions, not copies of ceremonial
   designs. Every advected coordinate has an integer texture period."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]))

(set! *warn-on-reflection* true)

(defn stepped-cross
  "Signed L-infinity distance to a three-step cross, centred at the origin.
   The union of three axis-aligned boxes gives twelve re-entrant corners.
   A negative value is inside; the zero set is the exact stepped outline."
  ^double [^double x ^double y]
  (let [a (Math/abs x) b (Math/abs y)]
    (min (max (- a 0.39) (- b 0.12))
         (max (- a 0.27) (- b 0.27))
         (max (- a 0.12) (- b 0.39)))))

(defn- glow
  "A thin Gaussian filament and a weaker, finite-support halo."
  ^double [^double distance ^double width]
  (let [q (/ distance width) q2 (* q q)]
    (if (> q2 36.0) 0.0
        (+ (* 0.88 (Math/exp (- q2)))
           (* 0.12 (Math/exp (* -0.16 q2)))))))

(defn- neon
  "Hue-preserving bounded exposure with at least .98 HSV saturation.
   No additive white core: all three output channels remain below .95.
   The cutoff also leaves a genuinely black ground beyond the glow."
  ^long [^double red ^double green ^double blue]
  (let [hi (max red green blue) lo (min red green blue)
        span (- hi lo)]
    (if (or (< hi 0.003) (< span 1.0e-12))
      0
      (let [v (* 0.95 (- 1.0 (Math/exp (* -1.65 hi))))
            scale (/ (* 0.98 v) span) floor (* 0.02 v)]
        (r/rgb (+ floor (* scale (- red lo)))
               (+ floor (* scale (- green lo)))
               (+ floor (* scale (- blue lo))))))))

(defn tunnel-coordinates
  "Exact pinhole ray/cylinder intersection, tube radius 1, focal length 3.
   Returns [circumferential tile coordinate, axial tile coordinate, depth].
   Twelve tiles close around the cylinder. Advecting one axial period per
   loop and a local sinusoidal shear give endless travel without spinning."
  [x y u]
  (let [x (double x) y (double y) u (double u)
        depth (/ 3.0 (max 1.0e-9 (Math/hypot x y)))
        axial (- depth u)
        angular (+ (* 12.0 (/ (Math/atan2 y x) lp/tau))
                   (* 0.12 (Math/sin (* lp/tau axial))))]
    [angular axial depth]))

(defn tunnel-shader
  "Cylindrical double-outline crosses, with ornament nested in every gap.
   Analytic signed distances and 3x3 supersampling smooth the near wall.
   Alternating lime/gold diamonds, red eyes and blue chevrons are periodic
   around twelve columns. Only the stationary dashed centre rings are pale;
   their bounded icy cores pulse twice per loop, never clipping to white."
  [u]
  (let [u (double u)
        ring-pulse (+ 0.64 (* 0.30 (lp/wave (* 2.0 u))))]
    (fn ^long [^double x ^double y]
      (let [radius (Math/hypot x y)]
        (if (< radius 0.23)
          0
          (let [theta (/ (Math/atan2 y x) lp/tau)
                z (/ 3.0 radius) t (- z u)
                s (+ (* 12.0 theta) (* 0.12 (Math/sin (* lp/tau t))))
                a (- (lp/frac s) 0.5) b (- (lp/frac t) 0.5)
                ax (Math/abs a) by (Math/abs b)
                d (stepped-cross a b)
                outer (glow (- d 0.014) 0.011)
                inner (glow (+ d 0.052) 0.009)
                diamond (glow (- (+ ax by) 0.125) 0.010)
                chevron (glow (- (+ ax by) 0.685) 0.009)
                hatch (if (< d -0.09)
                        (* 0.48 (glow (- (lp/frac (* 5.0 (+ ax by))) 0.5) 0.045))
                        0.0)
                bead-x (- ax 0.457)
                bead-y (* 0.125 (- (lp/frac (* 8.0 t)) 0.5))
                chain (Math/exp (* -4100.0 (+ (* bead-x bead-x) (* bead-y bead-y))))
                ;; Repeated angular V-lines in the interstitial black pockets.
                zig (- (lp/frac (+ (* 8.0 by) (* 3.0 ax))) 0.5)
                gap (if (> d 0.045) (glow zig 0.05) 0.0)
                eye (Math/exp (* -2400.0 (+ (* a a) (* b b))))
                eye-ring (glow (- (+ (* 1.6 ax) by) 0.045) 0.008)
                alternating (* 0.5 (- 1.0 (Math/cos (* Math/PI (Math/floor s)))))
                pulse (+ 0.70 (* 0.30 (lp/wave (+ t (* 0.2 (Math/cos (* lp/tau s)))))))
                fade (* pulse (Math/exp (* -0.047 z z)))
                cyan (* fade (+ outer (* 0.44 chain)))
                magenta (* fade (+ inner (* 0.65 chevron)))
                lime (* fade (+ (* (- 1.0 alternating) diamond) (* 0.52 hatch)))
                gold (* fade (+ (* alternating diamond) (* 0.75 chain)))
                blue (* fade 0.28 gap)
                ember (* fade (+ (* 0.95 eye) (* 0.48 eye-ring)))
                c (neon (+ (* 0.035 cyan) (* 0.92 magenta) (* 0.35 lime) gold (* 0.05 blue) ember)
                        (+ (* 0.86 cyan) (* 0.025 magenta) lime (* 0.57 gold) (* 0.15 blue) (* 0.12 ember))
                        (+ cyan (* 0.95 magenta) (* 0.025 lime) (* 0.015 gold) blue (* 0.025 ember)))
                dash (Math/pow (lp/wave (* 24.0 theta)) 3.0)
                dash2 (Math/pow (lp/wave (+ (* 36.0 theta) 0.5)) 4.0)
                ring (* ring-pulse (+ (* dash (glow (- radius 0.315) 0.008))
                                          (* 0.72 dash2 (glow (- radius 0.397) 0.006))))
                cr (/ (bit-and 255 (bit-shift-right c 16)) 255.0)
                cg (/ (bit-and 255 (bit-shift-right c 8)) 255.0)
                cb (/ (bit-and 255 c) 255.0)]
            (r/rgb (+ (* (- 1.0 ring) cr) (* ring 0.77))
                   (+ (* (- 1.0 ring) cg) (* ring 0.86))
                   (+ (* (- 1.0 ring) cb) (* ring 0.94)))))))))

(def textile-tunnel
  {:id :textile-tunnel
   :title "UV textile tunnel: exact cylindrical rays and periodic stepped crosses"
   :seconds 18 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 3
   :field tunnel-shader})

(def loops
  "Specs exported for the coordinator's gallery registry."
  (into (array-map) (map (juxt :id identity)) [textile-tunnel]))
