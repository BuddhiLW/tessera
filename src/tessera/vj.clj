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
  "A cylindrical perspective tunnel tiled by double-outline stepped crosses.
   Smaller diamonds, chevrons, interior hatching and bead chains nest within
   the tiles. Inverse-radius depth supplies the scale gradient; Beer-Lambert
   attenuation makes the vanishing point black. All geometry stays fixed."
  [u]
  (let [u (double u)]
    (fn ^long [^double x ^double y]
      (let [radius (Math/hypot x y)]
        (if (< radius 0.23)
          0
          (let [z (/ 3.0 radius) t (- z u)
                s (+ (* 12.0 (/ (Math/atan2 y x) lp/tau))
                     (* 0.12 (Math/sin (* lp/tau t))))
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
                bead-x (- ax 0.445) bead-y (- by 0.445)
                bead (Math/exp (* -1800.0 (+ (* bead-x bead-x) (* bead-y bead-y))))
                bead-ring (glow (- (Math/hypot bead-x bead-y) 0.039) 0.006)
                pulse (+ 0.70 (* 0.30 (lp/wave (+ t (* 0.2 (Math/cos (* lp/tau s)))))))
                fade (* pulse (Math/exp (* -0.047 z z)))
                cyan (* fade (+ outer (* 0.30 bead-ring)))
                magenta (* fade (+ inner (* 0.7 chevron)))
                gold (* fade (+ diamond hatch (* 0.75 bead)))]
            (neon (+ (* 0.035 cyan) (* 0.92 magenta) (* 0.65 gold))
                  (+ (* 0.86 cyan) (* 0.025 magenta) (* 0.98 gold))
                  (+ cyan (* 0.95 magenta) (* 0.025 gold)))))))))

(def textile-tunnel
  {:id :textile-tunnel
   :title "UV textile tunnel: exact cylindrical rays and periodic stepped crosses"
   :seconds 18 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
   :field tunnel-shader})

(def loops
  "Specs exported for the coordinator's gallery registry."
  (into (array-map) (map (juxt :id identity)) [textile-tunnel]))
