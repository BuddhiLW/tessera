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

(defn dihedral-fold
  "Canonical point under D_n, reflection mirrors separated by pi/n.
   The result has polar angle in [0,pi/n], preserving radial distance.
   This is a stationary mirror quotient, not an animated rotation."
  [n x y]
  (let [n (long n) x (double x) y (double y)]
    (when (< n 2) (throw (ex-info "D_n needs n>=2" {:n n})))
    (let [half (/ Math/PI n) sector (* 2.0 half)
          angle (Math/abs (- (mod (+ (Math/atan2 y x) half) sector) half))
          radius (Math/hypot x y)]
      [(* radius (Math/cos angle)) (* radius (Math/sin angle))])))

(defn kaleido-shader
  "D8 five-wave interference through rho=3.2 log(1+2r).
   Three contour levels are bold cyan/magenta; others are thin and dim.
   The A2 net is secondary electric blue. Priority masks keep lime hatching,
   bright gold beads and orange-red eyes visibly distinct at intersections.
   Stationary petalled medallion is brightest; edges gently fall off."
  [u]
  (let [u (double u) phase (* lp/tau u)
        dirs (double-array (mapcat (fn [j]
                                    [(Math/cos (* lp/tau (/ j 5.0)))
                                     (Math/sin (* lp/tau (/ j 5.0)))]) (range 5)))
        pulse (+ 0.85 (* 0.15 (lp/wave (* 2.0 u))))]
    (fn ^long [^double x ^double y]
      (let [radius (Math/hypot x y)
            half (/ Math/PI 8.0)
            angle (Math/abs (- (mod (+ (Math/atan2 y x) half) (* 2.0 half)) half))
            rho (* 3.2 (Math/log1p (* 2.0 radius)))
            px (* rho (Math/cos angle)) py (* rho (Math/sin angle))
            field (loop [j 0 sum 0.0]
                    (if (< j 5)
                      (recur (inc j)
                             (+ sum (Math/cos (+ (* 2.6 (+ (* px (aget dirs (* 2 j)))
                                                            (* py (aget dirs (inc (* 2 j)))))) phase))))
                      (* 0.5 sum)))
            level (* 1.4 field)
            primary (<= -1.0 (Math/floor level) 1.0)
            width (if primary 0.052 0.020)
            weight (if primary 1.18 0.30)
            contour (- (lp/frac level) 0.5)
            cyan-line (* weight (glow (- contour 0.085) width))
            magenta-line (* weight (glow (+ contour 0.085) width))
            g1 (* 0.78 px) g2 (* 0.78 (+ (* 0.5 px) (* 0.866025403784 py)))
            g3 (- g1 g2)
            d1 (Math/abs (- (lp/frac g1) 0.5))
            d2 (Math/abs (- (lp/frac g2) 0.5))
            d3 (Math/abs (- (lp/frac g3) 0.5))
            grid-distance (min d1 d2 d3)
            grid (+ (glow (- grid-distance 0.030) 0.010)
                    (* 0.65 (glow (- grid-distance 0.065) 0.008)))
            meander (glow (- (lp/frac (+ (* 3.0 d1) (* 3.0 d2))) 0.5) 0.080)
            hatch (* meander (min 1.0 (* 7.0 grid-distance)))
            bx (- (lp/frac (* 3.0 g1)) 0.5)
            beads (* (glow (- d2 0.026) 0.032) (Math/exp (* -36.0 bx bx)))
            ex (- (lp/frac g1) 0.5) ey (- (lp/frac g2) 0.5)
            eyes (Math/exp (* -850.0 (+ (* ex ex) (* ey ey))))
            eye-rim (* 0.5 (glow (- (+ (Math/abs ex) (* 1.7 (Math/abs ey))) 0.10) 0.016))
            petal (glow (- radius (+ 0.235 (* 0.026 (Math/cos (* 16.0 angle))))) 0.012)
            centre (+ (* 1.8 petal) (* 1.2 (glow (- radius 0.148) 0.013)))
            fade (* (min 1.0 (* 5.0 radius)) (- 1.0 (* 0.065 radius)))
            mask (* (- 1.0 (* 0.85 hatch)) (- 1.0 beads) (- 1.0 eyes))
            blue (* fade mask 0.28 grid)
            cyan (* fade mask cyan-line)
            magenta (* fade mask magenta-line)
            lime (* fade pulse 0.82 hatch (- 1.0 beads) (- 1.0 eyes))
            gold (* fade 1.40 beads)
            orange (* fade (+ (* 1.5 eyes) eye-rim))]
        (neon (+ (* 0.025 blue) (* 0.02 cyan) magenta (* 0.40 lime) gold orange (* 0.10 centre))
              (+ (* 0.10 blue) (* 0.88 cyan) (* 0.025 magenta) lime (* 0.61 gold) (* 0.10 orange) (* 0.85 centre))
              (+ blue cyan (* 0.93 magenta) (* 0.02 lime) (* 0.015 gold) (* 0.015 orange) centre))))))

(def kaleido-quasicrystal
  {:id :kaleido-quasicrystal
   :title "D8 UV kaleidoscope: five-wave contours through a logarithmic radial lens"
   :seconds 20 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 3
   :field kaleido-shader})

(defn graphene-amplitude
  "Honeycomb nearest-neighbour structure factor |1+exp(i k1)+exp(i k2)|.
   k1,k2 are primitive reciprocal phases; the hopping is one. The exact
   band lies in [0,3], with a Dirac zero at (2pi/3,-2pi/3)."
  ^double [^double k1 ^double k2]
  (let [re (+ 1.0 (Math/cos k1) (Math/cos k2))
        im (+ (Math/sin k1) (Math/sin k2))]
    (Math/hypot re im)))

(defn sprite-shader
  "Brightness-to-size sampling of the exact graphene band, 16 columns wide.
   Size spans .06..1.66 cells; overlapping glyphs evaluate neighbouring sites
   instead of being clipped at cell boundaries. Largest glyphs have a dim
   saturated fill. Two separate waves of wavelength 12.6 cells give coherent
   hue bands; orange-red eyes have colour priority over the glyph fill.
   The slide is one exact cell, with C2-periodic gentle pacing, slower at the
   wrap. Every site uses i-v, not i, so both sampled fields close on relabel."
  [u]
  (let [u (double u)
        v (- u (* 0.60 (/ (Math/sin (* lp/tau u)) lp/tau)))
        phase (* lp/tau v) spacing 0.50 w 24 h 16
        sizes (double-array (* w h))
        reds (double-array (* w h)) greens (double-array (* w h)) blues (double-array (* w h))]
    (dotimes [row h]
      (dotimes [col w]
        (let [sx (- col 12.0 v) sy (- row 8.0)
              energy (graphene-amplitude (+ (* 0.62 sx) (* 0.23 sy) phase)
                                         (- (* 0.62 sx) (* 0.72 sy) phase))
              size (+ 0.06 (* 1.60 (Math/pow (/ energy 3.0) 1.25)))
              hue (+ 0.50 (* 0.33 (Math/sin (+ (* 0.46 sx) (* 0.20 sy) phase)))
                     (* 0.10 (Math/cos (- (* 0.30 sx) (* 0.40 sy) phase))))
              o (+ col (* row w))]
          (aset sizes o size)
          (aset reds o (+ 0.5 (* 0.5 (Math/cos (* lp/tau hue)))))
          (aset greens o (+ 0.5 (* 0.5 (Math/cos (* lp/tau (+ hue 0.333333333333))))))
          (aset blues o (+ 0.5 (* 0.5 (Math/cos (* lp/tau (+ hue 0.666666666667)))))))))
    (fn ^long [^double x ^double y]
      (let [gx (+ (/ x spacing) v) gy (/ y spacing)
            i (long (Math/floor gx)) j (long (Math/floor gy))
            a (- (lp/frac gx) 0.5) b (- (lp/frac gy) 0.5)]
        (loop [k 0 red 0.0 green 0.0 blue 0.0]
          (if (< k 9)
            (let [di (- (rem k 3) 1) dj (- (quot k 3) 1)
                  dx (- a di) dy (- b dj)
                  col (+ i di 12) row (+ j dj 8)]
              (if (or (> (Math/abs dx) 0.73) (> (Math/abs dy) 0.73)
                      (< col 0) (>= col w) (< row 0) (>= row h))
                (recur (inc k) red green blue)
                (let [o (+ col (* row w)) size (aget sizes o)
                      aa (/ dx size) bb (/ dy size)
                      ax (Math/abs aa) by (Math/abs bb)
                      kind (long (mod (+ j dj) 3))
                      raw (case kind
                            0 (stepped-cross aa bb)
                            1 (* 0.707106781187 (- (+ ax by) 0.43))
                            2 (max (- ax 0.39) (- (Math/abs (- by (* 0.68 ax))) 0.12)))
                      d (* size raw)
                      outline (glow (- d 0.009) 0.017)
                      inline (glow (+ d 0.045) 0.012)
                      fill (* 0.20 (Math/pow (/ size 1.66) 0.7)
                              (max 0.0 (min 1.0 (/ (- d) 0.035))))
                      nested (glow (* size (- (+ ax by) 0.125)) 0.012)
                      hatch (if (< raw -0.08)
                              (* 0.22 (glow (- (lp/frac (* 4.0 (+ ax by))) 0.5) 0.080))
                              0.0)
                      eye (Math/exp (* -1300.0 (+ (* aa aa) (* bb bb))))
                      mask (- 1.0 (* 0.95 eye))
                      r (aget reds o) g (aget greens o) b (aget blues o)
                      main (* mask (+ (* 1.30 outline) fill))
                      inner (* mask 0.72 inline)
                      lime (* mask 0.43 (+ nested hatch))]
                  (recur (inc k)
                         (+ red (* r main) (* b inner) (* 0.38 lime) (* 0.95 eye))
                         (+ green (* g main) (* r inner) lime (* 0.095 eye))
                         (+ blue (* b main) (* g inner) (* 0.02 lime) (* 0.014 eye))))))
            (neon red green blue)))))))

(def sprite-lattice
  {:id :sprite-lattice
   :title "UV sprite lattice: glyph sizes from the exact graphene hopping band"
   :seconds 18 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 3
   :field sprite-shader})

(def loops
  "Specs exported for the coordinator's gallery registry."
  (into (array-map) (map (juxt :id identity))
        [textile-tunnel kaleido-quasicrystal sprite-lattice]))
