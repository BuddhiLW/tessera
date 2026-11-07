(ns tessera.reciprocal
  "Reciprocal space as wallpaper.

     :brillouin-zones  the Brillouin zones of the hexagonal lattice as stained
                       glass: zone n is where n-1 reciprocal-lattice points are
                       nearer than the origin; edges glow, a pulse runs outward
                       zone by zone, the lattice turns 60 degrees per loop
     :graphene-fermi   constant-energy contours of graphene's tight-binding
                       band |f(k)|: rings around the six Dirac points that merge
                       at the M saddles and close into a hexagon around Gamma;
                       the contours rise one spacing per loop

   Both are per-pixel fields of desargues.wallpaper.render (wavevector k is
   the scene point), coloured by thi.ng/color schemes."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [tessera.palette :as pal]))

(set! *warn-on-reflection* true)

(def ^:private s3 (Math/sqrt 3.0))

(defn- channels
  "[r g b] in [0, 1] of 0xRRGGBB."
  [^long c]
  [(/ (bit-and (bit-shift-right c 16) 0xFF) 255.0)
   (/ (bit-and (bit-shift-right c 8) 0xFF) 255.0)
   (/ (bit-and c 0xFF) 255.0)])

;; ── Brillouin zones ───────────────────────────────────────────────────────

(def lattices
  "Reciprocal-lattice bases [b1 b2] of the 2D Bravais lattices, |b1| = 1."
  {:hex      [[1.0 0.0] [0.5 (/ s3 2.0)]]
   :square   [[1.0 0.0] [0.0 1.0]]
   :rect     [[1.0 0.0] [0.0 0.62]]
   :centred  [[1.0 0.0] [0.5 0.85]]
   :oblique  [[1.0 0.0] [0.31 0.83]]})

(defn reciprocal-points
  "Flat [x y |G|² ..] of the reciprocal lattice with basis `basis` ([b1 b2],
   or a key of `lattices`) within radius `rmax` of the origin, origin excluded."
  ^doubles [basis rmax]
  (let [[[ax ay] [bx by]] (if (keyword? basis) (lattices basis) basis)]
    (double-array
     (mapcat identity
             (for [i (range -24 25) j (range -24 25)
                   :let [x (+ (* i ax) (* j bx)) y (+ (* i ay) (* j by))
                         r2 (+ (* x x) (* y y))]
                   :when (and (pos? r2) (< r2 (* rmax rmax)))]
               [x y r2])))))

(defn zone-shader
  "Shader of the zones at phase `u`: each pixel's zone number by counting the
   Bragg planes (perpendicular bisectors of 0-G) it lies beyond; its distance
   to the nearest plane lights the edges. Nothing turns: a pulse of light and
   the colours drift outward, one zone band per loop."
  [u {:keys [palette lattice rmax edge pulse-zones] :or {lattice :hex rmax 9.5 edge 0.012 pulse-zones 12.0}}]
  (let [^doubles gs (reciprocal-points lattice rmax)
        n (quot (alength gs) 3)
        inv (double-array (for [i (range n)] (/ 1.0 (Math/sqrt (aget gs (+ 2 (* 3 i)))))))
        u (double u) edge (double edge) pulse-zones (double pulse-zones)]
    (fn ^long [^double x ^double y]
      (loop [i 0 zone 1 dmin 1e9]
        (if (< i n)
          (let [j (* 3 i)
                d (* (aget inv i) (- (+ (* x (aget gs j)) (* y (aget gs (inc j)))) (* 0.5 (aget gs (+ 2 j)))))]
            (recur (inc i) (if (pos? d) (inc zone) zone) (min dmin (Math/abs d))))
          (let [q (/ dmin edge)
                glow (Math/exp (- (* q q)))
                pulse (+ 0.25 (* 0.75 (Math/pow (lp/wave (lp/frac (- (/ zone pulse-zones) u))) 3.0)))
                [cr cg cb] (channels (pal/at palette (- (/ zone pulse-zones) u)))
                fill (* 0.32 pulse)
                e (* glow (+ 0.4 (* 0.6 pulse)))]
            (r/rgb (+ (* (double cr) fill) (* e (+ 0.5 (* 0.5 (double cr)))))
                   (+ (* (double cg) fill) (* e (+ 0.5 (* 0.5 (double cg)))))
                   (+ (* (double cb) fill) (* e (+ 0.5 (* 0.5 (double cb))))))))))))

(defn brillouin
  "A Brillouin-zone loop on `lattice` (a key of `lattices`) coloured by the
   thi.ng scheme `scheme`, cycled around so the drift never seams."
  [lattice scheme title]
  (let [p (pal/around (pal/scheme scheme))]
    {:id (keyword (str "brillouin-" (name lattice)))
     :title title
     :seconds 20 :fps 30 :size [1920 1080] :view [8.0 4.5]
     :field (fn [u] (zone-shader u {:palette p :lattice lattice}))
     :post (fn [_] [{:type :bloom :radius 10 :strength 1.0}
                    {:type :vignette :strength 0.5}])}))

(def brillouin-zones
  (assoc (brillouin :hex :blue-magenta-orange "Brillouin zones of the hexagonal lattice, stained glass")
         :id :brillouin-zones))

(def brillouin-square
  (brillouin :square :green-blue-orange "Brillouin zones of the square lattice: four-fold stained glass"))

(def brillouin-oblique
  (brillouin :oblique :orange-magenta-blue "Brillouin zones of an oblique lattice: only inversion survives"))

(def brillouin-rect
  (brillouin :rect :yellow-green-blue "Brillouin zones of a rectangular lattice"))

;; ── Graphene Fermi surfaces ───────────────────────────────────────────────

(defn graphene-band
  "|f(k)| of graphene's nearest-neighbour tight-binding band, lattice
   vectors a1 = (1, 0), a2 = (1/2, √3/2) scaled by `a`: 0 at the Dirac
   points K, 1 at the saddles M, 3 at Gamma."
  ^double [^double a ^double x ^double y]
  (let [p1 (* a x)
        p2 (* a (+ (* 0.5 x) (* (/ s3 2.0) y)))]
    (Math/sqrt (max 0.0 (+ 3.0 (* 2.0 (Math/cos p1)) (* 2.0 (Math/cos p2)) (* 2.0 (Math/cos (- p1 p2))))))))

(defn fermi-shader
  "Shader of rising constant-energy contours at phase `u`: a contour every
   `step` in |f|, moving up one step per loop, glowing over the band's
   colour from `palette` (0 at the Dirac points, 1 at Gamma)."
  [u {:keys [palette step a sharp] :or {step 0.25 a 4.2 sharp 24.0}}]
  (let [u (double u) step (double step) a (double a) sharp (double sharp)]
    (fn ^long [^double x ^double y]
      (let [e (graphene-band a x y)
            line (Math/pow (lp/wave (lp/frac (- (/ e step) u))) sharp)
            dirac (Math/exp (* -40.0 e e))
            [cr cg cb] (channels (pal/at palette (/ e 3.0)))
            base 0.10
            k (+ base (* 0.95 line))]
        (r/rgb (+ (* (double cr) k) (* 0.8 dirac))
               (+ (* (double cg) k) (* 0.8 dirac))
               (+ (* (double cb) k) (* 0.8 dirac)))))))

(def graphene-fermi
  (let [p (pal/scheme :yellow-purple-magenta)]
    {:id :graphene-fermi
     :title "Graphene: Fermi contours rising from the Dirac points"
     :seconds 16 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
     :field (fn [u] (fermi-shader u {:palette p}))
     :post (fn [_] [{:type :bloom :radius 12 :strength 1.1}
                    {:type :chroma :shift 2}
                    {:type :vignette :strength 0.55}])}))

;; ── Kagome bands ───────────────────────────────────────────────────────────

(defn kagome-band
  "The upper dispersive band of the nearest-neighbour kagome lattice, as
   1 + sqrt(4 (cos²q1 + cos²q2 + cos²q3) - 3) with q_i = k·a_i/2: Dirac
   cones at K, a quadratic touch with the flat band at Gamma."
  ^double [^double a ^double x ^double y]
  (let [q1 (* 0.5 a x)
        q2 (* 0.5 a (+ (* 0.5 x) (* (/ s3 2.0) y)))
        q3 (- q2 q1)
        c1 (Math/cos q1) c2 (Math/cos q2) c3 (Math/cos q3)
        s (- (* 4.0 (+ (* c1 c1) (* c2 c2) (* c3 c3))) 3.0)]
    (+ 1.0 (Math/sqrt (max 0.0 s)))))

(defn band-contour-shader
  "Shader of rising contours of `band` ((fn [a x y] energy) on [0, emax]) at
   phase `u`, one contour every `step`, rising one step per loop, over the
   band's colour; `cusps` glows where the band reaches `emin`."
  [u band {:keys [palette step a sharp emin emax] :or {step 0.25 a 4.2 sharp 24.0 emin 0.0 emax 3.0}}]
  (let [u (double u) step (double step) a (double a) sharp (double sharp)
        emin (double emin) emax (double emax)]
    (fn ^long [^double x ^double y]
      (let [e (double (band a x y))
            line (Math/pow (lp/wave (lp/frac (- (/ e step) u))) sharp)
            dz (- e emin)
            cusp (Math/exp (* -40.0 dz dz))
            [cr cg cb] (channels (pal/at palette (/ dz (- emax emin))))
            k (+ 0.10 (* 0.95 line))]
        (r/rgb (+ (* (double cr) k) (* 0.8 cusp))
               (+ (* (double cg) k) (* 0.8 cusp))
               (+ (* (double cb) k) (* 0.8 cusp)))))))

(def kagome-fermi
  (let [p (pal/scheme :green-magenta)]
    {:id :kagome-fermi
     :title "Kagome: Fermi contours of the corner-sharing triangle lattice"
     :seconds 16 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
     :field (fn [u] (band-contour-shader u kagome-band {:palette p :emin 1.0 :emax 4.0 :a 3.2}))
     :post (fn [_] [{:type :bloom :radius 12 :strength 1.1}
                    {:type :chroma :shift 2}
                    {:type :vignette :strength 0.55}])}))

;; ── Other symmetries: square band, quasicrystal bands ──────────────────────

(defn square-band
  "Shifted square-lattice band 2 - cos(a x) - cos(a y): 0 at Gamma, 2 on
   the half-filling diamond, 4 at the zone corner."
  ^double [^double a ^double x ^double y]
  (- 2.0 (Math/cos (* a x)) (Math/cos (* a y))))

(defn quasi-band
  "An `n`-fold quasiperiodic landscape, n - sum_j cos(a k·e_j) over the unit
   vectors e_j at angles j·π/n: 0 at the origin, local n-fold stars
   everywhere, no period."
  [n]
  (let [n (long n)
        dirs (double-array (mapcat (fn [j] [(Math/cos (/ (* Math/PI j) n)) (Math/sin (/ (* Math/PI j) n))])
                                   (range n)))]
    (fn ^double [^double a ^double x ^double y]
      (loop [j 0 acc 0.0]
        (if (< j n)
          (recur (inc j) (+ acc (Math/cos (* a (+ (* x (aget dirs (* 2 j))) (* y (aget dirs (inc (* 2 j)))))))))
          (- (double n) acc))))))

(defn contour-loop
  "A rising-contour loop over `band` (energy 0 .. `emax`), coloured by the
   thi.ng scheme `scheme`. `offset` [x y] moves the view across the
   landscape (off the central rosette of a quasiperiodic band)."
  [id title band scheme {:keys [a step emax offset] :or {a 4.2 step 0.25 offset [0.0 0.0]}}]
  (let [p (pal/scheme scheme)
        [ox oy] offset
        ox (double ox) oy (double oy)
        moved (fn ^double [^double a ^double x ^double y] (double (band a (+ x ox) (+ y oy))))]
    {:id id :title title
     :seconds 16 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
     :field (fn [u] (band-contour-shader u moved {:palette p :a a :step step :emin 0.0 :emax emax}))
     :post (fn [_] [{:type :bloom :radius 12 :strength 1.1}
                    {:type :chroma :shift 2}
                    {:type :vignette :strength 0.55}])}))

(def square-fermi
  (contour-loop :square-fermi "Square lattice: diamond Fermi surfaces, four-fold"
                square-band :blue-cyan {:a 3.4 :step 0.2 :emax 4.0}))

(def penrose-fermi
  (contour-loop :penrose-fermi "Five-fold quasicrystal: contours that never repeat"
                (quasi-band 5) :orange-magenta-blue {:a 4.0 :step 0.45 :emax 10.0}))

(def heptagonal-fermi
  (contour-loop :heptagonal-fermi "Seven-fold: a symmetry no crystal can have"
                (quasi-band 7) :yellow-red {:a 7.5 :step 0.7 :emax 14.0}))

(def octagonal-fermi
  (contour-loop :octagonal-fermi "Eight-fold quasicrystal: Ammann-Beenker stars"
                (quasi-band 8) :green-cyan {:a 7.5 :step 0.8 :emax 16.0}))

(def symmetry-family
  "Generated n-fold contour loops beyond 5, 7, 8: id :fold-<n>. The view sits
   off the central rosette, in the quasiperiodic star field, which differs
   for every n. Odd n shows 2n-fold stars (the landscape is even in k)."
  (into (array-map)
        (for [[n scheme offset] [[9 :yellow-magenta-cyan [7.3 2.1]] [10 :blue-magenta-orange [11.6 -4.4]]
                                 [11 :green-red [-9.2 6.7]] [12 :cyan-magenta [14.1 3.3]]]
              :let [id (keyword (str "fold-" n))]]
          [id (contour-loop id (str n "-fold quasiperiodic contours")
                            (quasi-band n) scheme
                            {:a 5.0 :step (* 0.1 n) :emax (* 2.0 n) :offset offset})])))

;; ── Twisted bilayer graphene ──────────────────────────────────────────────

(defn honeycomb-density
  "A smooth atom density of a honeycomb lattice (spacing 1/`k`) turned by
   `theta`: the sum of three plane waves at 120 degrees, peaked on the sites."
  ^double [^double k ^double theta ^double x ^double y]
  (loop [i 0 acc 0.0]
    (if (< i 3)
      (let [a (+ theta (* i (/ lp/tau 3.0)))]
        (recur (inc i) (+ acc (Math/cos (* k (+ (* x (Math/cos a)) (* y (Math/sin a))))))))
      acc)))

(defn moire-shader
  "Shader of two honeycomb layers twisted by `twist` radians at phase `u`.
   A layer's honeycomb sites are the minima of its three-wave sum, so each
   layer's density is (3/2 - sum)/(9/2) in [0, 1]; their product lights the
   AA regions where the layers align, the moire superlattice of period about
   (2π/k)/twist. A pulse of light travels outward once per loop."
  [u {:keys [palette k twist] :or {k 26.0 twist 0.11}}]
  (let [u (double u) k (double k) twist (double twist)
        t1 (- (* 0.5 twist)) t2 (* 0.5 twist)]
    (fn ^long [^double x ^double y]
      (let [h1 (/ (- 1.5 (honeycomb-density k t1 x y)) 4.5)
            h2 (/ (- 1.5 (honeycomb-density k t2 x y)) 4.5)
            both (* h1 h2)
            atoms (Math/pow both 1.5)
            r (Math/sqrt (+ (* x x) (* y y)))
            pulse (+ 0.4 (* 0.6 (Math/pow (lp/wave (lp/frac (- (/ r 5.0) u))) 2.0)))
            [cr cg cb] (channels (pal/at palette (+ (* 0.5 (+ h1 h2)) (* 0.08 r))))
            v (* atoms pulse 2.2)]
        (r/rgb (* v (double cr)) (* v (double cg)) (* v (double cb)))))))

(def twisted-bilayer
  (let [p (pal/around (pal/scheme :yellow-purple-magenta))]
    {:id :twisted-bilayer
     :title "Twisted bilayer graphene: the moire superlattice, light pulsing through"
     :seconds 16 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
     :field (fn [u] (moire-shader u {:palette p}))
     :post (fn [_] [{:type :bloom :radius 10 :strength 1.0}
                    {:type :vignette :strength 0.5}])}))
