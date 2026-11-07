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
