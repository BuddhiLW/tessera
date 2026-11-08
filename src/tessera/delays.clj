(ns tessera.delays
  "Etienne Jacob's loop techniques (bleuje.com), applied to mechanics.

   Delay propagation: every element runs the SAME periodic motion
   f(m u - delay(x)); delay may be any real function of position, so the
   loop closes exactly while a wave of motion sweeps the structure.
   Replacement chains: beads on a closed orbit advance exactly one slot per
   loop, so bead k ends where bead k+1 began. Periodic noise: gradient noise
   whose time lattice wraps, sampled at t = P u, returns exactly.
   No whole-frame rotation anywhere; colour cycles use integer frequencies."
  (:require [desargues.wallpaper.loop :as lp]
            [tessera.palette :as pal]))

(set! *warn-on-reflection* true)

(def view [8.0 4.5])

(defn- channels [^long c]
  [(/ (bit-and (bit-shift-right c 16) 0xff) 255.0)
   (/ (bit-and (bit-shift-right c 8) 0xff) 255.0)
   (/ (bit-and c 0xff) 255.0)])

;; ---------------------------------------------------------------------------
;; The exact nonlinear pendulum, theta'' = -sin theta.

(defn agm
  "Arithmetic-geometric mean of a and b."
  ^double [^double a ^double b]
  (loop [a a b b i 0]
    (if (or (< (Math/abs (- a b)) 1.0e-15) (> i 60))
      a
      (recur (* 0.5 (+ a b)) (Math/sqrt (* a b)) (inc i)))))

(defn pendulum-period
  "Exact period of theta'' = -sin theta released from rest at amplitude a:
   T = 2 pi / AGM(1, cos(a/2)), the complete elliptic integral 4K(sin a/2)."
  ^double [^double a]
  (/ lp/tau (agm 1.0 (Math/cos (* 0.5 a)))))

(defn pendulum-orbit
  "theta at m uniformly spaced fractions of ONE exact period, released from
   rest at amplitude a (fraction 0 is the right turning point). RK4 with 16
   substeps per sample; the period comes from the AGM, not from the stepper."
  ^doubles [a m]
  (let [a (double a) m (long m) out (double-array m)]
    (if (zero? a)
      out
      (let [sub 16 h (/ (pendulum-period a) (* m sub)) h6 (/ h 6.0)]
        (loop [i 0 q a v 0.0]
          (if (= i m)
            out
            (do (aset out i q)
                (let [state (double-array [q v])]
                  (dotimes [_ sub]
                    (let [q (aget state 0) v (aget state 1)
                          k1q v k1v (- (Math/sin q))
                          k2q (+ v (* 0.5 h k1v)) k2v (- (Math/sin (+ q (* 0.5 h k1q))))
                          k3q (+ v (* 0.5 h k2v)) k3v (- (Math/sin (+ q (* 0.5 h k2q))))
                          k4q (+ v (* h k3v)) k4v (- (Math/sin (+ q (* h k3q))))]
                      (aset state 0 (+ q (* h6 (+ k1q (* 2.0 k2q) (* 2.0 k3q) k4q))))
                      (aset state 1 (+ v (* h6 (+ k1v (* 2.0 k2v) (* 2.0 k3v) k4v))))))
                  (recur (inc i) (aget state 0) (aget state 1))))))))))

(defn table-at
  "Periodic linear interpolation of table t at phase s (any real; period 1)."
  ^double [^doubles t ^double s]
  (let [m (alength t) x (* m (lp/frac s))
        i (long (Math/floor x)) f (- x i)
        i (mod i m) j (mod (inc i) m)]
    (+ (* (- 1.0 f) (aget t i)) (* f (aget t j)))))

(def ^:private amplitude-max 2.75)
(def ^:private amplitude-steps 24)

(def ^:private orbit-family
  "Exact orbits for amplitudes k/24 * 2.75, k = 0..24."
  (delay (mapv #(pendulum-orbit (* amplitude-max (/ (double %) amplitude-steps)) 512)
               (range (inc amplitude-steps)))))

(defn pendulum-angle
  "theta of a pendulum of amplitude a (0..2.75) at phase s of its own period.
   Neighbouring exact orbits are blended in amplitude; each is 1-periodic."
  ^double [^double a ^double s]
  (let [family @orbit-family
        x (* amplitude-steps (/ (max 0.0 (min amplitude-max a)) amplitude-max))
        i (min (dec amplitude-steps) (long (Math/floor x))) f (- x i)]
    (+ (* (- 1.0 f) (table-at (family i) s))
       (* f (table-at (family (inc i)) s)))))

;; ---------------------------------------------------------------------------
;; 1. delay-pendulums: tuned pendulums driven by a Huygens field.

(def huygens-sources
  "Three coherent point sources [x y] (scene units), wavenumber k."
  {:points [[-2.85 -1.35] [2.65 -1.05] [0.15 2.05]] :k (/ lp/tau 1.55)})

(defn huygens
  "Complex sum of cylindrical waves exp(i k r)/sqrt(r + 0.6) at (x, y)."
  [x y]
  (let [{:keys [points k]} huygens-sources x (double x) y (double y) k (double k)]
    (reduce (fn [[re im] [px py]]
              (let [r (Math/hypot (- x (double px)) (- y (double py)))
                    w (/ 1.0 (Math/sqrt (+ r 0.6)))]
                [(+ re (* w (Math/cos (* k r)))) (+ im (* w (Math/sin (* k r))))]))
            [0.0 0.0] points)))

(def ^:private pendulum-spacing 0.2)

(def ^:private pendulums
  "Pivots on a square grid. Each pendulum is tuned (its length chosen) so its
   period is 1/5 loop at ANY amplitude. Amplitude follows |field|, the delay
   is arg(field)/2pi: the delay winds by one whole period round each node."
  (delay
    (let [s pendulum-spacing cols 40 rows 23
          cells (for [j (range rows) i (range cols)]
                  [(* s (- i (/ (dec cols) 2.0))) (* s (- j (/ (dec rows) 2.0)))])
          fields (mapv (fn [[x y]] (huygens x y)) cells)
          peak (apply max (map (fn [[re im]] (Math/hypot re im)) fields))]
      (mapv (fn [[x y] [re im]]
              (let [mag (/ (Math/hypot re im) peak)]
                {:pivot [x y]
                 :amplitude (* amplitude-max (Math/pow mag 0.8))
                 :delay (/ (Math/atan2 im re) lp/tau)}))
            cells fields))))

(def ^:private swing-palette
  "Integer-frequency cyclic rainbow, deep minimum channel for saturation."
  (pal/cosine [0.42 0.42 0.42] [0.58 0.58 0.58] [1.0 1.0 1.0] [0.0 0.33 0.67]))

(defn pendulum-splats
  "Splat rows at loop phase u: bob, a short comet trail, a dim rod and pivot.
   Swing phase is 5u - delay, so every pendulum completes 5 swings per loop."
  [u]
  (let [u (double u) swings 5.0 len (* 0.44 pendulum-spacing)
        trail 7 dt 0.016]
    (persistent!
     (reduce
      (fn [out {:keys [pivot amplitude delay]}]
        (let [[px py] pivot px (double px) py (double py)
              a (double amplitude) d (double delay)
              strength (/ a amplitude-max)
              phase (- (* swings u) d)
              [cr cg cb] (channels (pal/at swing-palette (lp/frac phase)))
              cr (double cr) cg (double cg) cb (double cb)
              cos-a (Math/cos a)
              out (conj! out [px py 0.010 0.05 0.05 0.07])]
          (if (< a 0.02)
            (conj! out [px (- py len) 0.016 (* 0.10 cr) (* 0.10 cg) (* 0.10 cb)])
            (loop [s 0 out out]
              (if (= s trail)
                out
                (let [th (pendulum-angle a (- phase (* s dt)))
                      bx (+ px (* len (Math/sin th))) by (- py (* len (Math/cos th)))
                      speed (if (< (- 1.0 cos-a) 1.0e-9) 0.0
                                (Math/sqrt (max 0.0 (/ (- (Math/cos th) cos-a) (- 1.0 cos-a)))))
                      light (* (+ 0.30 (* 0.70 speed)) (+ 0.25 (* 0.75 strength))
                               (Math/exp (* -0.42 s)))
                      sigma (if (zero? s) 0.017 (* 0.012 (- 1.0 (* 0.06 s))))
                      out (conj! out [bx by sigma (* light cr) (* light cg) (* light cb)])]
                  (recur (inc s)
                         (if (zero? s)
                           (reduce (fn [out t]
                                     (conj! out [(+ px (* t (- bx px))) (+ py (* t (- by py)))
                                                 0.006 (* 0.10 cr) (* 0.10 cg) (* 0.10 cb)]))
                                   out [0.25 0.5 0.75])
                           out))))))))
      (transient []) @pendulums))))

(def delay-pendulums
  {:id :delay-pendulums
   :title "Delay pendulums: tuned nonlinear pendulums phased by three coherent sources"
   :seconds 20 :fps 30 :size [1920 1080] :view view
   :frame (fn [_] {:background "#010205" :shapes []})
   :splat pendulum-splats :exposure 1.0
   :post (fn [_] [{:type :bloom :radius 8 :strength 0.55}
                  {:type :vignette :strength 0.22}])})

(def loops
  "Loop specs for the coordinator's registry."
  (array-map :delay-pendulums delay-pendulums))
