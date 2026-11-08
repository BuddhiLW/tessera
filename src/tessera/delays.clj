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
            [tessera.palette :as pal]
            [desargues.wallpaper.render :as r]))

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

(def ^:private pendulum-spacing 0.4)

(def ^:private pendulums
  "Pivots on a 20 x 11 square grid. Each pendulum is tuned (its length
   chosen) so its period is 1/5 loop at ANY amplitude. Amplitude follows
   |field|, the delay is arg(field)/2pi: the delay winds by one whole period
   round each node, and nodal lanes barely move."
  (delay
    (let [s pendulum-spacing cols 20 rows 11
          cells (for [j (range rows) i (range cols)]
                  [(* s (- i (/ (dec cols) 2.0))) (+ 0.12 (* s (- j (/ (dec rows) 2.0))))])
          fields (mapv (fn [[x y]] (huygens x y)) cells)
          peak (apply max (map (fn [[re im]] (Math/hypot re im)) fields))]
      (mapv (fn [[x y] [re im]]
              (let [mag (/ (Math/hypot re im) peak)]
                {:pivot [x y]
                 :strength mag
                 :amplitude (* amplitude-max (Math/pow mag 1.1))
                 :delay (/ (Math/atan2 im re) lp/tau)}))
            cells fields))))

(def swing-stops
  "Cyan -> violet -> gold, closed into a cycle."
  [[0.00 0.72 1.00] [0.58 0.08 1.00] [1.00 0.58 0.00]])

(defn cyclic-colour
  "Smoothstep blend between consecutive stops, 1-periodic in t. The blend is
   resaturated (90% of its minimum channel removed, maximum kept), so the
   gold -> cyan leg does not pass through grey."
  [stops t]
  (let [n (count stops) x (* n (lp/frac (double t)))
        i (long (Math/floor x)) f (lp/smoothstep (- x i))
        a (stops (mod i n)) b (stops (mod (inc i) n))
        c (mapv #(+ (* (- 1.0 f) (double %1)) (* f (double %2))) a b)
        hi (double (apply max c)) lo (* 0.9 (double (apply min c)))
        k (/ hi (max 1.0e-9 (- hi lo)))]
    (mapv #(* k (- (double %) lo)) c)))

(defn pendulum-splats
  "Splat rows at loop phase u. Swing phase is 5u - delay: every pendulum
   completes 5 swings per loop. A thin rod, a bob and a continuous swept arc
   over the last 15% of the period; arc light and length scale with |field|.
   Colour is the cyclic palette at u - delay, so phase fronts drift outward.
   The three sources are faint rings pulsing with the drive (5 per loop)."
  [u]
  (let [u (double u) swings 5.0 len (* 0.43 pendulum-spacing)
        arc-n 40 arc-span 0.15
        drive (+ 0.55 (* 0.45 (Math/cos (* lp/tau swings u))))]
    (persistent!
     (reduce
      (fn [out {:keys [pivot amplitude delay strength]}]
        (let [[px py] pivot px (double px) py (double py)
              a (double amplitude) d (double delay) m (double strength)
              phase (- (* swings u) d)
              [cr cg cb] (cyclic-colour swing-stops (- u d))
              cr (double cr) cg (double cg) cb (double cb)
              glow (+ 0.06 (* 0.94 (Math/pow m 1.4)))
              th (pendulum-angle a phase)
              bx (+ px (* len (Math/sin th))) by (- py (* len (Math/cos th)))
              ;; pivot
              out (conj! out [px py 0.010 (* 0.30 glow cr) (* 0.30 glow cg) (* 0.30 glow cb)])
              ;; rod: a line of tiny splats
              out (reduce (fn [out k]
                            (let [t (/ (double k) 12.0) l (* 0.20 glow)]
                              (conj! out [(+ px (* t (- bx px))) (+ py (* t (- by py)))
                                          0.0055 (* l cr) (* l cg) (* l cb)])))
                          out (range 1 12))
              ;; bob
              out (conj! out [bx by 0.030 (* 0.80 glow cr) (* 0.80 glow cg) (* 0.80 glow cb)])
              out (conj! out [bx by 0.065 (* 0.12 glow cr) (* 0.12 glow cg) (* 0.12 glow cb)])]
          ;; swept arc behind the bob, fading toward its tail
          ;; light per sample is scaled by its spacing along the arc, so the
          ;; slow turning points do not pile up into a desaturated hot spot
          (reduce (fn [out k]
                    (let [f (/ (double k) arc-n)
                          ts (pendulum-angle a (- phase (* arc-span f)))
                          tn (pendulum-angle a (- phase (* arc-span (/ (+ k 1.0) arc-n))))
                          spacing (min 1.0 (/ (* len (Math/abs (- ts tn))) 0.012))
                          l (* 0.80 glow spacing (- 1.0 f) (- 1.0 f))]
                      (conj! out [(+ px (* len (Math/sin ts))) (- py (* len (Math/cos ts)))
                                  0.011 (* l cr) (* l cg) (* l cb)])))
                  out (range 1 (inc arc-n)))))
      (reduce (fn [out [sx sy]]
                (reduce (fn [out k]
                          (let [ang (* lp/tau (/ (double k) 40.0)) l (* 0.30 drive)]
                            (conj! out [(+ (double sx) (* 0.11 (Math/cos ang)))
                                        (+ (double sy) (* 0.11 (Math/sin ang)))
                                        0.008 (* 0.55 l) (* 0.85 l) l])))
                        out (range 40)))
              (transient []) (:points huygens-sources))
      @pendulums))))

(def ^:private huygens-peak
  (delay (double (reduce max (for [x (range -4.0 4.0 0.05) y (range -2.25 2.25 0.05)]
                               (let [[re im] (huygens x y)] (Math/hypot re im)))))))

(defn huygens-shader
  "Dim underlay: the driving field's intensity |field|^2, shaded by the same
   cyclic palette at u - arg(field)/2pi, so bright lanes and dark nodal lanes
   read between the pendulums and the phase fronts drift outward."
  [u]
  (let [u (double u)
        {:keys [points k]} huygens-sources
        pts (double-array (apply concat points)) k (double k)
        n (quot (alength pts) 2)
        mx (double @huygens-peak)]
    (fn ^long [^double x ^double y]
      (loop [j 0 re 0.0 im 0.0]
        (if (< j n)
          (let [r (Math/hypot (- x (aget pts (* 2 j))) (- y (aget pts (inc (* 2 j)))))
                w (/ 1.0 (Math/sqrt (+ r 0.6)))]
            (recur (inc j) (+ re (* w (Math/cos (* k r)))) (+ im (* w (Math/sin (* k r))))))
          (let [m (min 1.0 (/ (Math/hypot re im) mx))
                [cr cg cb] (cyclic-colour swing-stops (- u (/ (Math/atan2 im re) lp/tau)))
                v (* 0.85 m m m)]
            (r/rgb (* v (double cr)) (* v (double cg)) (* v (double cb)))))))))

(def delay-pendulums
  {:id :delay-pendulums
   :title "Delay pendulums: tuned nonlinear pendulums phased by three coherent sources"
   :seconds 20 :fps 30 :size [1920 1080] :view view :ss 2
   :field huygens-shader
   :splat pendulum-splats :exposure 2.6
   :post (fn [_] [{:type :bloom :radius 8 :strength 0.55}
                  {:type :vignette :strength 0.22}])})

;; ---------------------------------------------------------------------------
;; 2. noise-flow-lines: streamlines of a stream function made of periodic noise.

(def ^:private noise-modes
  "Random-feature noise in 4D: [kx ky cx cy a phase]. The stream function is
   psi = sum a sin(kx x + ky y + R(cx cos 2pi u + cy sin 2pi u) + phase): a
   smooth random field sampled on a circle of radius R in its last two axes,
   so psi(u + 1) = psi(u) exactly. Spectrum ~ k^-2.4 for large calm eddies."
  (let [rng (java.util.Random. 20261108) n 28 radius 1.35]
    (vec (for [_ (range n)]
           (let [k (+ 0.55 (* 1.9 (Math/pow (.nextDouble rng) 1.6)))
                 angle (* lp/tau (.nextDouble rng))
                 t-angle (* lp/tau (.nextDouble rng))
                 t-mag (* radius (Math/sqrt (.nextDouble rng)))]
             [(* k (Math/cos angle)) (* k (Math/sin angle))
              (* t-mag (Math/cos t-angle)) (* t-mag (Math/sin t-angle))
              (* 0.62 (Math/pow k -2.4)) (* lp/tau (.nextDouble rng))])))))

(def ^:private mode-array
  (double-array (apply concat noise-modes)))

(defn noise-flow
  "[vx vy vorticity psi] of the incompressible flow v = (d_y psi, -d_x psi) at
   (x, y) and loop phase u. Derivatives are analytic, so div v = 0 exactly."
  ^doubles [^double x ^double y ^double u]
  (let [^doubles m mode-array n (quot (alength m) 6)
        cu (Math/cos (* lp/tau u)) su (Math/sin (* lp/tau u))
        out (double-array 4)]
    (dotimes [j n]
      (let [o (* 6 j) kx (aget m o) ky (aget m (+ o 1))
            a (aget m (+ o 4))
            th (+ (* kx x) (* ky y) (* (aget m (+ o 2)) cu) (* (aget m (+ o 3)) su)
                  (aget m (+ o 5)))
            s (* a (Math/sin th)) c (* a (Math/cos th))]
        (aset out 0 (+ (aget out 0) (* ky c)))
        (aset out 1 (- (aget out 1) (* kx c)))
        (aset out 2 (+ (aget out 2) (* (+ (* kx kx) (* ky ky)) s)))
        (aset out 3 (+ (aget out 3) s))))
    out))

(def ^:private flow-speed 3.2)

(defn- trace-particle
  "Open path over one life: age p in [0,1] maps to loop time p - offset.
   Midpoint steps, 160 samples, stored as interleaved xy."
  ^doubles [origin offset]
  (let [steps 160 h (/ 1.0 steps) out (double-array (* 2 (inc steps)))
        offset (double offset)]
    (loop [i 0 x (double (first origin)) y (double (second origin))]
      (aset out (* 2 i) x) (aset out (inc (* 2 i)) y)
      (if (= i steps)
        out
        (let [t (- (* i h) offset)
              ^doubles a (noise-flow x y t)
              mx (+ x (* 0.5 h flow-speed (aget a 0))) my (+ y (* 0.5 h flow-speed (aget a 1)))
              ^doubles b (noise-flow mx my (+ t (* 0.5 h)))]
          (recur (inc i) (+ x (* h flow-speed (aget b 0))) (+ y (* h flow-speed (aget b 1)))))))))

(defn- envelope
  "C2 birth/death ramp, exactly zero at both ends of the life."
  ^double [^double p]
  (if (or (<= p 0.0) (>= p 1.0))
    0.0
    (let [s (min 1.0 (/ p 0.22) (/ (- 1.0 p) 0.22))]
      (* s s s (+ 10.0 (* s (+ -15.0 (* 6.0 s))))))))

(def ^:private flow-particles
  "Stratified seeds over a slightly enlarged view; golden-ratio life offsets,
   so a fresh stroke fades in somewhere every frame as an old one fades out."
  (delay
    (let [rng (java.util.Random. 7041) cols 64 rows 36 [vw vh] view]
      (vec (for [j (range rows) i (range cols)
                 :let [idx (+ i (* j cols))
                       x (* 1.08 vw (- (/ (+ i (.nextDouble rng)) cols) 0.5))
                       y (* 1.08 vh (- (/ (+ j (.nextDouble rng)) rows) 0.5))
                       offset (lp/frac (* 0.618033988749895 idx))]]
             {:offset offset :path (trace-particle [x y] offset)})))))

(defn- path-at
  "Particle position at age p, linear in the stored samples, clamped."
  [^doubles path ^double p]
  (let [n (dec (quot (alength path) 2))
        t (* n (max 0.0 (min 1.0 p)))
        i (min (dec n) (long (Math/floor t))) f (- t i) k (* 2 i)]
    [(+ (* (- 1.0 f) (aget path k)) (* f (aget path (+ k 2))))
     (+ (* (- 1.0 f) (aget path (inc k))) (* f (aget path (+ k 3))))]))

(def ^:private vortex-palette
  "Clockwise eddies glow magenta-rose, counter-clockwise ones cyan-teal."
  [[0.95 0.16 0.62] [0.30 0.24 0.98] [0.10 0.90 0.86]])

(defn- vortex-colour [^double w]
  (let [t (Math/tanh (* 0.9 w))
        [a b c] vortex-palette
        mix (fn [p q s] (mapv #(+ (* (- 1.0 s) %1) (* s %2)) p q))]
    (if (neg? t) (mix b a (- t)) (mix b c t))))

(defn flow-line-splats
  "Each stroke is the streamline of the INSTANTANEOUS flow through its
   particle, traced both ways by unit-speed midpoint steps; the particle
   itself rides the flow. Light tapers to the stroke's ends and to zero at
   birth and death (replacement), so there is no pop and no seam."
  [u]
  (let [u (lp/frac (double u)) half 14 ds 0.022]
    (persistent!
     (reduce
      (fn [out {:keys [offset path]}]
        (let [p (lp/frac (+ u (double offset))) vis (envelope p)]
          (if (< vis 1.0e-6)
            out
            (let [[x0 y0] (path-at path p)
                  walk (fn [out dir]
                         (loop [k 0 x (double x0) y (double y0) out out]
                           (if (> k half)
                             out
                             (let [^doubles f (noise-flow x y u)
                                   sp (Math/hypot (aget f 0) (aget f 1))
                                   f-k (/ (double k) half)
                                   taper (* (- 1.0 (* f-k f-k)) (- 1.0 (* 0.5 f-k)))
                                   light (* 0.24 vis taper (Math/min 1.0 (* 1.6 sp)))
                                   [cr cg cb] (vortex-colour (aget f 2))
                                   out (if (and (zero? k) (neg? dir)) out
                                           (conj! out [x y 0.0075 (* light (double cr))
                                                       (* light (double cg)) (* light (double cb))]))
                                   step (/ (* dir ds) (max sp 1.0e-6))
                                   mx (+ x (* 0.5 step (aget f 0))) my (+ y (* 0.5 step (aget f 1)))
                                   ^doubles g (noise-flow mx my u)
                                   sg (/ (* dir ds) (max 1.0e-6 (Math/hypot (aget g 0) (aget g 1))))]
                               (recur (inc k) (+ x (* sg (aget g 0))) (+ y (* sg (aget g 1))) out)))))]
              (-> out (walk 1.0) (walk -1.0))))))
      (transient []) @flow-particles))))

(def noise-flow-lines
  {:id :noise-flow-lines
   :title "Periodic-noise flow lines: streamlines of a 4D-noise stream function, coloured by vorticity"
   :seconds 20 :fps 30 :size [1920 1080] :view view
   :frame (fn [_] {:background "#010206" :shapes []})
   :splat flow-line-splats :exposure 1.0
   :post (fn [_] [{:type :bloom :radius 7 :strength 0.45}
                  {:type :vignette :strength 0.25}])})

(def loops
  "Loop specs for the coordinator's registry."
  (array-map :delay-pendulums delay-pendulums))
