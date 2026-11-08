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
                 ;; capped at 1.6 rad (92 deg) so long rods never sweep
                 ;; into the row above; still well into the nonlinear regime
                 :amplitude (* 1.6 (Math/pow mag 1.1))
                 :delay (/ (Math/atan2 im re) lp/tau)}))
            cells fields))))

(def swing-stops
  "Cyan -> violet -> gold -> violet, closed into a cycle. The return leg goes
   back through violet: a straight OKLab line from gold to cyan would pass
   through grey."
  [[0.00 0.72 1.00] [0.58 0.08 1.00] [1.00 0.58 0.00] [0.58 0.08 1.00]])

(defn- srgb->linear ^double [^double c]
  (if (<= c 0.04045) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4)))

(defn- linear->srgb ^double [^double c]
  (let [c (max 0.0 (min 1.0 c))]
    (if (<= c 0.0031308) (* 12.92 c) (- (* 1.055 (Math/pow c (/ 1.0 2.4))) 0.055))))

(defn oklab
  "sRGB [r g b] in [0,1] -> OKLab [L a b] (Ottosson 2020)."
  [[r g b]]
  (let [r (srgb->linear (double r)) g (srgb->linear (double g)) b (srgb->linear (double b))
        l (Math/cbrt (+ (* 0.4122214708 r) (* 0.5363325363 g) (* 0.0514459929 b)))
        m (Math/cbrt (+ (* 0.2119034982 r) (* 0.6806995451 g) (* 0.1073969566 b)))
        s (Math/cbrt (+ (* 0.0883024619 r) (* 0.2817188376 g) (* 0.6299787005 b)))]
    [(+ (* 0.2104542553 l) (* 0.7936177850 m) (* -0.0040720468 s))
     (+ (* 1.9779984951 l) (* -2.4285922050 m) (* 0.4505937099 s))
     (+ (* 0.0259040371 l) (* 0.7827717662 m) (* -0.8086757660 s))]))

(defn oklab->srgb
  "OKLab [L a b] -> sRGB [r g b], clamped to [0,1]."
  [[L a b]]
  (let [L (double L) a (double a) b (double b)
        l (Math/pow (+ L (* 0.3963377774 a) (* 0.2158037573 b)) 3.0)
        m (Math/pow (- L (* 0.1055613458 a) (* 0.0638541728 b)) 3.0)
        s (Math/pow (- L (* 0.0894841775 a) (* 1.2914855480 b)) 3.0)]
    [(linear->srgb (+ (* 4.0767416621 l) (* -3.3077115913 m) (* 0.2309699292 s)))
     (linear->srgb (+ (* -1.2684380046 l) (* 2.6097574011 m) (* -0.3413193965 s)))
     (linear->srgb (+ (* -0.0041960863 l) (* -0.7034186147 m) (* 1.7076147010 s)))]))

(defn cyclic-colour
  "Straight-line OKLab blend between consecutive stops (smoothstep pacing),
   1-periodic in t. A straight line in OKLab never swings round the hue
   wheel, so the cycle stays within its stops' family."
  [stops t]
  (let [n (count stops) x (* n (lp/frac (double t)))
        i (long (Math/floor x)) f (lp/smoothstep (- x i))
        a (oklab (stops (mod i n))) b (oklab (stops (mod (inc i) n)))
        c (oklab->srgb (mapv #(+ (* (- 1.0 f) (double %1)) (* f (double %2))) a b))
        ;; HSV-hue-preserving resaturation: drop 70% of the minimum channel
        ;; and restore the maximum, so additive overlaps stay coloured
        hi (double (apply max c)) lo (* 0.7 (double (apply min c)))
        k (/ hi (max 1.0e-9 (- hi lo)))]
    (mapv #(* k (- (double %) lo)) c)))

(defn pendulum-splats
  "Splat rows at loop phase u. Swing phase is 5u - delay: every pendulum
   completes 5 swings per loop. A thin rod, a bob and a continuous swept arc
   over the last 15% of the period; arc light and length scale with |field|.
   Colour is the cyclic palette at u - delay, so phase fronts drift outward.
   The three sources are faint rings pulsing with the drive (5 per loop)."
  [u]
  (let [u (double u) swings 5.0 len (* 0.8 pendulum-spacing)
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
              glow (+ 0.05 (* 0.95 (Math/pow m 1.15)))
              th (pendulum-angle a phase)
              bx (+ px (* len (Math/sin th))) by (- py (* len (Math/cos th)))
              ;; pivot
              out (conj! out [px py 0.010 (* 0.30 glow cr) (* 0.30 glow cg) (* 0.30 glow cb)])
              ;; rod: a line of tiny splats
              out (reduce (fn [out k]
                            (let [t (/ (double k) 32.0) l (* 0.13 glow)]
                              (conj! out [(+ px (* t (- bx px))) (+ py (* t (- by py)))
                                          0.0055 (* l cr) (* l cg) (* l cb)])))
                          out (range 2 30))
              ;; bob
              out (conj! out [bx by 0.030 (* 0.45 glow cr) (* 0.45 glow cg) (* 0.45 glow cb)])
              out (conj! out [bx by 0.065 (* 0.12 glow cr) (* 0.12 glow cg) (* 0.12 glow cb)])]
          ;; swept arc behind the bob, fading toward its tail
          ;; light per sample is scaled by its spacing along the arc, so the
          ;; slow turning points do not pile up into a desaturated hot spot
          (reduce (fn [out k]
                    (let [f (/ (double k) arc-n)
                          ts (pendulum-angle a (- phase (* arc-span f)))
                          tn (pendulum-angle a (- phase (* arc-span (/ (+ k 1.0) arc-n))))
                          spacing (min 1.0 (/ (* len (Math/abs (- ts tn))) 0.012))
                          l (* 0.65 glow spacing (- 1.0 f))]
                      (conj! out [(+ px (* len (Math/sin ts))) (- py (* len (Math/cos ts)))
                                  0.017 (* l cr) (* l cg) (* l cb)])))
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

(def ^:private swing-lut
  "cyclic-colour of swing-stops tabulated at 1024 phases, flat rgb."
  (delay (double-array (mapcat #(cyclic-colour swing-stops (/ (double %) 1024.0)) (range 1024)))))

(defn huygens-shader
  "Dim underlay: four thin, crisp iso-lines of the drive amplitude |field|
   (levels .3 .5 .7 .9 of its peak), screen-space width via the analytic-free
   finite-difference gradient. Coloured by the cyclic palette at
   u - arg(field)/2pi, like the pendulums above them."
  [u]
  (let [u (double u)
        {:keys [points k]} huygens-sources
        pts (double-array (apply concat points)) k (double k)
        n (quot (alength pts) 2)
        mx (double @huygens-peak)
        ^doubles lut @swing-lut
        field (fn [^double x ^double y]
                (loop [j 0 re 0.0 im 0.0]
                  (if (< j n)
                    (let [r (Math/hypot (- x (aget pts (* 2 j))) (- y (aget pts (inc (* 2 j)))))
                          w (/ 1.0 (Math/sqrt (+ r 0.6)))]
                      (recur (inc j) (+ re (* w (Math/cos (* k r)))) (+ im (* w (Math/sin (* k r))))))
                    (double-array [re im]))))]
    (fn ^long [^double x ^double y]
      (let [e 0.002
            ^doubles f (field x y) ^doubles fx (field (+ x e) y) ^doubles fy (field x (+ y e))
            m (/ (Math/hypot (aget f 0) (aget f 1)) mx)
            gx (/ (- (/ (Math/hypot (aget fx 0) (aget fx 1)) mx) m) e)
            gy (/ (- (/ (Math/hypot (aget fy 0) (aget fy 1)) mx) m) e)
            grad (max 0.05 (Math/hypot gx gy))
            level (* 5.0 (- m 0.1))
            d (/ (/ (Math/abs (- level (Math/rint level))) 5.0) grad)
            on (and (>= (Math/rint level) 1.0) (<= (Math/rint level) 4.0))
            line (if on (Math/exp (- (Math/pow (/ d 0.0065) 2.0))) 0.0)
            o (* 3 (bit-and 1023 (long (Math/floor (* 1024.0 (- u (/ (Math/atan2 (aget f 1) (aget f 0)) lp/tau)))))))
            v (* 0.22 line (+ 0.4 (* 0.6 m)))]
        (r/rgb (* v (aget lut o)) (* v (aget lut (+ o 1))) (* v (aget lut (+ o 2))))))))

(def delay-pendulums
  {:id :delay-pendulums
   :title "Delay pendulums: tuned nonlinear pendulums phased by three coherent sources"
   :seconds 20 :fps 30 :size [1920 1080] :view view :ss 2
   :field huygens-shader
   :splat pendulum-splats :exposure 5.0
   :post (fn [_] [{:type :bloom :radius 8 :strength 0.40}
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

(defn flow-grid
  "The flow at phase u tabulated on a 0.025-unit grid over the enlarged view:
   {:vx :vy :w} float planes plus geometry, sampled bilinearly by flow-at."
  [u]
  (let [h 0.025 [vw vh] view
        x0 (- (* 0.6 (double vw))) y0 (- (* 0.6 (double vh)))
        nx (long (Math/ceil (/ (* 1.2 (double vw)) h))) ny (long (Math/ceil (/ (* 1.2 (double vh)) h)))
        vx (double-array (* nx ny)) vy (double-array (* nx ny)) w (double-array (* nx ny))]
    (dotimes [j ny]
      (dotimes [i nx]
        (let [^doubles f (noise-flow (+ x0 (* i h)) (+ y0 (* j h)) (double u)) o (+ i (* j nx))]
          (aset vx o (aget f 0)) (aset vy o (aget f 1)) (aset w o (aget f 2)))))
    {:h h :x0 x0 :y0 y0 :nx nx :ny ny :vx vx :vy vy :w w}))

(defn- sample-grid
  [^doubles a nx ny gx gy]
  (let [nx (long nx) ny (long ny) gx (double gx) gy (double gy)
        i (max 0 (min (- nx 2) (long (Math/floor gx))))
        j (max 0 (min (- ny 2) (long (Math/floor gy))))
        tx (- gx i) ty (- gy j) o (+ i (* j nx))]
    (+ (* (- 1.0 ty) (+ (* (- 1.0 tx) (aget a o)) (* tx (aget a (inc o)))))
       (* ty (+ (* (- 1.0 tx) (aget a (+ o nx))) (* tx (aget a (+ o nx 1))))))))

(defn flow-line-splats
  "Each stroke is the streamline of the INSTANTANEOUS flow through its
   particle, traced both ways by unit-speed midpoint steps; the particle
   itself rides the flow. Light tapers to the stroke's ends and to zero at
   birth and death (replacement), so there is no pop and no seam."
  [u]
  (let [u (lp/frac (double u)) half 16 ds 0.02
        {:keys [h x0 y0 nx ny vx vy w]} (flow-grid u)
        h (double h) x0 (double x0) y0 (double y0) nx (long nx) ny (long ny)
        ^doubles vx vx ^doubles vy vy ^doubles w w]
    (persistent!
     (reduce
      (fn [out {:keys [offset path]}]
        (let [p (lp/frac (+ u (double offset))) vis (envelope p)]
          (if (< vis 1.0e-6)
            out
            (let [[x0p y0p] (path-at path p)
                  walk (fn [out dir]
                         (loop [k 0 x (double x0p) y (double y0p) out out]
                           (if (> k half)
                             out
                             (let [gx (/ (- x x0) h) gy (/ (- y y0) h)
                                   fx (sample-grid vx nx ny gx gy) fy (sample-grid vy nx ny gx gy)
                                   sp (Math/hypot fx fy)
                                   f-k (/ (double k) half)
                                   taper (* (- 1.0 (* f-k f-k)) (- 1.0 (* 0.4 f-k)))
                                   light (* 0.42 vis taper (Math/min 1.0 (* 0.9 sp)))
                                   [cr cg cb] (vortex-colour (sample-grid w nx ny gx gy))
                                   out (if (and (zero? k) (neg? dir)) out
                                           (conj! out [x y 0.0075 (* light (double cr))
                                                       (* light (double cg)) (* light (double cb))]))
                                   step (/ (* dir ds) (max sp 1.0e-6))
                                   mx (+ x (* 0.5 step fx)) my (+ y (* 0.5 step fy))
                                   mgx (/ (- mx x0) h) mgy (/ (- my y0) h)
                                   gx2 (sample-grid vx nx ny mgx mgy) gy2 (sample-grid vy nx ny mgx mgy)
                                   sg (/ (* dir ds) (max 1.0e-6 (Math/hypot gx2 gy2)))]
                               (recur (inc k) (+ x (* sg gx2)) (+ y (* sg gy2)) out)))))]
              (-> out (walk 1.0) (walk -1.0))))))
      (transient []) @flow-particles))))

(def noise-flow-lines
  {:id :noise-flow-lines
   :title "Periodic-noise flow lines: streamlines of a 4D-noise stream function, coloured by vorticity"
   :seconds 20 :fps 30 :size [1920 1080] :view view
   :frame (fn [_] {:background "#010206" :shapes []})
   :splat flow-line-splats :exposure 1.9
   :post (fn [_] [{:type :bloom :radius 7 :strength 0.45}
                  {:type :vignette :strength 0.25}])})

;; ---------------------------------------------------------------------------
;; 3. delay-mesh: a Gerstner (trochoidal) sea, every mesh vertex a water parcel.

(def gerstner-trains
  "[direction-angle wavelength temporal-harmonic steepness]. In a Gerstner
   train every surface parcel runs the SAME circle, delayed by k.x/2pi; m
   periods per loop, deep-water steepness Q = kA < 1 (no self-crossing)."
  [[0.35 2.6 2 0.42] [-0.55 1.7 3 0.30] [1.95 1.15 4 0.20]])

(def ^:private ripple
  "A radial packet from one point: the same orbit f(u - r/L) with an envelope
   that is itself periodic in its argument, so one ring leaves per loop."
  {:centre [-1.6 0.6] :wavelength 0.55 :reach 4.0 :amplitude 0.05
   ;; time origin of the whole sea (u -> u + origin is still 1-periodic):
   ;; puts the wrap on an ordinary, not a fast, moment of the motion
   :origin 0.46})

(defn gerstner
  "[dx dy z] of the parcel at rest position (x, y), loop phase u. Horizontal
   displacement Q/k along k cos(theta), height Q/k sin(theta), theta =
   k.x - 2pi m u, summed over trains, plus the radial ripple packet."
  [x y u]
  (let [x (double x) y (double y) u (+ (double u) (double (:origin ripple 0.0)))
        [dx dy z]
        (reduce (fn [[dx dy z] [ang lam m q]]
                  (let [k (/ lp/tau (double lam)) a (/ (double q) k)
                        cx (Math/cos (double ang)) cy (Math/sin (double ang))
                        th (- (* k (+ (* cx x) (* cy y))) (* lp/tau (long m) u))
                        c (Math/cos th) s (Math/sin th)]
                    [(- dx (* a cx s)) (- dy (* a cy s)) (+ z (* a c))]))
                [0.0 0.0 0.0] gerstner-trains)
        {[ox oy] :centre lam :wavelength reach :reach amp :amplitude} ripple
        rx (- x (double ox)) ry (- y (double oy)) r (Math/hypot rx ry)
        s (- u (/ r (double reach)))
        q (- (lp/frac s) 0.5)
        env (* (Math/exp (* -60.0 q q)) (/ 1.0 (Math/sqrt (+ 1.0 (* 2.0 r)))))
        th (* lp/tau (/ r (double lam)))
        ;; the packet's carrier is also an orbit in the delayed time s:
        ;; 7 radial cycles inside one envelope period
        ph (- th (* lp/tau 7.0 s))
        a (* (double amp) env)
        nx (if (> r 1.0e-9) (/ rx r) 0.0) ny (if (> r 1.0e-9) (/ ry r) 0.0)]
    [(+ dx (* a nx (Math/sin ph))) (+ dy (* a ny (Math/sin ph))) (+ z (* a (Math/cos ph)))]))

(def ^:private mesh-spacing 0.2)

(def ^:private mesh
  "Triangular lattice vertices (rest positions) and edges [i j]."
  (delay
    (let [s mesh-spacing h (* s (Math/sqrt 3.0) 0.5)
          cols 48 rows 30
          idx (fn [i j] (+ i (* j cols)))
          verts (vec (for [j (range rows) i (range cols)]
                       [(+ (* s (- i (/ cols 2.0))) (if (odd? j) (* 0.5 s) 0.0))
                        (* h (- j (/ (dec rows) 2.0)))]))
          edges (vec (concat
                      (for [j (range rows) i (range (dec cols))] [(idx i j) (idx (inc i) j)])
                      (for [j (range (dec rows)) i (range cols)] [(idx i j) (idx i (inc j))])
                      (for [j (range (dec rows)) i (range cols)
                            :let [i2 (if (odd? j) (inc i) (dec i))]
                            :when (< -1 i2 cols)]
                        [(idx i j) (idx i2 (inc j))])))]
      {:verts verts :edges edges})))

(def mesh-stops
  "Trough deep blue -> teal -> crest gold, by height: not phase-cycled."
  [[0.10 0.20 1.00] [0.00 0.85 0.85] [1.00 0.62 0.08]])

(defn- height-colour
  "OKLab blend through mesh-stops by height t in [0,1], resaturated like
   cyclic-colour (70% of the minimum channel removed, maximum kept)."
  [^double t]
  (let [t (max 0.0 (min 1.0 t)) x (* 2.0 t) i (min 1 (long x)) f (- x i)
        a (oklab (mesh-stops i)) b (oklab (mesh-stops (inc i)))
        c (oklab->srgb (mapv #(+ (* (- 1.0 f) (double %1)) (* f (double %2))) a b))
        hi (double (apply max c)) lo (* 0.85 (double (apply min c)))
        k (/ hi (max 1.0e-9 (- hi lo)))]
    (mapv #(* k (- (double %) lo)) c)))

(defn mesh-splats
  "Thin glowing edges and bead vertices of the displaced mesh. Light grows
   with height (crests glow, troughs fade) and with local compression of
   the edge (the trochoid bunches parcels into sharp crests)."
  [u]
  (let [{:keys [verts edges]} @mesh
        moved (mapv (fn [[x y]] (let [[dx dy z] (gerstner x y u)]
                                  [(+ (double x) dx) (+ (double y) dy) z]))
                    verts)
        zmax 0.30 seg 7]
    (persistent!
     (reduce
      (fn [out [i j]]
        (let [[ax ay az] (moved i) [bx by bz] (moved j)
              ax (double ax) ay (double ay) bx (double bx) by (double by)
              zm (* 0.5 (+ (double az) (double bz)))
              t (+ 0.5 (* 0.5 (/ zm zmax)))
              len (Math/hypot (- bx ax) (- by ay))
              squeeze (/ (double mesh-spacing) (max 0.05 len))
              ;; smooth taper: smoothstep in height, a soft saturating squeeze
              crest (lp/smoothstep (max 0.0 (min 1.0 t)))
              light (* 0.34 (+ 0.15 (* 0.85 crest crest)) (Math/tanh (* 1.1 squeeze)))
              [cr cg cb] (height-colour t)
              cr (* light (double cr)) cg (* light (double cg)) cb (* light (double cb))]
          (reduce (fn [out k]
                    (let [f (/ (+ 0.5 (double k)) seg)]
                      (conj! out [(+ ax (* f (- bx ax))) (+ ay (* f (- by ay))) 0.0075 cr cg cb])))
                  out (range seg))))
      (reduce (fn [out [x y z]]
                (let [t (+ 0.5 (* 0.5 (/ (double z) zmax)))
                      l (* 0.55 (+ 0.1 (* 0.9 t t t)))
                      [cr cg cb] (height-colour t)]
                  (conj! out [x y 0.016 (* l (double cr)) (* l (double cg)) (* l (double cb))])))
              (transient []) moved)
      edges))))

(def delay-mesh
  {:id :delay-mesh
   :title "Delay mesh: a Gerstner sea, every vertex the same water-parcel circle, delayed by k.x"
   :seconds 20 :fps 30 :size [1920 1080] :view view
   :frame (fn [_] {:background "#010206" :shapes []})
   :splat mesh-splats :exposure 2.4
   :post (fn [_] [{:type :bloom :radius 8 :strength 0.45}
                  {:type :vignette :strength 0.25}])})

(def loops
  "Loop specs for the coordinator's registry."
  (array-map :delay-pendulums delay-pendulums
             :noise-flow-lines noise-flow-lines
             :delay-mesh delay-mesh))
