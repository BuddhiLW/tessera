(ns tessera.flows
  "Incompressible dye, with a fixed view. Trajectories need not close:
   staggered births replay a period-long path and fade the WHOLE streak to
   zero at replacement. Cached arrays are built once, then treated as immutable."
  (:require [desargues.wallpaper.loop :as lp]
            [tessera.palette :as pal]))

(def view [(/ 128.0 9.0) 8.0])

(defn envelope
  "C2 birth/death ramps; exactly zero outside (0,1), including both ends."
  [p]
  (if (or (<= p 0.0) (>= p 1.0))
    0.0
    (let [s (min 1.0 (/ p 0.14) (/ (- 1.0 p) 0.14))]
      (* s s s (+ 10.0 (* s (+ -15.0 (* 6.0 s))))))))

(defn rk4
  "One fourth-order tracer step, for v(time,[x y]). No endpoint correction."
  [v q t h]
  (let [add (fn [a k s] (mapv #(+ %1 (* s %2)) a k))
        a (v t q)
        b (v (+ t (* 0.5 h)) (add q a (* 0.5 h)))
        c (v (+ t (* 0.5 h)) (add q b (* 0.5 h)))
        d (v (+ t h) (add q c h))]
    (mapv #(+ %1 (* (/ h 6.0) (+ %2 (* 2.0 %3) (* 2.0 %4) %5))) q a b c d)))

(defn trace-path
  "Store an open one-period path as interleaved xy doubles, including age 1.
   `substeps` resolves fast vortex orbits inside each uniform table interval."
  [v origin birth steps substeps]
  (let [out (double-array (* 2 (inc steps))) h (/ 1.0 steps)]
    (loop [i 0 q origin]
      (aset-double out (* 2 i) (double (first q)))
      (aset-double out (inc (* 2 i)) (double (second q)))
      (if (= i steps)
        out
        (let [n (long (max 1 (substeps q h))) dt (/ h n)
              t (+ birth (* i h))
              next-q (loop [j 0 p q]
                       (if (= j n) p
                           (recur (inc j) (rk4 v p (+ t (* j dt)) dt))))]
          (recur (inc i) next-q))))))

(defn path-point
  "Interpolate by AGE, clamped not wrapped. Open paths must never be closed
   by interpolation from death back to birth. Lifetime visibility hides it."
  [^doubles path p]
  (let [n (dec (quot (alength path) 2))
        t (* n (max 0.0 (min 1.0 (double p))))
        i (min (dec n) (long (Math/floor t))) f (- t i) k (* 2 i)]
    [(+ (* (- 1.0 f) (aget path k)) (* f (aget path (+ k 2))))
     (+ (* (- 1.0 f) (aget path (inc k))) (* f (aget path (+ k 3))))]))

(def pigments
  "Restricted teal/ice and copper palettes, sampled by dye ORIGIN, not time."
  [(pal/cosine [0.20 0.66 0.79] [0.12 0.22 0.16]
               [1.0 1.0 1.0] [0.0 0.08 0.18])
   (pal/cosine [0.88 0.48 0.16] [0.10 0.22 0.10]
               [1.0 1.0 1.0] [0.0 0.10 0.20])])

(defn pigment [family phase]
  (let [c (pal/at (pigments family) (double phase))]
    (mapv #(/ (double (bit-and 255 (bit-shift-right c %))) 255.0) [16 8 0])))

(def curl-modes
  "[nx ny temporal-frequency amplitude phase]. Eight asymmetric integer
   wavevectors on the view torus: no paired product modes or rigid cell grid."
  [[1 1 0 1.65 0.37] [2 -1 1 1.12 2.11]
   [3 2 -1 0.68 4.32] [1 -3 2 0.53 1.73]
   [4 1 -2 0.31 5.19] [2 3 1 0.39 3.07]
   [5 -2 3 0.20 0.91] [3 -4 -1 0.17 2.63]])

(defn stream-function
  "psi(x,y,t), exactly 1-periodic in t. v = (d_y psi, -d_x psi)."
  [t [x y]]
  (reduce (fn [acc [nx ny m a phase]]
            (+ acc (* a (Math/cos (+ (* lp/tau (+ (/ (* nx x) (first view))
                                                    (/ (* ny y) (second view)) (* m t)))
                                      phase)))))
          0.0 curl-modes))

(defn curl-velocity
  "Analytic curl of psi, hence divergence-free (no finite-difference noise)."
  [t [x y]]
  (reduce (fn [[vx vy] [nx ny m a phase]]
            (let [kx (/ (* lp/tau nx) (first view))
                  ky (/ (* lp/tau ny) (second view))
                  s (* a (Math/sin (+ (* kx x) (* ky y) (* lp/tau m t) phase)))]
              [(- vx (* ky s)) (+ vy (* kx s))]))
          [0.0 0.0] curl-modes))

(def curl-particles
  (delay
    (let [rng (java.util.Random. 481516)]
      (mapv (fn [i]
              (let [offset (lp/frac (* 0.618033988749895 i))
                    origin [(* (first view) (- (.nextDouble rng) 0.5))
                            (* (second view) (- (.nextDouble rng) 0.5))]]
                {:offset offset :origin origin
                 :rgb (pigment (if (pos? (second origin)) 0 1) (.nextDouble rng))
                 :path (trace-path (fn [t q] (mapv #(* 4.0 %) (curl-velocity t q)))
                                   origin (- offset) 512 (fn [_ _] 1))}))
            (range 2560)))))

(defn periodic-kernel
  "Velocity per unit circulation on the x-periodic cylinder. This is the
   derivative of log|sin(pi z/L)|, NOT a finite sum of Euclidean neighbours.
   Positive circulation is counterclockwise. Point cores are not softened."
  [length [dx dy]]
  (let [a (/ Math/PI length) sx (Math/sin (* a dx)) sh (Math/sinh (* a dy))
        d (* 2.0 (+ (* sx sx) (* sh sh)))]
    (when (zero? d) (throw (ex-info "Point-vortex collision" {:delta [dx dy]})))
    [(/ (- (Math/sinh (* 2.0 a dy))) (* 2.0 length d))
     (/ (Math/sin (* 2.0 a dx)) (* 2.0 length d))]))

(defn vortex-hamiltonian
  "Kirchhoff H on the cylinder: -sum(i<j) Gi Gj log(2Dij)/(4 pi),
   Dij = cosh(2 pi dy/L)-cos(2 pi dx/L). Near a core, 2D is proportional
   to r^2. Constants in the periodic Green's function have no dynamics."
  [length strengths positions]
  (let [a (/ Math/PI length)]
    (reduce +
            (for [i (range (count positions)) j (range (inc i) (count positions))
                  :let [[dx dy] (mapv - (positions i) (positions j))
                        sx (Math/sin (* a dx)) sh (Math/sinh (* a dy))]]
              (* (/ (- (* (strengths i) (strengths j))) (* 4.0 Math/PI))
                 (Math/log (* 4.0 (+ (* sx sx) (* sh sh)))))))))

(defn vortex-velocities [length strengths positions]
  (mapv (fn [i]
          (reduce (fn [[vx vy] j]
                    (let [[kx ky] (periodic-kernel length (mapv - (positions i) (positions j)))
                          g (strengths j)]
                      [(+ vx (* g kx)) (+ vy (* g ky))]))
                  [0.0 0.0] (remove #{i} (range (count positions)))))
        (range (count positions))))

(defn vortex-midpoint
  "Implicit midpoint: symplectic for the constant Kirchhoff Poisson tensor.
   Refuse a failed fixed-point solve rather than silently changing the physics."
  [length strengths q h]
  (loop [next-q q iteration 0]
    (let [mid (mapv (fn [a b] (mapv #(* 0.5 (+ %1 %2)) a b)) q next-q)
          v (vortex-velocities length strengths mid)
          out (mapv (fn [a b] (mapv #(+ %1 (* h %2)) a b)) q v)
          err (reduce max 0.0 (map #(Math/abs (double %)) (mapcat #(map - %1 %2) out next-q)))]
      (cond
        (< err 1.0e-13) out
        (>= iteration 60) (throw (ex-info "Vortex midpoint did not converge; reduce h" {:h h :error err}))
        :else (recur out (inc iteration))))))

(def street
  "Classical staggered von Karman street, cosh(pi h/L)=sqrt(2).
   Three full cells span the fixed view. One +/-Gamma pair per cell represents
   both INFINITE rows. U=Gamma*tanh(pi h/L)/(2L), T=L/U."
  (let [length (/ (first view) 3.0) gamma 4.0
        height (* (/ length Math/PI) (Math/log (+ 1.0 (Math/sqrt 2.0))))
        speed (/ (* gamma (Math/tanh (/ (* Math/PI height) length))) (* 2.0 length))]
    {:length length :height height :strengths [gamma (- gamma)]
     :positions [[0.0 (* 0.5 height)] [(* 0.5 length) (* -0.5 height)]]
     :speed speed :period (/ length speed)}))

(defn street-vortices
  "Analytic relative equilibrium in the laboratory frame. Physical t,
   unwrapped x; callers tile by L. No camera or configuration rotation."
  [t]
  (mapv (fn [[x y]] [(+ x (* (:speed street) t)) y]) (:positions street)))

(defn street-velocity
  "Tracer velocity in the translating street frame, per unit loop phase.
   Exact singular kernel; the passive dye has no back-reaction or softening."
  [_ q]
  (let [[vx vy] (reduce (fn [[vx vy] [p gamma]]
                          (let [[kx ky] (periodic-kernel (:length street) (mapv - q p))]
                            [(+ vx (* gamma kx)) (+ vy (* gamma ky))]))
                        [0.0 0.0] (map vector (:positions street) (:strengths street)))
        period (:period street)]
    [(* period (- vx (:speed street))) (* period vy)]))

(defn street-substeps [q h]
  (let [length (:length street)
        r2 (reduce min
                   (map (fn [[x y]]
                          (let [dx (- (first q) x)
                                dx (- dx (* length (Math/rint (/ dx length))))
                                dy (- (second q) y)]
                            (+ (* dx dx) (* dy dy)))) (:positions street)))
        angle (/ (* (:period street) 4.0 h) (* lp/tau (max 1.0e-8 r2)))]
    (long (Math/ceil (/ angle 0.10)))))

(def street-particles
  (delay
    (let [rng (java.util.Random. 271828) length (:length street)]
      (mapv (fn [i]
              (let [col (mod i 48) row (quot i 48)
                    ;; Stratified dye covers the WHOLE cylinder, not just core rings.
                    origin [(* length (/ (+ col 0.15 (* 0.7 (.nextDouble rng))) 48.0))
                            (* 8.0 (- (/ (+ row 0.15 (* 0.7 (.nextDouble rng))) 32.0) 0.5))]
                    origin (if (> (street-substeps origin (/ 1.0 768.0)) 8)
                             (update origin 1 + (if (pos? (second origin)) 0.26 -0.26)) origin)
                    band (long (Math/floor (/ (+ 4.0 (second origin)) 0.42)))
                    family (mod band 2)]
                {:offset (lp/frac (* 0.618033988749895 i)) :origin origin :band band
                 :rgb (pigment family (/ (first origin) length))
                 :path (trace-path street-velocity origin 0.0 768 street-substeps)}))
            (range 1536)))))

(defn tile-points
  "Canonical periodic coordinates and their visible neighbouring images.
   The halo margin hides raster support at cell edges, without duplicate light.
   `window` is the scene window shown, `view` unless a wider frame asks."
  ([q tile drift] (tile-points q tile drift view))
  ([[x y] [width height] [drift-x drift-y] window]
   (let [wrap (fn [v period] (- v (* period (Math/floor (+ 0.5 (/ v period))))))
         x (double (wrap (+ x drift-x) width))
         y (double (if height (wrap (+ y drift-y) height) (+ y drift-y)))
         width (double width) height (double (or height 0.0))
         [vw vh] window
         reach (inc (long (Math/ceil (/ vw (* 2.0 width)))))]
     (for [i (range (- reach) (inc reach))
           j (if (pos? height) [-1 0 1] [0])
           :let [px (+ x (* (double i) width)) py (+ y (* (double j) height))]
           :when (and (< (Math/abs (double px)) (+ 0.12 (* 0.5 vw)))
                      (< (Math/abs (double py)) (+ 0.12 (* 0.5 vh))))]
       [px py]))))

(defn streak-rows
  "Replacement is per particle: p=frac(u+offset), origin and birth phase
   repeat every loop. Both current-age AND tail-age envelopes multiply the
   entire streak; no old tail can flash when an open trajectory resets."
  [particles u {:keys [tile drift tail samples radius power window]
                :or {drift 0.0 tail 0.040 samples 14 radius 0.011 power 0.46 window view}}]
  (let [u (lp/frac (double u))]
    (persistent!
     (reduce
      (fn [out {:keys [offset path rgb]}]
        (let [p (lp/frac (+ u offset)) visibility (envelope p)]
          (if (< visibility 1.0e-10) out
              (reduce
               (fn [out j]
                 (let [f (/ (double j) (dec samples)) lag (* tail f) age (- p lag)
                       light (* power visibility (envelope age) (Math/exp (* -3.1 f)))]
                   (if (< light 1.0e-9) out
                       (let [[r g b] (mapv #(* light %) rgb)
                             q (path-point path age)]
                         (reduce (fn [out [x y]]
                                   (conj! out [x y (* radius (- 1.0 (* 0.4 f))) r g b]))
                                 out (tile-points q tile [(* drift (- u lag)) 0.0] window))))))
               out (range samples)))))
      (transient []) particles))))

(defn curl-splats [u]
  (streak-rows @curl-particles u {:tile view :tail 0.037 :samples 14 :power 0.42}))

(defn street-splats
  ([u] (street-splats u view))
  ([u window]
   (let [u (lp/frac (double u)) length (:length street)]
     (into (streak-rows @street-particles u
                        {:tile [length nil] :drift length :tail 0.026
                         :samples 16 :radius 0.010 :power 0.34 :window window})
           (mapcat (fn [p family]
                     (mapcat (fn [[x y]]
                               (let [[r g b] (pigment family 0.2)]
                                 [[x y 0.050 (* 0.9 r) (* 0.9 g) (* 0.9 b)]
                                  [x y 0.015 2.0 2.0 1.8]]))
                             (tile-points p [length nil] [(* length u) 0.0] window)))
                   (:positions street) (range 2))))))

(def curl-flow
  {:id :curl-flow :title "Curl flow: copper and glacial dye in periodic eddies"
   :seconds 18 :fps 30 :size [1920 1080] :view view
   :frame (fn [_] {:background "#020710" :shapes []})
   :splat curl-splats :exposure 1.0
   :blur {:samples 3 :shutter 0.65}
   :post (fn [_] [{:type :bloom :radius 9 :strength 0.50}
                  {:type :vignette :strength 0.24}])})

(def vortex-street
  {:id :vortex-street :title "Von Karman street: counter-rotating teal and amber dye"
   :seconds 20 :fps 30 :size [1920 1080] :view view
   :frame (fn [_] {:background "#020710" :shapes []})
   :splat street-splats :exposure 1.0
   :blur {:samples 3 :shutter 0.65}
   :post (fn [_] [{:type :bloom :radius 10 :strength 0.55}
                  {:type :vignette :strength 0.28}])})

(def loops
  "Opt-in loop family. The central tessera.loops registry is left untouched."
  (array-map :curl-flow curl-flow :vortex-street vortex-street))
