(ns tessera.waves
  "Exactly periodic waves as portable desargues field forms.

     :caustics   vertical rays refracted by a toroidal water surface, with
                 the pool-floor light given by the inverse ray-map Jacobian
     :breathers  an array of exact sine-Gordon breathers as luminous ribbons

  All temporal phases are integral turns. The spatial wave vectors are
  integral too. Both forms run unchanged on the :jvm, :raster and :gpu field
  backends; only their per-frame scalar phases are bound on the host."
  (:require [desargues.wallpaper.field :as fld]))

(set! *warn-on-reflection* true)

(def ^:private tau (* 2.0 Math/PI))

(defn- form [params bindings rgb]
  {:params (vec params) :body (list 'let (vec bindings) (vec rgb))})

(defn- sum [terms] (cons '+ terms))
(defn- pow2 [x] (list '* x x))

;; Caustics ---------------------------------------------------------------

(def water-waves
  "Wave-vector x, wave-vector y, amplitude, cycles per loop, initial phase.
  The torus is eight scene units square. Amplitudes include a mild directional
  anisotropy, while the two travelling directions keep the surface lively."
  [[1 0 0.072 1 0.0]
   [0 1 0.061 1 1.1]
   [1 1 0.049 2 2.2]
   [1 -1 0.043 2 0.4]
   [2 1 0.031 3 1.7]
   [-1 2 0.028 3 2.9]
   [2 -2 0.023 4 0.8]
   [3 1 0.014 5 2.0]])

(defn water-phases
  "Sine/cosine pairs of the eight temporal phases at loop phase u."
  [u]
  (mapv (fn [[_ _ _ n phase]]
          (let [a (+ (* tau n (double u)) phase)]
            [(Math/sin a) (Math/cos a)]))
        water-waves))

(defn- wave-bindings []
  (vec
   (mapcat
    (fn [j [kx ky _ _ _]]
      (let [spatial (list '+ (list '* (/ (* tau kx) 8.0) 'x)
                          (list '* (/ (* tau ky) 8.0) 'y))
            s (symbol (str "s" j)) c (symbol (str "c" j)) w (symbol (str "w" j))]
        [s (list 'sin spatial)
         w (list 'cos spatial)
         c (list '- (list '* w (symbol (str "q" j)))
                 (list '* s (symbol (str "p" j))))]))
    (range) water-waves)))

(defn- surface-derivatives []
  (let [weighted (fn [f]
                   (sum (map-indexed
                         (fn [j [kx ky a _ _]]
                           (let [k (/ tau 8.0)]
                             (list '* (f kx ky) (* a k k) (symbol (str "c" j)))))
                         water-waves)))]
    ['gx (weighted (fn [kx _] kx))
     'gy (weighted (fn [_ ky] ky))
     'hxx (weighted (fn [kx _] (- (* kx kx))))
     'hyy (weighted (fn [_ ky] (- (* ky ky))))
     'hxy (weighted (fn [kx ky] (- (* kx ky))))]))

(def refraction-depth 2.9)
(def air-water-ratio (/ 1.0 1.333))

(defn water-ray
  "Snell-refracted landing point of the vertical ray through (x,y), plus
  the exact Jacobian determinant of the floor map. This is the same formula
  as caustic-field, exposed independently of the compiler's renamed lets."
  [u x y]
  (let [ds (mapv (fn [[kx ky _ _ _] [p q]]
                   (let [a (+ (* tau (/ kx 8.0) x) (* tau (/ ky 8.0) y))]
                     (- (* q (Math/cos a)) (* p (Math/sin a)))))
                 water-waves (water-phases u))
        k (/ tau 8.0)
        grad (fn [select] (reduce + (map (fn [[kx ky a _ _] d] (* (select kx ky) a k k d)) water-waves ds)))
        gx (grad (fn [a _] a)) gy (grad (fn [_ b] b))
        hxx (grad (fn [a _] (- (* a a)))) hyy (grad (fn [_ b] (- (* b b))))
        hxy (grad (fn [a b] (- (* a b))))
        g (+ (* gx gx) (* gy gy)) n2 (+ 1.0 g)
        root (Math/sqrt (- 1.0 (/ (* air-water-ratio air-water-ratio g) n2)))
        f (- root (/ air-water-ratio n2))
        fp (/ (- air-water-ratio (/ (* air-water-ratio air-water-ratio) (* 2.0 root)))
              (* n2 n2))
        dgx (* 2.0 (+ (* hxx gx) (* hxy gy)))
        dgy (* 2.0 (+ (* hxy gx) (* hyy gy)))
        a (+ (* hxx f) (* gx fp dgx)) b (+ (* hyy f) (* gy fp dgy))
        c (+ (* hxy f) (* gx fp dgy)) d (+ (* hxy f) (* gy fp dgx))
        det (- (* (+ 1.0 (* refraction-depth a)) (+ 1.0 (* refraction-depth b)))
               (* refraction-depth refraction-depth c d))]
    {:det det :point [(+ x (* refraction-depth gx f))
                      (+ y (* refraction-depth gy f))]}))

(def caustic-field
  "Refract d=(0,0,-1) through n=(-hx,-hy,1)/|n| with air/water ratio
  eta=1/1.333. The transmitted horizontal direction is gradient(h)*f(g),
  g=|gradient(h)|^2. f includes Snell's exact nonlinear normal correction.
  For g=|gradient(h)|^2, dg=2*H*gradient(h). Each landing displacement is
  depth*gradient(h)_j*f(g), so its derivative includes both the Hessian and
  depth*gradient(h)_j*f'(g)*dg. I+that full 2x2 matrix gives det(J).
  Intensity is 1/|det(J)|; the three exponential scales render its sharp
  gold ridges and aqua penumbra without replacing the physics by stripes."
  (let [eta (/ 1.0 1.333) depth 2.9
        derivative-bindings
        ['g (list '+ (pow2 'gx) (pow2 'gy))
         'n2 (list '+ 1.0 'g)
         'n4 (pow2 'n2)
         'root (list 'sqrt (list '- 1.0 (list '/ (list '* eta eta 'g) 'n2)))
         'f (list '- 'root (list '/ eta 'n2))
         'fp (list '/ (list '- eta
                              (list '/ (* eta eta) (list '* 2.0 'root))) 'n4)
         'dgx (list '* 2.0 (list '+ (list '* 'hxx 'gx) (list '* 'hxy 'gy)))
         'dgy (list '* 2.0 (list '+ (list '* 'hxy 'gx) (list '* 'hyy 'gy)))
         'a (list '+ (list '* 'hxx 'f) (list '* 'gx 'fp 'dgx))
         'b (list '+ (list '* 'hyy 'f) (list '* 'gy 'fp 'dgy))
         'c (list '+ (list '* 'hxy 'f) (list '* 'gx 'fp 'dgy))
         'd (list '+ (list '* 'hxy 'f) (list '* 'gy 'fp 'dgx))
         'det (list '- (list '* (list '+ 1.0 (list '* depth 'a))
                               (list '+ 1.0 (list '* depth 'b)))
                     (list '* depth depth 'c 'd))
         'ad (list 'abs 'det)
         'focus (list '/ 1.0 (list '+ 0.025 'ad))
         'ambient (list '/ (list 'log (list '+ 1.0 'focus)) 4.0)
         'halo (list '* 0.16 (list 'exp (list '* -7.0 'ad)))
         'line (list '* 0.38 (list 'exp (list '* -46.0 'ad)))
         'core (list '* 0.74 (list 'exp (list '* -270.0 'ad)))]]
    (form (mapcat (fn [j] [(symbol (str "p" j)) (symbol (str "q" j))]) (range 8))
          (concat (wave-bindings) (surface-derivatives) derivative-bindings)
          [(list '+ 0.0015 (list '* 0.018 'ambient) (list '* 0.06 'halo)
                 (list '* 0.09 'line) (list '* 0.95 'core))
           (list '+ 0.006 (list '* 0.12 'ambient) (list '* 0.60 'halo)
                 (list '* 0.63 'line) (list '* 0.72 'core))
           (list '+ 0.032 (list '* 0.29 'ambient) (list '* 0.88 'halo)
                 (list '* 0.82 'line) (list '* 0.22 'core))])))

(defn- caustic-shader [u]
  (apply fld/bind caustic-field (mapcat identity (water-phases u))))

(def caustics
  {:id :caustics
   :title "Water caustics: Snell-refracted sunlight focused into aqua and gold"
   :seconds 16 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0] :ss 2
   :field caustic-shader
   :post (fn [_] [{:type :bloom :radius 9 :strength 0.5}
                  {:type :vignette :strength 0.22}])})

;; Sine-Gordon breathers --------------------------------------------------

(def breather-rows
  "Temporal cycles n in each row. w=n/sqrt(n^2+25) is commensurate; the
  internal frequency is n cycles per loop and sqrt(1-w^2)/w=5/n."
  [2 3 4])

(defn breather-value
  "Exact 1+1 sine-Gordon breather at x, with w=n/sqrt(n^2+25). Time is
  scaled by n/w so sin(wt)=sin(2*pi*n*u), an integral number of oscillations."
  [u n x]
  (let [w (/ (double n) (Math/sqrt (+ (* n n) 25.0)))
        k (/ (Math/sqrt (- 1.0 (* w w))) w)]
    (* 4.0 (Math/atan (/ (* k (Math/sin (* tau n (double u))))
                         (Math/cosh (* k (double x))))))))

(defn- breather-bindings []
  (vec
   (mapcat
    identity
    (for [row (range 3) col (range 4)
          :let [n (breather-rows row)
                w (/ (double n) (Math/sqrt (+ (* n n) 25.0)))
                k (/ (Math/sqrt (- 1.0 (* w w))) w)
                j (+ (* 4 row) col)
                v (symbol (str "v" j))
                b (symbol (str "b" j))
                z (list '* k (list '- 'x (* 3.6 (- col 1.5))))
                dy (list '- 'y (* 2.55 (- row 1)))
                env (list 'exp (list '* -3.2 (pow2 dy)))
                a (list '/ (list '* k (symbol (str "p" j)))
                        (list '* 0.5 (list '+ (list 'exp z)
                                           (list 'exp (list '- z)))))]]
      [v (list '* (/ 4.0 Math/PI) (list 'atan2 a 1.0) env)
       b (pow2 v)]))))

(def breather-field
  "Twelve independent 1+1 breathers, extended across y only as luminous
  ribbon envelopes. atan and cosh are expanded with the closed field
  vocabulary. The value is the exact breather, not a Gaussian pulse."
  (let [bindings (breather-bindings)
        values (map #(symbol (str "v" %)) (range 12))
        bodies (map #(symbol (str "b" %)) (range 12))
        stripes (map (fn [v b]
                       (list '* b (list 'pow
                                        (list '+ 0.5 (list '* 0.5 (list 'cos (list '* 19.0 v)))) 14.0)))
                     values bodies)
        energy (sum bodies)
        hot (list 'pow (list 'min 1.0 energy) 3.0)
        stripe (sum stripes)
        rows (map (fn [row] (sum (subvec (vec stripes) (* 4 row) (* 4 (inc row))))) (range 3))]
    (form (map #(symbol (str "p" %)) (range 12))
          (concat bindings ['energy energy 'hot hot 'stripe stripe])
          [(list '+ 0.004 (list '* 0.28 (nth rows 0)) (list '* 0.62 (nth rows 1))
                 (list '* 0.10 (nth rows 2)) (list '* 0.09 'energy) (list '* 0.28 'hot))
           (list '+ 0.003 (list '* 0.07 (nth rows 0)) (list '* 0.43 (nth rows 1))
                 (list '* 0.66 (nth rows 2)) (list '* 0.04 'energy) (list '* 0.23 'hot))
           (list '+ 0.018 (list '* 0.40 (nth rows 0)) (list '* 0.13 (nth rows 1))
                 (list '* 0.72 (nth rows 2)) (list '* 0.15 'energy) (list '* 0.20 'hot))])))

(defn- breather-shader [u]
  (apply fld/bind breather-field
         (for [row (range 3) col (range 4)
               :let [n (breather-rows row)
                     phase (if (even? (+ row col)) 0.0 (* 0.5 Math/PI))]]
           (Math/sin (+ (* tau n (double u)) phase)))))

(def breathers
  {:id :breathers
   :title "Sine-Gordon breathers: a lattice of exact, luminous oscillations"
   :seconds 12 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0] :ss 2
   :field breather-shader
   :post (fn [_] [{:type :bloom :radius 11 :strength 0.65}
                  {:type :vignette :strength 0.3}])})

(def loops
  "The wave gallery, keyed by loop id."
  {:caustics caustics
   :breathers breathers})
