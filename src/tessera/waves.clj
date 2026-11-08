(ns tessera.waves
  "Exactly periodic waves.

     :caustics   photons refracted by a toroidal water surface and bilinearly
                 splatted into a periodic pool-floor density buffer
     :breathers  exact sine-Gordon breathers as a portable field form

  All temporal phases are integral turns. The spatial wave vectors are
  integral too. Caustics build one CPU density buffer per frame; breathers
  run unchanged on the :jvm, :raster and :gpu field backends."
  (:require [desargues.wallpaper.field :as fld]
            [desargues.wallpaper.render :as r]))

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
   [3 1 0.014 5 2.0]
   [4 -1 0.008 6 0.6]
   [3 4 0.006 7 2.5]
   [-4 3 0.005 8 1.4]
   [5 2 0.004 9 0.2]])

(defn water-phases
  "Sine/cosine pairs of the twelve temporal phases at loop phase u."
  [u]
  (mapv (fn [[_ _ _ n phase]]
          (let [a (+ (* tau n (double u)) phase)]
            [(Math/sin a) (Math/cos a)]))
        water-waves))

(def refraction-depth 10.0)
(defn water-gradient
  "Gradient of the toroidal water surface. The tenfold effective slope used
  by the splat is the small-slope wave-steepness parameter of this render."
  [u x y]
  (reduce (fn [[gx gy] [[kx ky a _ _] [p q]]]
            (let [angle (+ (* tau (/ kx 8.0) x) (* tau (/ ky 8.0) y))
                  deriv (* a (/ tau 8.0)
                           (- (* q (Math/cos angle)) (* p (Math/sin angle))))]
              [(+ gx (* kx deriv)) (+ gy (* ky deriv))]))
          [0.0 0.0] (map vector water-waves (water-phases u))))

(def ^:private water-data
  (double-array
   (mapcat (fn [[kx ky a _ _]] [(* kx a (/ tau 8.0)) (* ky a (/ tau 8.0))
                               (* kx (/ tau 8.0)) (* ky (/ tau 8.0))])
           water-waves)))

(defn caustic-buffer
  "One photon density period, w*h samples. Two offset source grids are
  refracted with the small-slope Snell ray map and bilinearly splatted.
  Landings wrap on the torus. Mean density is exactly one."
  [u w h depth]
  (let [w (long w) h (long h) depth (double depth)
        bend (* 10.0 depth (- 1.0 (/ 1.0 1.333)))
        out (double-array (* w h))
        phases (double-array (mapcat identity (water-phases u)))
        nw (count water-waves)]
    (dotimes [pass 2]
      (dotimes [sy h]
        (dotimes [sx w]
          (let [jx (if (zero? pass) 0.25 0.75)
                jy (if (zero? pass) 0.75 0.25)
                px (* 8.0 (/ (+ sx jx) w)) py (* 8.0 (/ (+ sy jy) h))]
            (loop [j 0 gx 0.0 gy 0.0]
              (if (= j nw)
                (let [qx (+ px (* bend gx)) qy (+ py (* bend gy))
                      tx (mod (* qx (/ w 8.0)) w) ty (mod (* qy (/ h 8.0)) h)
                      ix (long (Math/floor tx)) iy (long (Math/floor ty))
                      fx (- tx ix) fy (- ty iy)
                      i0 (mod ix w) i1 (mod (inc ix) w)
                      j0 (mod iy h) j1 (mod (inc iy) h)
                      a (+ (* j0 w) i0) b (+ (* j0 w) i1)
                      c (+ (* j1 w) i0) d (+ (* j1 w) i1)]
                  (aset out a (+ (aget out a) (* 0.5 (- 1.0 fx) (- 1.0 fy))))
                  (aset out b (+ (aget out b) (* 0.5 fx (- 1.0 fy))))
                  (aset out c (+ (aget out c) (* 0.5 (- 1.0 fx) fy)))
                  (aset out d (+ (aget out d) (* 0.5 fx fy))))
                (let [o (* 4 j)
                      angle (+ (* (aget ^doubles water-data (+ o 2)) px)
                               (* (aget ^doubles water-data (+ o 3)) py))
                      deriv (- (* (aget phases (inc (* 2 j))) (Math/cos angle))
                               (* (aget phases (* 2 j)) (Math/sin angle)))]
                  (recur (inc j)
                         (+ gx (* (aget ^doubles water-data o) deriv))
                         (+ gy (* (aget ^doubles water-data (inc o)) deriv))))))))))
    out))

(defn caustic-shader
  "Build the refracted photon density once per frame; the shader only samples
  it. The sharp density is retained for folds and a filtered copy supplies glow."
  [u]
  (let [w 1024 h 1024
        ^doubles density (caustic-buffer u w h refraction-depth)
        at (fn [ix iy] (aget density (+ (* (mod (long iy) h) w) (mod (long ix) w))))
        sample (fn ^double [^double x ^double y]
                 (let [tx (mod (* x (/ w 8.0)) w) ty (mod (* y (/ h 8.0)) h)
                       ix (long (Math/floor tx)) iy (long (Math/floor ty))
                       fx (- tx ix) fy (- ty iy)
                       a (at ix iy) b (at (inc ix) iy)
                       c (at ix (inc iy)) d (at (inc ix) (inc iy))]
                   (+ (* a (- 1.0 fx) (- 1.0 fy)) (* b fx (- 1.0 fy))
                      (* c (- 1.0 fx) fy) (* d fx fy))))
        glow (fn ^double [^double x ^double y]
               (* 0.0625 (+ (sample x y)
                            (* 2.0 (+ (sample (+ x 0.025) y) (sample (- x 0.025) y)
                                      (sample x (+ y 0.025)) (sample x (- y 0.025))))
                            (sample (+ x 0.025) (+ y 0.025)) (sample (- x 0.025) (+ y 0.025))
                            (sample (+ x 0.025) (- y 0.025)) (sample (- x 0.025) (- y 0.025)))))]
    (fn ^long [^double x ^double y]
      (let [i (sample x y) g (glow x y)
            line (Math/pow (min 1.0 (* 0.45 i)) 1.2)
            halo (- 1.0 (Math/exp (* -0.045 g)))
            hot (Math/pow (min 1.0 (max 0.0 (/ (- i 3.0) 4.0))) 0.45)
            gold (min 1.0 (max 0.0 (/ (- i 1.8) 1.5)))]
        (desargues.wallpaper.render/rgb
         (+ 0.003 (* 0.025 halo) (* 0.11 (- 1.0 gold) line) (* 0.82 hot) (* 0.95 gold))
         (+ 0.022 (* 0.20 halo) (* 0.47 (- 1.0 gold) line) (* 0.42 hot) (* 0.68 gold))
         (+ 0.09 (* 0.38 halo) (* 0.69 (- 1.0 gold) line) (* 0.08 hot) (* 0.38 gold)))))))

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
