(ns tessera.waves
  "Exactly periodic waves.

     :caustics       photons refracted by a toroidal water surface and
                     bilinearly splatted into a periodic pool-floor buffer
     :caustics-gold  a finer caustic network in a warm gold palette
     :breathers      a lattice of exact sine-Gordon breathers rendered as
                     glowing level sets of their analytic height field

  All temporal phases are integral turns. The spatial wave vectors are
  integral too. Caustics build one CPU density buffer per frame; breathers
  evaluate the closed-form breather and its slope per pixel."
  (:require [desargues.wallpaper.render :as r]))

(set! *warn-on-reflection* true)

(def ^:private tau (* 2.0 Math/PI))

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

(def gold-water-waves
  "A finer network: eighteen modes, wave vectors up to |k| = 8, shorter
  wavelengths and smaller amplitudes so the folds stay thin and dense.
  The base modes match water-waves so the gold variant breathes at the
  same slow rhythm; the added high modes supply the fine filigree."
  (vec (concat water-waves
               [[3 -3 0.0075 5 1.9]
                [4 2 0.0068 6 0.3]
                [-2 4 0.0062 7 2.7]
                [5 -1 0.0055 8 1.2]
                [4 4 0.0048 9 2.1]
                [6 2 0.0040 10 0.9]])))

(defn water-phases
  "Sine/cosine pairs of the temporal phases at loop phase u, one pair per
  wave. With no argument, the base twelve; pass gold-water-waves for the
  finer set."
  ([u] (water-phases u water-waves))
  ([u waves]
   (mapv (fn [[_ _ _ n phase]]
           (let [a (+ (* tau n (double u)) phase)]
             [(Math/sin a) (Math/cos a)]))
         waves)))

(def refraction-depth 10.0)
(defn water-gradient
  "Gradient of the toroidal water surface. The tenfold effective slope used
  by the splat is the small-slope wave-steepness parameter of this render."
  ([u x y] (water-gradient u x y water-waves))
  ([u x y waves]
   (reduce (fn [[gx gy] [[kx ky a _ _] [p q]]]
             (let [angle (+ (* tau (/ kx 8.0) x) (* tau (/ ky 8.0) y))
                   deriv (* a (/ tau 8.0)
                            (- (* q (Math/cos angle)) (* p (Math/sin angle))))]
               [(+ gx (* kx deriv)) (+ gy (* ky deriv))]))
           [0.0 0.0] (map vector waves (water-phases u waves)))))

(defn- wave-data
  "Packed per-wave constants [kx*a*tau/8 ky*a*tau/8 kx*tau/8 ky*tau/8]."
  [waves]
  (double-array
   (mapcat (fn [[kx ky a _ _]] [(* kx a (/ tau 8.0)) (* ky a (/ tau 8.0))
                                (* kx (/ tau 8.0)) (* ky (/ tau 8.0))])
           waves)))

(def ^:private water-data (wave-data water-waves))
(def ^:private gold-water-data (wave-data gold-water-waves))

(defn caustic-buffer
  "One photon density period, w*h samples. Two offset source grids are
  refracted with the small-slope Snell ray map and bilinearly splatted.
  Landings wrap on the torus. Mean density is exactly one. The optional
  fifth argument selects the wave set (default water-waves)."
  ([u w h depth] (caustic-buffer u w h depth water-waves))
  ([u w h depth waves]
   (let [w (long w) h (long h) depth (double depth)
         bend (* 10.0 depth (- 1.0 (/ 1.0 1.333)))
         out (double-array (* w h))
         phases (double-array (mapcat identity (water-phases u waves)))
         ^doubles data (if (identical? waves gold-water-waves) gold-water-data water-data)
         nw (count waves)]
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
                       angle (+ (* (aget data (+ o 2)) px)
                                (* (aget data (+ o 3)) py))
                       deriv (- (* (aget phases (inc (* 2 j))) (Math/cos angle))
                                (* (aget phases (* 2 j)) (Math/sin angle)))]
                   (recur (inc j)
                          (+ gx (* (aget data o) deriv))
                          (+ gy (* (aget data (inc o)) deriv))))))))))
     out)))


(def aqua-palette
  "Deep teal ground, aqua folds, white-hot caustic hearts, gilded ridges."
  {:ground [0.003 0.022 0.09]
   :halo [0.025 0.20 0.38]
   :line [0.11 0.47 0.69]
   :hot [0.82 0.42 0.08]
   :gold [0.95 0.68 0.38]})

(def gold-palette
  "Warm gold on near-black bronze: amber folds, pale champagne hearts."
  {:ground [0.012 0.006 0.002]
   :halo [0.16 0.085 0.020]
   :line [0.55 0.33 0.10]
   :hot [1.00 0.86 0.52]
   :gold [1.00 0.74 0.30]})

(defn caustic-shader
  "Build the refracted photon density once per frame; the shader only samples
  it. The sharp density is retained for folds and a filtered copy supplies
  glow. Optional args: the wave set (default water-waves), the palette, and
  the density-buffer resolution (default 1024; 1536 for the finer network)."
  ([u] (caustic-shader u water-waves aqua-palette 1024))
  ([u waves palette resolution]
   (let [w (long resolution) h (long resolution)
         ^doubles density (caustic-buffer u w h refraction-depth waves)
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
                             (sample (+ x 0.025) (- y 0.025)) (sample (- x 0.025) (- y 0.025)))))
         [gr gg gb] (:ground palette)
         [hr hg hb] (:halo palette)
         [lr lg lb] (:line palette)
         [or og ob] (:hot palette)
         [kr kg kb] (:gold palette)]
     (fn ^long [^double x ^double y]
       (let [i (sample x y) g (glow x y)
             line (Math/pow (min 1.0 (* 0.45 i)) 1.2)
             halo (- 1.0 (Math/exp (* -0.045 g)))
             hot (Math/pow (min 1.0 (max 0.0 (/ (- i 3.0) 4.0))) 0.45)
             gold (min 1.0 (max 0.0 (/ (- i 1.8) 1.5)))]
         (r/rgb (+ gr (* hr halo) (* lr (- 1.0 gold) line) (* or hot) (* kr gold))
                (+ gg (* hg halo) (* lg (- 1.0 gold) line) (* og hot) (* kg gold))
                (+ gb (* hb halo) (* lb (- 1.0 gold) line) (* ob hot) (* kb gold))))))))

(def caustics
  {:id :caustics
   :title "Water caustics: Snell-refracted sunlight focused into aqua and gold"
   :seconds 16 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0] :ss 2
   :field caustic-shader
   :post (fn [_] [{:type :bloom :radius 9 :strength 0.5}
                  {:type :vignette :strength 0.22}])})

(def caustics-gold
  {:id :caustics-gold
   :title "Gold caustics: a finer eighteen-wave Snell network, amber folds and champagne hearts on bronze"
   :seconds 16 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0] :ss 2
   :field (fn [u] (caustic-shader u gold-water-waves gold-palette 1536))
   :post (fn [_] [{:type :bloom :radius 8 :strength 0.55}
                  {:type :vignette :strength 0.26}])})

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

(defn- breather-u-and-slope
  "Exact breather value u and its x-slope du/dx at offset dx, with
  s = sin(wt) already evaluated. k = sqrt(1-w^2)/w.
  u  = 4 atan(k s / cosh(k dx))
  du = -4 k^2 s sinh(k dx) / (cosh^2(k dx) (1 + (k s / cosh(k dx))^2))."
  [^double k ^double s ^double dx]
  (let [z (* k dx)
        c (Math/cosh z) sh (Math/sinh z)
        a (/ (* k s) c)
        denom (+ 1.0 (* a a))]
    [(* 4.0 (Math/atan a))
     (* -4.0 (/ (* k k s sh) (* c c denom)))]))

(defn breather-lattice-of
  "Three rows by `cols` columns (even) of exact breathers, columns 3.6 apart
   about x = 0, rows at y = -2.55, 0, 2.55. Row r uses n = (2,3,4). Phases
   alternate 0 and pi/2 across the checkerboard, so neighbouring breathers
   breathe in quadrature and the whole lattice never goes flat; the
   checkerboard is anchored at the centre, so a wider lattice extends the
   narrower one."
  [cols]
  (let [half (quot (long cols) 2)]
    (vec (for [row (range 3) col (range cols)]
           (let [n (breather-rows row)
                 j (- col half)]
             {:n n :k (/ 5.0 n)
              :x0 (* 3.6 (+ j 0.5)) :y0 (* 2.55 (- row 1))
              :phase (if (even? (+ row j)) 0.0 (* 0.5 Math/PI))})))))

(def breather-lattice
  "The 16:9 lattice: three rows by four columns, at x = -5.4,-1.8,1.8,5.4."
  (vec (for [row (range 3) col (range 4)]
         (let [n (breather-rows row)]
           {:n n :k (/ 5.0 n)
            :x0 (* 3.6 (- col 1.5)) :y0 (* 2.55 (- row 1))
            :phase (if (even? (+ row col)) 0.0 (* 0.5 Math/PI))}))))

(defn- breather-height
  "Lit height field h(x,y) = sum_i u_i(x) exp(-0.55 (y-y_i)^2), with its
  analytic gradient. Returns [h dh/dx dh/dy]."
  [lattice u ^double x ^double y]
  (reduce (fn [[h gx gy] {:keys [n k x0 y0 phase]}]
            (let [s (Math/sin (+ (* tau n (double u)) phase))
                  dx (- x x0) dy (- y y0)
                  env (Math/exp (* -0.55 (* dy dy)))
                  denv (* -1.1 dy env)
                  [v dv] (breather-u-and-slope k s dx)]
              [(+ h (* v env)) (+ gx (* dv env)) (+ gy (* v denv))]))
          [0.0 0.0 0.0] lattice))

(def breather-line-width
  "Widest a level line may draw, in scene units (about 7 px at 1080 rows).
   Lines on a steep slope are thinner than this and are left as they were."
  0.05)

(defn breather-shader
  "The lattice at phase `u`; `lattice` defaults to the 16:9 one."
  [u & [lattice]]
  (fn ^long [^double x ^double y]
    (let [[h gx gy] (breather-height (or lattice breather-lattice) u x y)
          ah (Math/abs h)
          warm (if (pos? h) 1.0 0.0)
          ;; Glowing level sets of the signed height: the zero level draws
          ;; the nodal network, positive levels warm, negative levels cool.
          lv (* h 2.2)
          frac (- lv (Math/floor lv))
          dist (min frac (- 1.0 frac))
          line (Math/exp (* -500.0 (* dist dist)))
          grad (Math/sqrt (+ (* gx gx) (* gy gy)))
          ;; Cap each line at breather-line-width scene units across. A
          ;; level line is dist/2.2/grad wide on screen, so where the field
          ;; goes flat (h = 0 over every zero-phase breather at u = 0, 1/4,
          ;; 1/2, 3/4) the zero level would flood whole regions for one
          ;; frame. That flash was the u = 0 frame, so it was the seam.
          dh (/ dist 2.2)
          wq (* breather-line-width grad)
          cap (Math/exp (- (/ (* dh dh) (+ (* wq wq) 1e-12))))
          aa (/ 1.0 (+ 1.0 (* 0.04 grad)))
          line (* line aa cap)
          body (* 0.05 (Math/pow (min 1.0 (/ ah 4.5)) 1.8))
          spec (Math/pow (max 0.0 (/ (+ (* -0.5 gx) (* 0.866 gy)) (+ 1.0 grad))) 3.0)]
      (r/rgb (+ 0.001 (* body 0.95) (* line (+ 0.40 (* 0.55 warm))) (* 0.10 spec))
             (+ 0.0015 (* body 0.80) (* line (+ 0.60 (* 0.18 warm))) (* 0.16 spec))
             (+ 0.005 (* body 1.05) (* line (+ 0.95 (* -0.60 warm))) (* 0.26 spec))))))

(def breathers
  {:id :breathers
   :title "Sine-Gordon breathers: glowing level sets of an exact breather lattice, warm peaks and cool troughs"
   :seconds 12 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0] :ss 2
   :field breather-shader
   :post (fn [_] [{:type :bloom :radius 11 :strength 0.65}
                  {:type :vignette :strength 0.3}])})

(def loops
  "The wave gallery, keyed by loop id."
  {:caustics caustics
   :caustics-gold caustics-gold
   :breathers breathers})
