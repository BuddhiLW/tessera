(ns tessera.orbitals
  "Exactly periodic bound states and resonances.

     :hydrogen-beat  normalized 3s, 4d, 4f and 5g eigenstates on the xz
                     plane, at their 1800-time-unit common period. Log
                     density and luminous iso-density contours reveal the
                     radial rings and the angular lobes.
     :box-modes     five commensurate eigenstates of a 16 by 9 infinite well.
                     m^2+n^2 = 5,13,20,26,32 cycles per loop. |psi|^2 glows
                     in phase colour with soft iso-density contours.
     :drum          Pythagorean square-membrane modes, tiled by odd
                     reflection to fill the frame with continuous nodal lines.

  Hydrogen states are normalized in R^3; the slice itself is not a 2D state.
  The box modes are normalized on [0,1]^2. Frame-level work precomputes
  phases and sine tables; the pixel path is primitive arithmetic and arrays."
  (:require [desargues.wallpaper.render :as r]))

(set! *warn-on-reflection* true)

(def ^:private tau (* 2.0 Math/PI))
(def ^:private view [(/ 128.0 9.0) 8.0])

(defn- hue->rgb
  "Full-saturation cosine colour wheel; h is in turns."
  [^double h]
  [(+ 0.5 (* 0.5 (Math/cos (* tau h))))
   (+ 0.5 (* 0.5 (Math/cos (* tau (- h (/ 1.0 3.0))))))
   (+ 0.5 (* 0.5 (Math/cos (* tau (- h (/ 2.0 3.0))))))])

;; Hydrogen ---------------------------------------------------------------

(def hydrogen-states
  "Real m=0 orbitals: radial rings from 3s, angular lobes from 4d, 4f and 5g."
  [[3 0 0] [4 2 0] [4 3 0] [5 4 0]])

(def hydrogen-period
  "LCM of the squared principal quantum numbers, divided by two."
  1800)

(defn- laguerre
  "Associated Laguerre polynomial L_k^alpha(x), by its stable recurrence."
  ^double [^long k ^long alpha ^double x]
  (loop [j 0 current 1.0 previous 0.0]
    (if (= j k)
      current
      (recur (inc j)
             (/ (- (* (+ (* 2.0 j) alpha 1.0 (- x)) current)
                   (* (+ j alpha) previous))
                (inc j))
             current))))

(defn- factorial ^double [^long n]
  (reduce * 1.0 (map double (range 1 (inc n)))))

(defn- hydrogen-radial-normalization
  ^double [^long n ^long l]
  (Math/sqrt (* (Math/pow (/ 2.0 n) 3.0)
                (/ (factorial (- n l 1))
                   (* 2.0 n (factorial (+ n l)))))))

(def hydrogen-coefficients
  "Unit-norm amplitudes belonging to hydrogen-states."
  (double-array [0.5 0.5 0.5 0.5]))

(defn hydrogen-radial
  "R_nl(r), normalized by integral r^2 R_nl^2 dr = 1."
  ^double [^long n ^long l ^double radius]
  (let [q (/ (* 2.0 radius) n)]
    (* (hydrogen-radial-normalization n l)
       (Math/pow q l)
       (Math/exp (* -0.5 q))
       (laguerre (- n l 1) (inc (* 2 l)) q))))

(defn hydrogen-angular
  "Real spherical harmonic Y_l0 at polar cosine z/r, normalized on S^2."
  ^double [^long l ^double cosine-theta]
  (loop [j 0 p 1.0 q 0.0]
    (if (= j l)
      (* (Math/sqrt (/ (+ (* 2.0 l) 1.0) (* 4.0 Math/PI))) p)
      (let [next-p (/ (- (* (inc (* 2 j)) cosine-theta p) (* j q)) (inc j))]
        (recur (inc j) next-p p)))))

(defn hydrogen-eigenvalue
  "psi_nlm(x,y,z), with the polar axis along z, in atomic units."
  [n l m x y z]
  (when-not (zero? m)
    (throw (ex-info "The real gallery uses m=0" {:m m})))
  (let [radius (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (* (hydrogen-radial n l radius)
       (hydrogen-angular l (if (< radius 1.0e-12) 0.0 (/ z radius))))))

(defn- hydrogen-pair
  "Complex hydrogen psi at u, returned as [re im]. Test and quadrature path;
  the shader evaluates the same coefficients without allocating this pair."
  ^doubles [u ^double x ^double y ^double z]
  (let [radius (Math/sqrt (+ (* x x) (* y y) (* z z)))
        theta (if (< radius 1.0e-12) 0.0 (/ z radius))]
    (loop [i 0 re 0.0 im 0.0]
      (if (= i 4)
        (doto (double-array 2) (aset 0 re) (aset 1 im))
        (let [[n l] (hydrogen-states i)
              frequency (long (/ (- hydrogen-period) (* 2.0 n n)))
              angle (* tau frequency (double u))
              a (aget ^doubles hydrogen-coefficients i)
              value (* a (hydrogen-radial n l radius)
                       (hydrogen-angular l theta))]
          (recur (inc i)
                 (+ re (* value (Math/cos angle)))
                 (+ im (* value (Math/sin angle)))))))))

(defn hydrogen-psi
  "The normalized four-state superposition in atomic units as [re im].
  u is measured in periods of 1800 atomic time units."
  ^doubles [u ^double x ^double y ^double z]
  (hydrogen-pair u x y z))

(defn- hydrogen-shader [u]
  (let [phases (double-array 8)]
    (dotimes [i 4]
      (let [[n] (hydrogen-states i)
            frequency (long (/ (- hydrogen-period) (* 2.0 n n)))
            angle (* tau frequency (double u))]
        (aset phases (* 2 i) (Math/cos angle))
        (aset phases (inc (* 2 i)) (Math/sin angle))))
    (letfn [(pair-at [^double x ^double z]
              (let [radius (Math/sqrt (+ (* x x) (* z z)))
                    theta (if (< radius 1.0e-12) 0.0 (/ z radius))]
                (loop [i 0 re 0.0 im 0.0]
                  (if (= i 4)
                    [re im]
                    (let [[n l] (hydrogen-states i)
                          v (* (aget ^doubles hydrogen-coefficients i)
                               (hydrogen-radial n l radius) (hydrogen-angular l theta))]
                      (recur (inc i) (+ re (* v (aget phases (* 2 i))))
                             (+ im (* v (aget phases (inc (* 2 i)))))))))))]
      (let [peak (reduce max 1.0e-12
                         (for [ix (range 96) iz (range 54)
                               :let [[re im] (pair-at (* 0.6666666667 (- ix 48))
                                                      (* 0.6666666667 (- iz 27)))]]
                           (+ (* re re) (* im im))))]
        (fn ^long [^double sx ^double sy]
          (let [[re im] (pair-at (* 4.5 sx) (* 4.5 sy))
                density (+ (* re re) (* im im))
                t (min 1.0 (/ (Math/log1p (* 1000.0 (/ density peak)))
                              (Math/log 1001.0)))
                body (Math/pow t 0.75)
                contour (* 0.42 (+ (Math/exp (* -700.0 (Math/pow (- t 0.3) 2.0)))
                                   (Math/exp (* -700.0 (Math/pow (- t 0.55) 2.0)))
                                   (Math/exp (* -700.0 (Math/pow (- t 0.8) 2.0)))))
                [cr cg cb] (hue->rgb (+ 0.54 (* 0.22 (/ (Math/atan2 im re) tau))))
                b (+ body contour)]
            (r/rgb (+ 0.0003 (* b (double cr)) (* 0.18 contour))
                   (+ 0.001 (* b (double cg)) (* 0.48 contour))
                   (+ 0.003 (* b (double cb)) (* 0.70 contour)))))))))

(def hydrogen-beat
  {:id :hydrogen-beat
   :title "Hydrogen beat: 2s, 2p, 3p and 3d orbitals, phase colour in a luminous cloud"
   :seconds 12 :fps 30 :size [1920 1080] :view view
   :field hydrogen-shader
   :post (fn [_] [{:type :bloom :radius 14 :strength 0.8}
                  {:type :vignette :strength 0.25}])})

;; Shared square-mode machinery -------------------------------------------

(defn- sine-table
  "sin(k pi i/(samples-1)), k=1..max-k, flat by k then i."
  ^doubles [^long samples ^long max-k]
  (let [out (double-array (* samples max-k))]
    (dotimes [k max-k]
      (dotimes [i samples]
        (aset out (+ (* k samples) i)
              (Math/sin (* Math/PI (inc k) (/ (double i) (dec samples)))))))
    out))

(defn- sine-at
  ^double [^doubles table ^long samples ^long k ^double q]
  (aget table (+ (* (dec k) samples)
                 (max 0 (min (dec samples) (long (* q (dec samples))))))))

(def box-mode-numbers
  "Five distinct modes of a 16 by 9 infinite well, in scene units.
  E = m^2+n^2 = 5, 13, 20, 26, 32 cycles per loop, all commensurate."
  (long-array [1 2  3 2  2 4  5 1  4 4]))

(defn- box-energy ^long [^long m ^long n]
  (+ (* m m) (* n n)))

(def box-coefficients
  "Signed amplitudes of the five modes; the dominant pair sets the two
  brightest lobes, the weaker modes make the interference pattern travel."
  (double-array [1.0 -0.82 0.72 0.62 -0.55]))

(defn- box-pair [u ^double x ^double y]
  (loop [i 0 re 0.0 im 0.0]
    (if (= i 5)
      (doto (double-array 2) (aset 0 re) (aset 1 im))
      (let [m (aget ^longs box-mode-numbers (* 2 i))
            n (aget ^longs box-mode-numbers (inc (* 2 i)))
            angle (* tau (box-energy m n) (double u))
            a (aget ^doubles box-coefficients i)
            spatial (* a (Math/sin (* Math/PI m x)) (Math/sin (* Math/PI n y)))]
        (recur (inc i) (+ re (* spatial (Math/cos angle)))
               (+ im (* spatial (Math/sin angle))))))))

(defn box-psi
  "Five-mode superposition of the 16 by 9 well at loop phase u, as [re im].
  x in [0,16], y in [0,9]. Each mode oscillates at m^2+n^2 cycles per loop,
  so psi is exactly periodic with period one."
  ^doubles [u ^double x ^double y]
  (box-pair u x y))

(defn- box-shader [u]
  (let [samples 2048 sx (sine-table samples 5) sy (sine-table samples 4)
        phases (double-array 10)]
    (dotimes [i 5]
      (let [m (aget ^longs box-mode-numbers (* 2 i))
            n (aget ^longs box-mode-numbers (inc (* 2 i)))
            angle (* tau (box-energy m n) (double u))]
        (aset phases (* 2 i) (Math/cos angle))
        (aset phases (inc (* 2 i)) (Math/sin angle))))
    (fn ^long [^double x ^double y]
      ;; The well occupies [0,16]x[0,9]; the frame is that box exactly.
      (let [qx (+ 0.5 (/ x 16.0)) qy (+ 0.5 (/ y 9.0))]
        (loop [i 0 re 0.0 im 0.0]
          (if (= i 5)
            (let [d (+ (* re re) (* im im))
                  q (/ d 1.6)
                  t (Math/pow (min 1.0 q) 3.0)
                  hue (+ 0.58 (* 0.36 (/ (Math/atan2 im re) tau)))
                  [cr cg cb] (hue->rgb hue)
                  cont (fn [lv] (Math/exp (* -2600.0 (Math/pow (- q lv) 2.0))))
                  line (* 0.9 (+ (cont 0.20) (cont 0.50) (cont 0.85)))
                  halo (* 0.022 (Math/pow (min 1.0 q) 2.0))]
              (r/rgb (+ 0.0004 (* 0.08 halo) (* t (double cr)) (* 0.45 line))
                     (+ 0.0008 (* 0.12 halo) (* t (double cg)) (* 0.70 line))
                     (+ 0.003 (* 0.20 halo) (* t (double cb)) (* 0.95 line))))
            (let [m (aget ^longs box-mode-numbers (* 2 i))
                  n (aget ^longs box-mode-numbers (inc (* 2 i)))
                  a (aget ^doubles box-coefficients i)
                  spatial (* a (sine-at sx samples m qx) (sine-at sy samples n qy))]
              (recur (inc i) (+ re (* spatial (aget phases (* 2 i))))
                     (+ im (* spatial (aget phases (inc (* 2 i)))))))))))))

(def box-modes
  {:id :box-modes
   :title "Box modes: five commensurate well eigenstates, |psi|^2 in phase colour with iso-density contours"
   :seconds 12 :fps 30 :size [1920 1080] :view [16.0 9.0]
   :field box-shader
   :post (fn [_] [{:type :bloom :radius 10 :strength 0.7}
                  {:type :vignette :strength 0.3}])})

;; Drum -------------------------------------------------------------------

(def drum-mode-numbers
  "Pythagorean square modes: sqrt(m^2+n^2) = 5,10,15,20."
  (long-array [3 4  4 3  6 8  8 6  9 12  12 9  12 16  16 12]))

(def ^:private drum-coefficients
  (double-array [0.32 0.26 0.23 -0.19 0.18 0.16 -0.15 0.13]))

(defn drum-amplitude
  "Real membrane displacement on [0,1]^2. Frequencies are exact integer
  ratios 1:1:2:2:3:3:4:4, with fixed spatially-dependent phase offsets."
  ^double [u ^double x ^double y]
  (loop [i 0 sum 0.0]
    (if (= i 8)
      sum
      (let [m (aget ^longs drum-mode-numbers (* 2 i))
            n (aget ^longs drum-mode-numbers (inc (* 2 i)))
            k (quot (long (Math/sqrt (+ (* m m) (* n n)))) 5)
            offset (if (even? i) 0.0 (* 0.5 Math/PI))
            a (aget ^doubles drum-coefficients i)]
        (recur (inc i)
               (+ sum (* 2.0 a (Math/sin (* Math/PI m x)) (Math/sin (* Math/PI n y))
                         (Math/cos (+ (* tau k (double u)) offset)))))))))

(defn- drum-shader [u]
  (let [samples 4096 sx (sine-table samples 16) sy (sine-table samples 16)
        phases (double-array 8)]
    (dotimes [i 8]
      (let [m (aget ^longs drum-mode-numbers (* 2 i)) n (aget ^longs drum-mode-numbers (inc (* 2 i)))
            k (quot (long (Math/sqrt (+ (* m m) (* n n)))) 5)
            offset (if (even? i) 0.0 (* 0.5 Math/PI))]
        (aset phases i (Math/cos (+ (* tau k (double u)) offset)))))
    (fn ^long [^double x ^double y]
      (let [qx (let [q (mod (/ x 4.0) 2.0)] (if (<= q 1.0) q (- 2.0 q)))
            qy (let [q (mod (/ y 4.0) 2.0)] (if (<= q 1.0) q (- 2.0 q)))]
        (loop [i 0 displacement 0.0]
          (if (= i 8)
            (let [distance (Math/abs displacement) node (Math/exp (* -42.0 distance))
                  body (* 0.07 (min 1.0 (* 2.2 distance))) g (+ body node)
                  halo (* 0.10 (Math/exp (* -8.0 distance)))]
              (r/rgb (+ (* 0.08 halo) (* 0.20 g) (* 0.42 body))
                     (+ (* 0.48 halo) (* 0.83 g) (* 0.25 body))
                     (+ (* 0.75 halo) (* 0.96 g) (* 0.75 body))))
            (let [m (aget ^longs drum-mode-numbers (* 2 i)) n (aget ^longs drum-mode-numbers (inc (* 2 i)))
                  a (aget ^doubles drum-coefficients i)]
              (recur (inc i) (+ displacement
                                (* 2.0 a (sine-at sx samples m qx) (sine-at sy samples n qy)
                                   (aget phases i)))))))))))

(def drum
  {:id :drum
   :title "Drum: Pythagorean membrane modes, luminous Chladni lines in a 1:2:3:4 resonance"
   :seconds 12 :fps 30 :size [1920 1080] :view view
   :field drum-shader
   :post (fn [_] [{:type :bloom :radius 12 :strength 0.85}
                  {:type :vignette :strength 0.3}])})

(def loops
  "The orbital gallery, keyed by loop id."
  {:hydrogen-beat hydrogen-beat
   :box-modes box-modes
   :drum drum})
