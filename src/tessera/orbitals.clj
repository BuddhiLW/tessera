(ns tessera.orbitals
  "Exactly periodic bound states and resonances.

     :hydrogen-beat  four real hydrogen eigenstates on the xz plane. With
                     E_n = -1/(2 n^2), 72 E_n is integral for n = 2, 3;
                     these phases advance by whole turns over one loop.
     :box-modes     ten eigenstates of a two-dimensional infinite well.
                     E_mn is proportional to m^2+n^2, an integer.
     :drum          standing waves of a square membrane. Pythagorean mode
                     pairs have frequencies 5, 10, 15, 20 in a common unit,
                     so their nodal lines return without retuning physics.

  Atomic units are used for hydrogen. The slice is not itself a normalized
  two-dimensional state: the displayed states are normalized in R^3. The
  square-well modes are normalized on [0,1]^2. All shaders precompute mode
  phases and sine tables once per frame, leaving only primitive arithmetic
  and array reads in the per-pixel path."
  (:require [desargues.wallpaper.render :as r]))

(set! *warn-on-reflection* true)

(def ^:private tau (* 2.0 Math/PI))
(def ^:private sqrt-two (Math/sqrt 2.0))
(def ^:private sqrt-three (Math/sqrt 3.0))
(def ^:private sqrt-six (Math/sqrt 6.0))
(def ^:private sqrt-thirty (Math/sqrt 30.0))
(def ^:private view [(/ 128.0 9.0) 8.0])

(defn- hue->rgb
  "Full-saturation cosine colour wheel; h is in turns."
  [^double h]
  [(+ 0.5 (* 0.5 (Math/cos (* tau h))))
   (+ 0.5 (* 0.5 (Math/cos (* tau (- h (/ 1.0 3.0))))))
   (+ 0.5 (* 0.5 (Math/cos (* tau (- h (/ 2.0 3.0))))))])

;; Hydrogen ---------------------------------------------------------------

(def hydrogen-states
  "Real m=0 orbitals: 2s, 2p0, 3p0 and 3d0."
  [[2 0 0] [2 1 0] [3 1 0] [3 2 0]])

(def hydrogen-coefficients
  "Unit-norm amplitudes belonging to hydrogen-states."
  (double-array [0.5 0.5 0.5 0.5]))

(defn hydrogen-radial
  "Normalized associated-Laguerre radial function R_nl(r), in atomic units."
  ^double [^long n ^long l ^double radius]
  (cond
    (= n 2)
    (let [e (Math/exp (* -0.5 radius))]
      (if (zero? l)
        (* (/ 1.0 sqrt-two) (- 1.0 (* 0.5 radius)) e)
        (* (/ 1.0 (* 2.0 sqrt-six)) radius e)))

    (= n 3)
    (let [e (Math/exp (/ (- radius) 3.0))]
      (cond
        (zero? l)
        (* (/ 2.0 (* 3.0 sqrt-three))
           (+ 1.0 (* (/ -2.0 3.0) radius) (* (/ 2.0 27.0) radius radius)) e)
        (= l 1)
        (* (/ 4.0 (* 81.0 sqrt-six)) radius (- 6.0 radius) e)
        :else
        (* (/ 4.0 (* 81.0 sqrt-thirty)) radius radius e)))

    :else
    (throw (ex-info "This gallery implements n=2 and n=3" {:n n :l l}))))

(defn hydrogen-angular
  "Real spherical harmonic Y_l0 at polar cosine z/r, normalized on S^2."
  ^double [^long l ^double cosine-theta]
  (cond
    (zero? l) 0.28209479177387814
    (= l 1) (* 0.4886025119029199 cosine-theta)
    (= l 2) (* 0.31539156525252005
              (- (* 3.0 cosine-theta cosine-theta) 1.0))
    :else (throw (ex-info "This gallery implements l=0,1,2" {:l l}))))

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
              frequency (long (/ -72.0 (* 2.0 n n)))
              angle (* tau frequency (double u))
              a (aget ^doubles hydrogen-coefficients i)
              value (* a (hydrogen-radial n l radius)
                       (hydrogen-angular l theta))]
          (recur (inc i)
                 (+ re (* value (Math/cos angle)))
                 (+ im (* value (Math/sin angle)))))))))

(defn hydrogen-psi
  "The normalized four-state superposition in atomic units as [re im].
  u is measured in periods of 72 atomic time units."
  ^doubles [u ^double x ^double y ^double z]
  (hydrogen-pair u x y z))

(defn- hydrogen-shader [u]
  (let [phases (double-array 8)]
    (dotimes [i 4]
      (let [[n] (hydrogen-states i)
            frequency (long (/ -72.0 (* 2.0 n n)))
            angle (* tau frequency (double u))]
        (aset phases (* 2 i) (Math/cos angle))
        (aset phases (inc (* 2 i)) (Math/sin angle))))
    (fn ^long [^double sx ^double sy]
      (let [x (* 1.28 sx) z (* 1.28 sy)
            radius (Math/sqrt (+ (* x x) (* z z)))
            theta (if (< radius 1.0e-12) 0.0 (/ z radius))]
        (loop [i 0 re 0.0 im 0.0]
          (if (= i 4)
            (let [density (+ (* re re) (* im im))
                  t (min 1.0 (/ (Math/log1p (* 1500.0 density)) 4.4))
                  brightness (Math/pow t 1.3)
                  [cr cg cb] (hue->rgb (+ 0.57 (/ (Math/atan2 im re) tau)))
                  hot (* 0.28 (Math/pow t 5.0))]
              (r/rgb (+ 0.0005 (* brightness (double cr)) hot)
                     (+ 0.001 (* brightness (double cg)) hot)
                     (+ 0.003 (* brightness (double cb)) hot)))
            (let [[n l] (hydrogen-states i)
                  a (aget ^doubles hydrogen-coefficients i)
                  value (* a (hydrogen-radial n l radius)
                           (hydrogen-angular l theta))]
              (recur (inc i)
                     (+ re (* value (aget phases (* 2 i))))
                     (+ im (* value (aget phases (inc (* 2 i)))))))))))))

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
  "Ten spatial modes, (m,n), with energy m^2+n^2."
  (long-array [1 1  2 1  1 2  3 2  2 3  4 1  1 4  3 4  4 3  5 2]))

(def box-coefficients
  "Alternating unit-norm amplitudes; orthogonality makes psi normalized."
  (double-array (map #(/ (if (even? %) 1.0 -1.0) (Math/sqrt 10.0)) (range 10))))

(defn- box-pair [u ^double x ^double y]
  (loop [i 0 re 0.0 im 0.0]
    (if (= i 10)
      (doto (double-array 2) (aset 0 re) (aset 1 im))
      (let [m (aget ^longs box-mode-numbers (* 2 i))
            n (aget ^longs box-mode-numbers (inc (* 2 i)))
            angle (* tau (+ (* m m) (* n n)) (double u))
            a (aget ^doubles box-coefficients i)
            spatial (* 2.0 a (Math/sin (* Math/PI m x)) (Math/sin (* Math/PI n y)))]
        (recur (inc i) (+ re (* spatial (Math/cos angle)))
               (+ im (* spatial (Math/sin angle))))))))

(defn box-psi
  "Normalized unit-well superposition at loop phase u, returned as [re im]."
  ^doubles [u ^double x ^double y]
  (box-pair u x y))

(defn- inside-square?
  [^double x ^double y ^double half]
  (and (> x (- half)) (< x half) (> y (- half)) (< y half)))

(defn- box-shader [u]
  (let [samples 4096
        sx (sine-table samples 5)
        sy (sine-table samples 5)
        phases (double-array 20)]
    (dotimes [i 10]
      (let [m (aget ^longs box-mode-numbers (* 2 i))
            n (aget ^longs box-mode-numbers (inc (* 2 i)))
            angle (* tau (+ (* m m) (* n n)) (double u))]
        (aset phases (* 2 i) (Math/cos angle))
        (aset phases (inc (* 2 i)) (Math/sin angle))))
    (fn ^long [^double x ^double y]
      (if-not (inside-square? x y 3.92)
        (r/rgb 0.0005 0.001 0.003)
        (let [qx (/ (+ x 3.92) 7.84) qy (/ (+ y 3.92) 7.84)]
          (loop [i 0 re 0.0 im 0.0]
            (if (= i 10)
              (let [density (+ (* re re) (* im im))
                    t (/ density (+ density 0.36))
                    b (Math/pow t 1.45)
                    [cr cg cb] (hue->rgb (+ 0.57 (/ (Math/atan2 im re) tau)))
                    hot (* 0.32 (Math/pow t 7.0))]
                (r/rgb (+ (* b (double cr)) hot)
                       (+ (* b (double cg)) hot)
                       (+ (* b (double cb)) hot)))
              (let [m (aget ^longs box-mode-numbers (* 2 i))
                    n (aget ^longs box-mode-numbers (inc (* 2 i)))
                    a (aget ^doubles box-coefficients i)
                    spatial (* 2.0 a (sine-at sx samples m qx) (sine-at sy samples n qy))]
                (recur (inc i) (+ re (* spatial (aget phases (* 2 i))))
                       (+ im (* spatial (aget phases (inc (* 2 i))))))))))))))

(def box-modes
  {:id :box-modes
   :title "Box modes: ten commensurate infinite-well eigenstates in phase colour"
   :seconds 12 :fps 30 :size [1920 1080] :view view
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
  (let [samples 4096
        sx (sine-table samples 16)
        sy (sine-table samples 16)
        phases (double-array 8)]
    (dotimes [i 8]
      (let [m (aget ^longs drum-mode-numbers (* 2 i))
            n (aget ^longs drum-mode-numbers (inc (* 2 i)))
            k (quot (long (Math/sqrt (+ (* m m) (* n n)))) 5)
            offset (if (even? i) 0.0 (* 0.5 Math/PI))]
        (aset phases i (Math/cos (+ (* tau k (double u)) offset)))))
    (fn ^long [^double x ^double y]
      (if-not (inside-square? x y 3.92)
        (r/rgb 0.0005 0.001 0.003)
        (let [qx (/ (+ x 3.92) 7.84) qy (/ (+ y 3.92) 7.84)]
          (loop [i 0 displacement 0.0]
            (if (= i 8)
              (let [distance (Math/abs displacement)
                    node (Math/exp (* -42.0 distance))
                    body (* 0.12 (min 1.0 (* 2.2 distance)))
                    g (+ body node)
                    halo (* 0.13 (Math/exp (* -8.0 distance)))]
                (r/rgb (+ (* 0.08 halo) (* 0.20 g) (* 0.42 body))
                       (+ (* 0.48 halo) (* 0.83 g) (* 0.25 body))
                       (+ (* 0.75 halo) (* 0.96 g) (* 0.75 body))))
              (let [m (aget ^longs drum-mode-numbers (* 2 i))
                    n (aget ^longs drum-mode-numbers (inc (* 2 i)))
                    a (aget ^doubles drum-coefficients i)]
                (recur (inc i)
                       (+ displacement
                          (* 2.0 a (sine-at sx samples m qx) (sine-at sy samples n qy)
                             (aget phases i))))))))))))

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
