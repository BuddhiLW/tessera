(ns tessera.lattice
  "Square-based quantum lattices, with light rather than rotating geometry.

   Lieb contours resolve the dispersive cone and the flat band's sublattice
   polarization. Bloch packets evolve under the exact constant-force
   tight-binding propagator, with commensurate x/y Bloch periods. The
   Hofstadter butterfly is the spectrum of actual rational-flux Harper
   matrices, not a fractal lookalike. All animation is periodic in u."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [tessera.palette :as pal])
  (:import (org.apache.commons.math3.linear Array2DRowRealMatrix EigenDecomposition)))

(set! *warn-on-reflection* true)

(defn- channels [^long c]
  [(/ (bit-and (bit-shift-right c 16) 0xff) 255.0)
   (/ (bit-and (bit-shift-right c 8) 0xff) 255.0)
   (/ (bit-and c 0xff) 255.0)])

(defn lieb-energy
  "Positive Lieb band for unit hopping: E=2 sqrt(cos²(kx/2)+cos²(ky/2)).
   The other bands are -E and the exactly flat E=0 band."
  ^double [^double kx ^double ky]
  (let [cx (Math/cos (* 0.5 kx)) cy (Math/cos (* 0.5 ky))]
    (* 2.0 (Math/sqrt (+ (* cx cx) (* cy cy))))))

(defn lieb-shader
  "Rising energy contours; cyan/magenta encodes the B/C polarization of
   the flat eigenvector (0,-cos(ky/2),cos(kx/2)). Touching points stay lit.
   The square lattice and every contour's orientation are stationary."
  [u]
  (let [u (double u)
        p (pal/scheme :blue-magenta-orange)
        [ar ag ab] (channels (pal/at p 0.12))
        [br bg bb] (channels (pal/at p 0.76))
        ar (double ar) ag (double ag) ab (double ab)
        br (double br) bg (double bg) bb (double bb)]
    (fn ^long [^double x ^double y]
      (let [kx (* 3.8 x) ky (* 3.8 y)
            cx (Math/cos (* 0.5 kx)) cy (Math/cos (* 0.5 ky))
            sx (* cx cx) sy (* cy cy) s (+ sx sy)
            e (* 2.0 (Math/sqrt s))
            b (/ sx (+ s 1.0e-12))
            line (Math/pow (lp/wave (- (/ e 0.29) u)) 38.0)
            fine (* 0.22 (Math/pow (lp/wave (- (/ e 0.58) u 0.3)) 55.0))
            cusp (Math/exp (* -32.0 s))
            v (+ 0.018 (* 0.94 line) fine)
            hot (* 0.30 (Math/pow line 4.0))]
        (r/rgb (+ (* v (+ ar (* b (- br ar)))) hot (* 0.58 cusp))
               (+ (* v (+ ag (* b (- bg ag)))) hot (* 0.77 cusp))
               (+ (* v (+ ab (* b (- bb ab)))) hot (* 0.95 cusp)))))))

(def lieb-fermi
  {:id :lieb-fermi
   :title "Lieb lattice: four-fold Fermi contours and flat-band polarization"
   :seconds 18 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
   :field lieb-shader
   :post (fn [_] [{:type :bloom :radius 11 :strength 0.9}
                  {:type :chroma :shift 1.3}
                  {:type :vignette :strength 0.46}])})

(defn bloch-seed
  "Fourier coefficients of a normalized real-space Gaussian packet.
   Returns a periodic quadrature of phi(k); no time integration is used."
  [sigma momentum]
  (let [sigma (double sigma) momentum (double momentum)
        sites (range -18 19)
        raw (mapv #(Math/exp (/ (- (* % %)) (* 4.0 sigma sigma))) sites)
        norm (Math/sqrt (reduce + (map #(* % %) raw)))
        m 256
        qs (double-array (map #(* lp/tau (/ (+ % 0.5) m)) (range m)))
        phi (double-array (* 2 m))]
    (dotimes [i m]
      (let [q (aget qs i)
            [re im] (reduce (fn [[re im] [n a]]
                              (let [phase (* (- momentum q) n) a (/ a norm)]
                                [(+ re (* a (Math/cos phase)))
                                 (+ im (* a (Math/sin phase)))]))
                            [0.0 0.0] (map vector sites raw))]
        (aset phi (* 2 i) (double re))
        (aset phi (inc (* 2 i)) (double im))))
    {:q qs :phi phi}))

(defn bloch-density
  "Exact 1D probability at sites -radius..radius, H=-J(T+T*)+Fn.
   `cycles` is an integer force ratio, `excursion`=2J/F. The momentum
   propagator is exp[i excursion (sin k - sin(k-2pi cycles u))]; the
   omitted exp[-i n F t] has unit modulus. T_B=2pi/F (a=hbar=1)."
  ^doubles [seed u cycles excursion radius]
  (let [^doubles qs (:q seed) ^doubles phi (:phi seed)
        m (alength qs) radius (long radius)
        theta (* lp/tau (long cycles) (lp/frac (double u)))
        excursion (double excursion)
        re (double-array m) im (double-array m)
        result (double-array (inc (* 2 radius)))]
    (dotimes [i m]
      (let [q (aget qs i)
            ph (* excursion (- (Math/sin q) (Math/sin (- q theta))))
            c (Math/cos ph) s (Math/sin ph)
            a (aget phi (* 2 i)) b (aget phi (inc (* 2 i)))]
        (aset re i (- (* a c) (* b s)))
        (aset im i (+ (* a s) (* b c)))))
    (dotimes [j (alength result)]
      (let [n (- j radius)]
        (loop [i 0 a 0.0 b 0.0]
          (if (< i m)
            (let [ph (* n (aget qs i)) c (Math/cos ph) s (Math/sin ph)]
              (recur (inc i)
                     (+ a (- (* (aget re i) c) (* (aget im i) s)))
                     (+ b (+ (* (aget re i) s) (* (aget im i) c)))))
            (aset result j (/ (+ (* a a) (* b b)) (* m m)))))))
    result))

(def ^:private packet-x (delay (bloch-seed 1.0 (/ Math/PI 2.0))))
(def ^:private packet-y (delay (bloch-seed 1.0 (* 0.35 Math/PI))))

(defn bloch-shader
  "An incoherent square superlattice of exact 2D Bloch packets. Adjacent
   cavities have different time origins, so light never collapses globally.
   A site's bead/halo brightness is its probability, without per-frame
   renormalization. Fx:Fy=1:2 gives a two-lobed, non-rotating Bloch path."
  [u]
  (let [radius 24 stride 49
        phases [0.0 0.25 0.5 0.75]
        px (mapv #(bloch-density @packet-x (+ (double u) %) 1 4.8 radius) phases)
        py (mapv #(bloch-density @packet-y (+ (double u) %) 2 2.4 radius) phases)
        p (pal/around (pal/scheme :blue-magenta-orange))
        colors (mapv #(channels (pal/at p %)) [0.05 0.26 0.55 0.79])]
    (fn ^long [^double x ^double y]
      (let [gx (/ x 0.22) gy (/ y 0.22)
            ix (long (Math/floor (+ gx 0.5))) iy (long (Math/floor (+ gy 0.5)))
            tile-x (long (Math/floor (/ (+ ix 9.0) 18.0)))
            tile-y (long (Math/floor (/ (+ iy 6.0) 12.0)))
            n (- ix (* tile-x 18)) m (- iy (* tile-y 12))
            phase (int (mod (+ tile-x (* 2 tile-y)) 4))
            ^doubles dx (nth px phase) ^doubles dy (nth py phase)
            ;; Sum neighboring replicas, rather than clipping a packet at a tile edge.
            vx (loop [s -2 v 0.0]
                 (if (> s 2) v
                     (let [j (+ radius n (* s 18))]
                       (recur (inc s) (+ v (if (< -1 j stride) (aget dx j) 0.0))))))
            vy (loop [s -3 v 0.0]
                 (if (> s 3) v
                     (let [j (+ radius m (* s 12))]
                       (recur (inc s) (+ v (if (< -1 j stride) (aget dy j) 0.0))))))
            density (* vx vy)
            dx (- gx ix) dy (- gy iy) d2 (+ (* dx dx) (* dy dy))
            bead (Math/exp (* -40.0 d2))
            halo (Math/exp (* -6.0 d2))
            energy (- 1.0 (Math/exp (* -32.0 density)))
            v (* energy (+ (* 0.84 bead) (* 0.25 halo)))
            hot (* 0.48 bead (Math/pow energy 3.0))
            grid (* 0.017 bead)
            [cr cg cb] (nth colors phase)]
        (r/rgb (+ grid (* v (double cr)) hot)
               (+ grid (* v (double cg)) hot)
               (+ (* 1.6 grid) (* v (double cb)) hot))))))

(def bloch-oscillation
  {:id :bloch-oscillation
   :title "Bloch oscillations: exact square-lattice packets, commensurate forces 1:2"
   :seconds 20 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
   :field bloch-shader
   :post (fn [_] [{:type :bloom :radius 10 :strength 1.1}
                  {:type :vignette :strength 0.40}])})

(defn harper-edges
  "Band edges of the square-lattice Harper model at rational flux p/q.
   Chambers' relation puts the extrema at (q kx,q ky)=(0,0),(pi,pi).
   Matrices are real symmetric, including the periodic/antiperiodic bond.
   Returns 2q eigenvalues, hopping J=1."
  [p q]
  (let [p (long p) q (long q)]
    (when-not (and (pos? q) (<= 0 p q))
      (throw (ex-info "Flux must have 0<=p<=q and q>0" {:p p :q q})))
    (vec
     (mapcat
      (fn [twist]
        (let [ky (/ (double twist) q)
              boundary (Math/cos (double twist))
              matrix (Array2DRowRealMatrix. (int q) (int q))]
          (dotimes [j q]
            (.addToEntry matrix j j (* 2.0 (Math/cos (+ (* lp/tau (/ (* p j) (double q))) ky))))
            ;; Add each positive-direction bond and its Hermitian partner.
            ;; This also handles q=1 self-bonds and q=2 doubled bonds.
            (let [k (mod (inc j) q) v (if (= j (dec q)) boundary 1.0)]
              (.addToEntry matrix j k v)
              (.addToEntry matrix k j v)))
          (seq (.getRealEigenvalues (EigenDecomposition. matrix)))))
      [0.0 Math/PI]))))

(defn- gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn butterfly-texture
  "Broaden exact Harper band edges into a monochrome spectral atlas.
   Farey fractions with denominator <=max-q, mirrored by flux/energy
   symmetries. A separable Gaussian is a display kernel, not fake levels."
  [w h max-q]
  (let [w (long w) h (long h) out (double-array (* w h))
        kernels (vec (for [dy (range -3 4) dx (range -2 3)]
                       [dx dy (Math/exp (- (+ (/ (* dx dx) 1.5) (/ (* dy dy) 2.2))))]))]
    (doseq [q (range 1 (inc max-q)) p (range 0 (inc (quot q 2)))
            :when (= 1 (gcd p q))
            :let [alpha (/ (double p) q) energies (harper-edges p q)]
            e energies flux [alpha (- 1.0 alpha)] sign [-1.0 1.0]
            :let [i (long (Math/round (* (dec w) flux)))
                  j (long (Math/round (* (dec h) (+ 0.5 (/ (* sign (double e)) 8.5)))))]
            [dx dy weight] kernels
            :let [x (+ i (long dx)) y (+ j (long dy))]
            :when (and (< -1 x w) (< -1 y h))]
      (let [o (+ (* y w) x)]
        (aset out o (max (aget out o) (double weight)))))
    {:w w :h h :values out}))

(def ^:private spectrum (delay (butterfly-texture 1200 720 72)))

(defn hofstadter-shader
  "A stationary luminous Hofstadter butterfly; a soft amber flux probe
   travels there and back, revealing allowed energies. The band gaps are
   genuine, and never filled by an animated approximation."
  [u]
  (let [{:keys [w h values]} @spectrum
        w (long w) h (long h) ^doubles values values
        alpha (+ 0.08 (* 0.84 (lp/wave (double u))))
        p (pal/scheme :blue-magenta-orange)]
    (fn ^long [^double x ^double y]
      (let [flux (+ 0.5 (/ x 8.0))
            fy (+ 0.5 (/ y 4.5))
            fx (* flux (dec w)) yy (* fy (dec h))
            i (max 0 (min (- w 2) (long fx))) j (max 0 (min (- h 2) (long yy)))
            tx (- fx i) ty (- yy j)
            o (+ (* j w) i)
            top (+ (* (- 1.0 tx) (aget values o)) (* tx (aget values (inc o))))
            bot (+ (* (- 1.0 tx) (aget values (+ o w))) (* tx (aget values (+ o w 1))))
            spectral (+ (* (- 1.0 ty) top) (* ty bot))
            probe (Math/exp (* -950.0 (- flux alpha) (- flux alpha)))
            pulse (+ 0.60 (* 0.40 (lp/wave (- (* flux 3.0) (double u)))))
            v (* spectral (+ (* 0.72 pulse) (* 0.45 probe)))
            [cr cg cb] (channels (pal/at p (+ 0.08 (* 0.78 (Math/abs (* 2.0 (- fy 0.5)))))))
            hot (* 0.50 spectral probe)]
        (r/rgb (+ (* v (double cr)) hot (* 0.045 probe))
               (+ (* v (double cg)) (* 0.85 hot) (* 0.020 probe))
               (+ (* v (double cb)) (* 0.55 hot)))))))

(def hofstadter
  {:id :hofstadter
   :title "Hofstadter butterfly: square-lattice Harper spectra in magnetic flux"
   :seconds 24 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
   :field hofstadter-shader
   :post (fn [_] [{:type :bloom :radius 9 :strength 0.85}
                  {:type :vignette :strength 0.35}])})

(def loops
  "Loop specs for the coordinator's registry; loading does not diagonalize."
  (into (array-map) (map (juxt :id identity))
        [lieb-fermi bloch-oscillation hofstadter]))
