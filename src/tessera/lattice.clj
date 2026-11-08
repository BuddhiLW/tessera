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
  "Two thin, interleaved contour families of the +/- dispersive Lieb bands.
   A faint A/B/C motif makes the square edge-centred lattice explicit.
   A narrow smooth energy probe E(u)=3 cos(2pi u) crosses the E=0 flat
   band twice per loop; its weight lights the entire zone and B/C sites."
  [u]
  (let [u (double u)
        probe (* 3.0 (Math/cos (* lp/tau u)))
        flat (Math/exp (* -16.0 probe probe))
        p (pal/scheme :blue-magenta-orange)
        [ar ag ab] (channels (pal/at p 0.15))
        [br bg bb] (channels (pal/at p 0.74))
        ar (double ar) ag (double ag) ab (double ab)
        br (double br) bg (double bg) bb (double bb)]
    (fn ^long [^double x ^double y]
      (let [kx (* 3.8 x) ky (* 3.8 y)
            cx (Math/cos (* 0.5 kx)) cy (Math/cos (* 0.5 ky))
            sx (* cx cx) sy (* cy cy) s (+ sx sy)
            e (* 2.0 (Math/sqrt s)) b (/ sx (+ s 1.0e-12))
            positive (Math/pow (lp/wave (- (/ e 0.65) u)) 110.0)
            negative (Math/pow (lp/wave (+ (- (/ e 0.65)) (- u) 0.5)) 110.0)
            cusp (Math/exp (* -46.0 s))
            ;; Unit-cell sites: A corners, B horizontal and C vertical midpoints.
            gx (/ kx lp/tau) gy (/ ky lp/tau)
            dx (- gx (Math/rint gx)) dy (- gy (Math/rint gy))
            ex (- gx 0.5 (Math/rint (- gx 0.5)))
            ey (- gy 0.5 (Math/rint (- gy 0.5)))
            a-dot (Math/exp (* -2500.0 (+ (* dx dx) (* dy dy))))
            b-dot (Math/exp (* -2500.0 (+ (* ex ex) (* dy dy))))
            c-dot (Math/exp (* -2500.0 (+ (* dx dx) (* ey ey))))
            lattice (+ (* 0.22 a-dot) (* (+ 0.14 (* 0.75 flat)) (+ b-dot c-dot)))
            plus (* 0.91 positive) minus (* 0.86 negative)
            hot (* 0.16 (+ (Math/pow positive 4.0) (Math/pow negative 4.0)))
            flat-glow (* flat (+ 0.025 (* 0.15 (+ a-dot b-dot c-dot))))]
        (r/rgb (+ (* plus (+ 0.10 (* 0.18 b))) (* minus (+ br 0.12)) hot
                  (* 0.65 lattice) (* 0.25 cusp) (* 0.25 flat-glow))
               (+ (* plus (+ 0.76 (* 0.12 b))) (* minus (* 0.50 bg)) hot
                  (* 0.86 lattice) (* 0.60 cusp) flat-glow)
               (+ (* plus 0.94) (* minus (+ ab (* 0.20 b))) hot
                  lattice (* 0.78 cusp) (* 0.72 flat-glow)))))))

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

(defn bloch-field
  "Complex exact constant-force packet at integer sites, flat [re im ..].
   Unlike bloch-density this retains the site phase exp(-i n F t), needed
   for physically meaningful arg(psi) colouring. Integer force ratios make
   the complex field, not merely its probability, revive after one loop."
  ^doubles [seed u cycles excursion radius]
  (let [^doubles qs (:q seed) ^doubles phi (:phi seed)
        m (alength qs) radius (long radius)
        theta (* lp/tau (long cycles) (lp/frac (double u)))
        excursion (double excursion)
        re (double-array m) im (double-array m)
        result (double-array (* 2 (inc (* 2 radius))))]
    (dotimes [i m]
      (let [q (aget qs i)
            ph (* excursion (- (Math/sin q) (Math/sin (- q theta))))
            c (Math/cos ph) s (Math/sin ph)
            a (aget phi (* 2 i)) b (aget phi (inc (* 2 i)))]
        (aset re i (- (* a c) (* b s)))
        (aset im i (+ (* a s) (* b c)))))
    (dotimes [j (inc (* 2 radius))]
      (let [n (- j radius)]
        (loop [i 0 a 0.0 b 0.0]
          (if (< i m)
            (let [ph (* n (- (aget qs i) theta)) c (Math/cos ph) s (Math/sin ph)]
              (recur (inc i)
                     (+ a (- (* (aget re i) c) (* (aget im i) s)))
                     (+ b (+ (* (aget re i) s) (* (aget im i) c)))))
            (do (aset result (* 2 j) (/ a m))
                (aset result (inc (* 2 j)) (/ b m)))))))
    result))

(defn bloch-density
  "Exact 1D probability at sites -radius..radius, H=-J(T+T*)+Fn.
   `cycles` is an integer force ratio, `excursion`=2J/F. T_B=2pi/F
   (a=hbar=1); the complex propagator lives in bloch-field."
  ^doubles [seed u cycles excursion radius]
  (let [^doubles psi (bloch-field seed u cycles excursion radius)
        n (quot (alength psi) 2)
        result (double-array n)]
    (dotimes [i n]
      (let [re (aget psi (* 2 i)) im (aget psi (inc (* 2 i)))]
        (aset result i (+ (* re re) (* im im)))))
    result))

(def ^:private packet-x (delay (bloch-seed 2.04 (/ Math/PI 2.0))))
(def ^:private packet-y (delay (bloch-seed 2.04 (* 0.35 Math/PI))))

(defn bloch-shader
  "Three tight exact Gaussian packets, five discrete earlier orbit positions.
   The trail applies 0.5^s AFTER each ghost's density exposure, not before.
   Dot colour is the site's exact complex phase on a closed integer-frequency
   violet/cyan/gold palette, resaturated after mixing. No chromatic post."
  [u]
  (let [radius 64 w 73 h 43 spacing 0.12
        centers [[-22 -7 0.0] [0 7 0.33] [22 -5 0.67]]
        p (pal/cosine [0.5 0.5 0.5] [0.5 0.5 0.5]
                      [1.0 1.0 1.0] [0.0 0.16 0.42])
        exposure (double-array (* w h)) weights (double-array (* w h))
        red (double-array (* w h)) green (double-array (* w h)) blue (double-array (* w h))]
    (doseq [[cx cy phase] centers s (range 5)
            :let [time (- (+ (double u) (double phase)) (* 0.090 s))
                  weight (Math/pow 0.5 s)
                  ^doubles fx (bloch-field @packet-x time 1 7.5 radius)
                  ^doubles fy (bloch-field @packet-y time 2 4.3 radius)]]
      (dotimes [j h]
        (dotimes [i w]
          (let [n (+ radius (- i 36 (long cx))) m (+ radius (- j 21 (long cy)))
                ax (aget fx (* 2 n)) bx (aget fx (inc (* 2 n)))
                ay (aget fy (* 2 m)) by (aget fy (inc (* 2 m)))
                re (- (* ax ay) (* bx by)) im (+ (* ax by) (* bx ay))
                rho (+ (* re re) (* im im))
                e (* weight 0.98 (- 1.0 (Math/exp (* -115.0 rho))))
                [cr cg cb] (channels (pal/at p (/ (Math/atan2 im re) lp/tau)))
                o (+ (* j w) i)]
            ;; Bounded sum of exposed photons (screen blend), never a white clip.
            (aset exposure o (- 1.0 (* (- 1.0 (aget exposure o)) (- 1.0 e))))
            (aset weights o (+ (aget weights o) e))
            (aset red o (+ (aget red o) (* e (double cr))))
            (aset green o (+ (aget green o) (* e (double cg))))
            (aset blue o (+ (aget blue o) (* e (double cb))))))))
    (dotimes [o (* w h)]
      (let [weight (+ 1.0e-30 (aget weights o))
            cr (/ (aget red o) weight) cg (/ (aget green o) weight) cb (/ (aget blue o) weight)
            lo (min cr cg cb) hi (max cr cg cb) span (max 1.0e-12 (- hi lo))
            e (* 0.94 (- 1.0 (Math/pow (- 1.0 (aget exposure o)) 1.4)))
            scale (/ (* e 0.97) span)]
        ;; HSV saturation .97, value=.94*exposure; hue preserved, no grey mix.
        (aset red o (+ (* 0.03 e) (* scale (- cr lo))))
        (aset green o (+ (* 0.03 e) (* scale (- cg lo))))
        (aset blue o (+ (* 0.03 e) (* scale (- cb lo))))))
    (fn ^long [^double x ^double y]
      (let [gx (+ 36.0 (/ x spacing)) gy (+ 21.0 (/ y spacing))
            i (max 0 (min (- w 2) (long (Math/floor gx))))
            j (max 0 (min (- h 2) (long (Math/floor gy))))
            tx (- gx i) ty (- gy j) o (+ (* j w) i)
            ni (max 0 (min (dec w) (long (Math/rint gx))))
            nj (max 0 (min (dec h) (long (Math/rint gy)))) site (+ (* nj w) ni)
            dx (- gx ni) dy (- gy nj)
            bead (Math/exp (* -35.0 (+ (* dx dx) (* dy dy))))
            sample (fn ^double [^doubles a]
                     (+ (* (- 1.0 ty) (+ (* (- 1.0 tx) (aget a o)) (* tx (aget a (inc o)))))
                        (* ty (+ (* (- 1.0 tx) (aget a (+ o w))) (* tx (aget a (+ o w 1)))))))
            grid (* 0.010 bead)
            paint (fn ^double [^doubles a]
                    (+ grid (* 0.13 (sample a)) (* 0.87 bead (aget a site))))]
        (r/rgb (paint red) (paint green) (paint blue))))))

(def bloch-oscillation
  {:id :bloch-oscillation
   :title "Bloch oscillations: exact square-lattice packets, commensurate forces 1:2"
   :seconds 20 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
   :field bloch-shader
   :post (fn [_] [{:type :bloom :radius 8 :strength 0.55}
                  {:type :vignette :strength 0.18}])})

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
            :let [i (long (Math/round (double (* (dec w) flux))))
                  j (long (Math/round (* (dec h) (+ 0.5 (/ (* sign (double e)) 8.5)))))]
            [dx dy weight] kernels
            :let [x (+ i (long dx)) y (+ j (long dy))]
            :when (and (< -1 x w) (< -1 y h))]
      (let [o (+ (* y w) x)]
        (aset out o (max (aget out o) (double weight)))))
    {:w w :h h :values out}))

(def ^:private spectrum (delay (butterfly-texture 1200 720 72)))

(defn hofstadter-shader
  "Stationary Harper spectrum with a crisp white-core, warm-halo scanner.
   A gamma exposure reveals nested butterflies; cyan band edges sit above
   violet depths. Only allowed energies become amber-white at the probe.
   With `periodic?` the flux axis wraps: the spectrum is periodic in flux
   with period one, so a wider frame shows the neighbouring butterflies."
  [u & [periodic?]]
  (let [{:keys [w h values]} @spectrum
        w (long w) h (long h) ^doubles values values
        alpha (+ 0.08 (* 0.84 (lp/wave (double u))))
        p (pal/scheme :blue-magenta-orange)]
    (fn ^long [^double x ^double y]
      (let [flux (+ 0.5 (/ x 8.0))
            flux (if periodic? (lp/frac flux) flux)
            fy (+ 0.5 (/ y 4.5))
            fx (* flux (dec w)) yy (* fy (dec h))
            i (max 0 (min (- w 2) (long fx))) j (max 0 (min (- h 2) (long yy)))
            tx (- fx i) ty (- yy j) o (+ (* j w) i)
            top (+ (* (- 1.0 tx) (aget values o)) (* tx (aget values (inc o))))
            bot (+ (* (- 1.0 tx) (aget values (+ o w))) (* tx (aget values (+ o w 1))))
            raw (+ (* (- 1.0 ty) top) (* ty bot))
            spectral (Math/pow (max 0.0 raw) 0.70)
            distance (- flux alpha)
            line (Math/exp (* -650000.0 distance distance))
            halo (Math/exp (* -65000.0 distance distance))
            probe (Math/exp (* -8500.0 distance distance))
            pulse (+ 0.74 (* 0.26 (lp/wave (- (* flux 3.0) (double u)))))
            v (* spectral pulse)
            edge (min 1.0 (* 2.5 (Math/abs (- bot top))))
            cyan (+ (* 0.72 edge) (* 0.25 (lp/wave (+ (* 3.0 fy) flux))))
            [cr cg cb] (channels (pal/at p (+ 0.05 (* 0.65 (Math/abs (* 2.0 (- fy 0.5)))))))
            hot (* 0.85 spectral probe)
            marker (+ (* 0.65 line) (* 0.10 halo))]
        (r/rgb (+ (* v (+ (* (- 1.0 cyan) (double cr)) (* cyan 0.08)))
                  hot marker (* 0.025 halo))
               (+ (* v (+ (* (- 1.0 cyan) (* 0.70 (double cg))) (* cyan 0.90)))
                  (* 0.78 hot) (* 0.90 marker))
               (+ (* v (+ (* (- 1.0 cyan) (double cb)) (* cyan 1.0)))
                  (* 0.32 hot) (* 0.76 marker)))))))

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
