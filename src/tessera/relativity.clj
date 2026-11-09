(ns tessera.relativity
  "Relativity as seamless neon diagrams and live fields. The fields are the
   single source for both offline loops and their live-layer ports.

   :minkowski-boost  Lorentz-sheared spacetime, fixed light cones
   :aberration       stars gathered toward motion, Doppler-shifted colour
   :einstein-ring    a point lens sweeping in front of a distant star
   :twin-paradox     two worldlines and their unequal proper-time clocks

   The lens uses the thin-lens approximation. The twin diagram is a smooth
   compact excursion, with clocks spaced by elapsed proper time."
  (:require [clojure.string :as str]
            [desargues.wallpaper.field :as field]
            [desargues.wallpaper.girih :as g]
            [desargues.wallpaper.loop :as lp]
            [tessera.curves :as curves]
            [tessera.neon :as neon]))



(def ^:private tau (* 2.0 Math/PI))
(def ^:private view [8.0 4.5])

(defn boost
  "Boost [x ct] into the frame moving at beta = v/c along x. The interval
   ct^2 - x^2 is invariant and null rays stay on |x| = |ct|."
  [[x ct] beta]
  (let [gamma (/ 1.0 (Math/sqrt (- 1.0 (* (double beta) (double beta)))))]
    [(* gamma (- (double x) (* (double beta) (double ct))))
     (* gamma (- (double ct) (* (double beta) (double x))))]))

(def minkowski-form
  "An inverse Lorentz map gathers the moving observer's grid. sinh/cosh are
   exponentials, keeping the form in the common JVM/GLSL/WGSL vocabulary."
  {:params '[u boost_amplitude line_width]
   :body
   '(let [rapidity (* boost_amplitude (sin (* tau u)))
          beta (/ (- (exp rapidity) (exp (- rapidity)))
                  (+ (exp rapidity) (exp (- rapidity))))
          gamma (/ 1.0 (sqrt (max 1.0e-8 (- 1.0 (* beta beta)))))
          gx (* gamma (+ x (* beta y)))
          gt (* gamma (+ y (* beta x)))
          fx (abs (- (frac gx) 0.5))
          ft (abs (- (frac gt) 0.5))
          qx (/ fx line_width)
          qt (/ ft line_width)
          grid (+ (exp (- (* qx qx))) (exp (- (* qt qt))))
          cone_d (abs (- (abs x) (abs y)))
          qc (/ cone_d (* 0.85 line_width))
          cones (exp (- (* qc qc)))
          qax (/ gx (* gamma 1.4 line_width))
          qat (/ gt (* gamma 1.4 line_width))
          moving_axes (exp (- (* qax qax)))
          simultaneity (exp (- (* qat qat)))
          qr (/ (abs y) (* 1.1 line_width))
          qct (/ (abs x) (* 1.1 line_width))
          rest_axes (exp (- (* qr qr)))
          rest_time (exp (- (* qct qct)))
          pulse (+ 0.85 (* 0.15 (cos (* tau u))))]
      [(* pulse (+ (* 0.36 grid) (* 0.90 simultaneity) (* 0.62 cones) (* 0.75 rest_time)))
       (* pulse (+ (* 0.76 grid) (* 0.25 moving_axes) (* 0.76 cones) (* 0.54 rest_axes)))
       (* pulse (+ (* 1.00 grid) (* 0.92 moving_axes) (* 0.15 cones) (* 0.26 rest_axes)))])})

(defn aberration-position
  "Forward aberration of source ray [sx sy 1] for an observer moving at
   beta along z. R=sqrt(1+|p|^2), q=p/gamma/(1-beta*R). The inverse is
   p=q/gamma/(1+beta*R'), exactly."
  [[sx sy] beta]
  (let [gamma (/ 1.0 (Math/sqrt (- 1.0 (* (double beta) (double beta)))))
        ray-length (Math/sqrt (+ 1.0 (* (double sx) (double sx)) (* (double sy) (double sy))))
        scale (* gamma (- 1.0 (* (double beta) ray-length)))]
    [(/ (double sx) scale) (/ (double sy) scale)]))

(def star-sites
  "A reproducible, gently jittered sky in the camera's angular plane. The
   field of view is wide enough for stars to sweep into the boost direction."
  (vec (for [j (range 7) i (range 15)]
         (let [k (+ i (* 15 j))
               hash (fn [n] (- (lp/frac (+ (* (inc k) 0.754877666) (* n 0.569840291))) 0.5))]
           [(* 0.8 (- i 7)) (* 0.8 (- j 3))
            (+ (* 0.7 (hash 0.0)) (* 0.8 (- i 7)))
            (+ (* 0.7 (hash 1.0)) (* 0.8 (- j 3)))
            (+ 0.040 (* 0.022 (lp/frac (* (inc k) 0.618033989))))]))))

(defn- aberration-form
  "Unroll the fixed sky. Each observed pixel maps back to a source ray;
   Doppler controls both perceived brightness and its cool/warm colour."
  []
  (let [bindings
        (vec (concat
              '[beta (* boost_amplitude (sin (* tau u)))
                gamma (/ 1.0 (sqrt (max 1.0e-8 (- 1.0 (* beta beta)))))
                ray_length (sqrt (+ 1.0 (* x x) (* y y)))
                sx (/ x (* gamma (+ 1.0 (* beta ray_length))))
                sy (/ y (* gamma (+ 1.0 (* beta ray_length))))
                doppler (* gamma (+ 1.0 (* beta ray_length)))
                brightness (pow doppler 2.0)
                red (clamp (+ 1.0 (* 0.52 (- doppler 1.0))) 0.18 1.0)
                green (clamp (+ 0.56 (* 0.22 doppler)) 0.0 1.0)
                blue (clamp (+ 0.52 (* -0.72 (- doppler 1.0))) 0.12 1.0)]
              (mapcat
               (fn [[i [_ _ sx sy r]]]
                 (let [n (fn [k] (symbol (str k "_" i)))]
                   [(n "dx") (list '- 'sx sx)
                    (n "dy") (list '- 'sy sy)
                    (n "q2") (list '/ (list '+ (list '* (n "dx") (n "dx"))
                                               (list '* (n "dy") (n "dy")))
                                    (* r r))
                    (n "star") (list '+ (list 'exp (list '* -2.0 (n "q2")))
                                        (list '* 0.3 (list 'exp (list '* -0.15 (n "q2")))))]))
               (map-indexed vector star-sites))))]
    {:params '[u boost_amplitude]
     :body (list 'let (conj bindings
                            's (reduce (fn [acc i] (list '+ acc (symbol (str "star_" i))))
                                       0.0 (range (count star-sites)))
                            'e '(* brightness (min 1.5 s)))
                 '[(* e red) (* e green) (* e blue)])}))

(def lens-form
  "Point-mass thin lens: source = observed - theta_E^2 observed/r^2.
   A source centred behind the lens produces |observed| = theta_E; the
   deflection is in time units, so a centred source rings at every phase."
  {:params '[u einstein_radius source_radius]
   :body
   '(let [s (* tau u)
          r2 (+ (* x x) (* y y))
          pull (/ (* einstein_radius einstein_radius) (max r2 1.0e-8))
          sx (- x (* x pull))
          sy (- y (* y pull))
          dx (- sx (* 0.62 (cos s)))
          dy (- sy (* 0.26 (sin s)))
          q2 (/ (+ (* dx dx) (* dy dy)) (* source_radius source_radius))
          source (exp (* -2.0 q2))
          lens (* 0.30 (exp (* -18.0 r2)))
          qr (/ (- (sqrt r2) einstein_radius) (* 0.16 einstein_radius))
          ring (* (+ 0.54 (* 0.46 (pow (wave u) 2.0))) (exp (- (* qr qr))))
          ray_q (/ (- y (* 0.22 x)) 0.018)
          ray (* 0.10 (exp (- (* ray_q ray_q))) (exp (* -0.22 r2)))]
      [(+ (* source 1.00) (* ring 0.96) (* lens 0.65) ray)
       (+ (* source 0.35) (* ring 0.32) (* lens 0.12) (* ray 0.25))
       (+ (* source 0.62) (* ring 0.08) (* lens 0.015) (* ray 0.75))])})

(def twin-height 6.4)
(def twin-reach 1.9)

(defn traveler-x
  "A smooth compact excursion, x(0)=x(1)=0 and zero velocity at the seam."
  [t]
  (* 0.5 twin-reach (- 1.0 (Math/cos (* tau (double t))))))

(defn traveler-speed
  "|dx/d(ct)| of traveler-x in the diagram's c=1 coordinates."
  [t]
  (* (/ (* Math/PI twin-reach) twin-height)
     (Math/abs (Math/sin (* tau (double t))))))

(def proper-times
  "Cumulative proper time of the smooth worldline, sampled uniformly in
   coordinate time. Equal steps along this table are equal clock ticks."
  (let [n 4096
        integral (reduce (fn [[sum prev] i]
                           (let [speed (traveler-speed (/ (double i) n))
                                 next (Math/sqrt (- 1.0 (* speed speed)))]
                             [(+ sum (* 0.5 (+ prev next) (/ 1.0 n))) next]))
                         [0.0 1.0] (range 1 (inc n)))]
    (first integral)))

(def twin-proper-total proper-times)
(def ^:private twin-ages (range 0.0 1.0 0.0625))

(def twin-clock-phases
  "Equal proper-time ages mapped back to coordinate phase, via a dense
   inverse lookup table (age increases monotonically along the excursion)."
  (let [n 4096
        table (double-array (inc n))]
    (loop [i 0 sum 0.0 prev 1.0]
      (aset table i (/ sum twin-proper-total))
      (when (< i n)
        (let [speed (traveler-speed (/ (double (inc i)) n))
              next (Math/sqrt (- 1.0 (* speed speed)))]
          (recur (inc i) (+ sum (* 0.5 (+ prev next) (/ 1.0 n))) next))))
    (mapv (fn [age]
            (let [i (long (Math/floor (* age n)))
                  i (min (dec n) (max 0 i))
                  a (aget table i) b (aget table (inc i))
                  f (if (> b a) (/ (- age a) (- b a)) 0.0)]
              (/ (+ i f) n)))
          twin-ages)))

(defn- worldline
  "Polyline of the home twin (zero x) or traveller, bottom to top."
  [traveler?]
  (vec (for [i (range 241)
             :let [t (/ (double i) 240.0)]]
         [(if traveler? (traveler-x t) 0.0)
          (* twin-height (- t 0.5))])))

(defn- tick-mark [[x y] age color]
  {:node :line :from [(- x 0.15) y] :to [(+ x 0.15) y]
   :opacity (+ 0.38 (* 0.62 (curves/comet [age] 0.0 0.25)))
   :stroke {:color color :width 2.2}})

(defn twin-frame
  "Fixed worldlines, clocks spaced by proper time, and light travelling once
   along each worldline. A periodic soft sweep avoids a reversal seam."
  [u]
  (let [cycle-t (fn [p] (let [q (lp/frac p)] (if (< q 0.5) (* 2.0 q) (* 2.0 (- 1.0 q)))))
        at (fn [traveler? p]
             (let [t (cycle-t p)]
               [(if traveler? (traveler-x t) 0.0) (* twin-height (- t 0.5))]))
        trace (fn [traveler? color]
                (for [[a b s] (map vector (worldline traveler?) (rest (worldline traveler?))
                                    (map #(/ (double %) 240.0) (range)))
                      :let [hit (lp/wave (+ (* 3.0 u) s))]]
                  {:node :line :from a :to b :opacity (+ 0.42 (* 0.58 hit))
                   :stroke {:color (if (> hit 0.85) "#d7c7ff" color) :width (+ 1.3 (* 2.4 hit))}}))
        ticks (fn [traveler?]
                (for [[i t] (map-indexed vector (if traveler? twin-clock-phases twin-ages))
                      :let [pos (at traveler? t)
                            age (/ (double i) 16.0)
                            hit (lp/wave (+ u age))]]
                  (assoc (tick-mark pos age (if traveler? "#ff4fc8" "#45ddff"))
                         :opacity (+ 0.35 (* 0.65 hit)))))]
    {:background "#000000"
     :shapes (vec (concat
                   (trace false "#1a93dc") (trace true "#bd2c9b") (ticks false) (ticks true)
                   (for [t [0.0 1.0]
                         p (g/beads (neon/star [0.0 (* twin-height (- t 0.5))] 8 0.26 0.12 0.0)
                                    0.048 u true)]
                     {:node :dot :at p :opts {:radius 0.021} :fill {:color "#d5ecff" :opacity 0.9}})))}))

(defn field-shader [form params]
  "The offline loop shader for the same data consumed by a live layer."
  (fn [u]
    (field/jvm-shader (apply field/bind form u params))))

(def field-params
  "Live slot ABI: names, defaults, bounds, and UI labels. Append only."
  {:minkowski_boost [['boost_amplitude {:default 0.92 :min 0.0 :max 1.3 :label "Maximum rapidity"}]
                     ['line_width {:default 0.026 :min 0.008 :max 0.08 :label "Grid-line width"}]]
   :aberration [['boost_amplitude {:default 0.92 :min 0.0 :max 0.97 :label "Maximum velocity / c"}]]
   :einstein_ring [['einstein_radius {:default 1.0 :min 0.3 :max 2.5 :label "Einstein radius"}]
                   ['source_radius {:default 0.07 :min 0.015 :max 0.3 :label "Angular source radius"}]]})

(def minkowski-layer
  {:id :minkowski_boost :title "Minkowski boost: light cones hold while spacetime shears"
   :period 16 :view view :blend :lighten :params (:minkowski_boost field-params)
   :form minkowski-form})

(def aberration-layer
  {:id :aberration :title "Relativistic aberration: stars sweep into the blue direction"
   :period 16 :view view :blend :lighten :params (:aberration field-params)
   :form (aberration-form)})

(def einstein-layer
  {:id :einstein_ring :title "Einstein ring: a point mass bends a background star's light"
   :period 16 :view view :blend :lighten :params (:einstein_ring field-params)
   :form lens-form})

(def live-layers [minkowski-layer aberration-layer einstein-layer])

(defn- loop-spec [layer form-key]
  (let [defaults (mapv (comp :default second) (:params layer))]
    {:id (keyword (str/replace (name (:id layer)) "_" "-"))
     :title (:title layer) :seconds (:period layer) :fps 30
     :size [1920 1080] :view (:view layer) :ss 2
     :field (field-shader (form-key {:minkowski minkowski-form
                                    :aberration (:form aberration-layer)
                                    :lens lens-form})
                          defaults)
     :post (fn [_] [{:type :bloom :radius 9 :strength 0.6}
                    {:type :vignette :strength 0.48}])}))

(def minkowski-boost (loop-spec minkowski-layer :minkowski))
(def aberration (loop-spec aberration-layer :aberration))
(def einstein-ring (loop-spec einstein-layer :lens))

(def twin-paradox
  {:id :twin-paradox
   :title "Twin paradox: equal clock ticks, unequal proper-time worldlines"
   :seconds 20 :fps 30 :size [1920 1080] :view view
   :frame twin-frame
   :post (fn [_] [{:type :bloom :radius 12 :strength 1.0}
                  {:type :vignette :strength 0.52}])})

(def loops
  (array-map :minkowski-boost minkowski-boost
             :aberration aberration
             :einstein-ring einstein-ring
             :twin-paradox twin-paradox))
