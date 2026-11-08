(ns tessera.layers
  "Live field layers, independent of the closure-based offline loop registry.

   :form is desargues field data, not a shader closure. :params is an ordered
   vector of [symbol spec] pairs: an append-only binding ABI. The trama emitter
   supplies loop_rate at slot 0; these pairs occupy slots 1 onwards. Never
   reorder or remove a published pair. Bloom/chroma/vignette from offline loop
   :post stages are intentionally not baked into a live source layer."
  (:require [clojure.walk :as walk]
            [desargues.wallpaper.emit :as emit]
            [desargues.wallpaper.field :as field]
            [thi.ng.color.gradients :as grad]))

(defn- quantized-channel
  "The offline palettes quantize BEFORE shading; preserve that intermediate
   8-bit round trip, not only the final field-raster quantization."
  [expression]
  (list '/ (list 'floor (list '+ (list '* 255.0 (list 'clamp expression 0.0 1.0)) 0.5)) 255.0))

(defn- palette-channels [scheme t]
  (let [[a b c d] (get grad/cosine-schemes scheme)]
    (mapv (fn [aa bb cc dd]
            (quantized-channel (list 'cospal t aa bb cc dd)))
          a b c d)))

(defn- palette-sample [scheme t]
  (let [[a b c d] (get grad/cosine-schemes scheme)]
    (mapv (fn [aa bb cc dd]
            (/ (field/quantize (+ aa (* bb (Math/cos (* 2.0 Math/PI (+ (* cc t) dd)))))) 255.0))
          a b c d)))

(defn- contour-params [a step]
  [['fermi_level {:default 0.0 :min -4.0 :max 4.0 :label "Fermi level offset"}]
   ['contour_step {:default step :min 0.05 :max 2.0 :label "Energy contour spacing"}]
   ['sharpness {:default 24.0 :min 2.0 :max 160.0 :label "Contour sharpness"}]
   ['lattice_scale {:default a :min 0.5 :max 12.0 :label "Lattice wave number"}]])

(defn- contour-form [band scheme emin emax]
  (let [[cr cg cb] (palette-channels scheme 'palette_t)]
    {:params '[u fermi_level contour_step sharpness lattice_scale]
     :body (walk/postwalk-replace
            {'BAND band 'RED cr 'GREEN cg 'BLUE cb 'EMIN emin 'RANGE (- emax emin)}
            '(let [e BAND
                   contour_phase (frac (- (/ (- e fermi_level) contour_step) u))
                   line (pow (wave contour_phase) sharpness)
                   dz (- e EMIN)
                   cusp (exp (* -40.0 dz dz))
                   palette_t (/ dz RANGE)
                   cr RED cg GREEN cb BLUE
                   k (+ 0.10 (* 0.95 line))]
               [(+ (* cr k) (* 0.8 cusp))
                (+ (* cg k) (* 0.8 cusp))
                (+ (* cb k) (* 0.8 cusp))]))}))

(def graphene-fermi
  {:id :graphene_fermi
   :title "Graphene: Fermi contours rising from the Dirac points"
   :period 16 :view [8.0 4.5] :blend :lighten
   ;; Exactly overlays the unit hexagonal Brillouin lattice, unlike offline 4.2.
   :params (contour-params (/ (* 4.0 Math/PI) 3.0) 0.25)
   :form (contour-form
          '(let [p1 (* lattice_scale x)
                 p2 (* lattice_scale (+ (* 0.5 x) (* 0.8660254037844386 y)))]
             (sqrt (max 0.0 (+ 3.0 (* 2.0 (cos p1)) (* 2.0 (cos p2))
                                 (* 2.0 (cos (- p1 p2)))))))
          :yellow-purple-magenta 0.0 3.0)})

(def kagome-fermi
  {:id :kagome_fermi
   :title "Kagome: Fermi contours of the corner-sharing triangle lattice"
   :period 16 :view [8.0 4.5] :blend :lighten
   :params (contour-params 3.2 0.25)
   :form (contour-form
          '(let [q1 (* 0.5 lattice_scale x)
                 q2 (* 0.5 lattice_scale (+ (* 0.5 x) (* 0.8660254037844386 y)))
                 q3 (- q2 q1)
                 c1 (cos q1) c2 (cos q2) c3 (cos q3)
                 s (- (* 4.0 (+ (* c1 c1) (* c2 c2) (* c3 c3))) 3.0)]
             (+ 1.0 (sqrt (max 0.0 s))))
          :green-magenta 1.0 4.0)})

(def square-fermi
  {:id :square_fermi
   :title "Square lattice: diamond Fermi surfaces, four-fold"
   :period 16 :view [8.0 4.5] :blend :lighten
   :params (contour-params 3.4 0.2)
   :form (contour-form '(- 2.0 (cos (* lattice_scale x)) (cos (* lattice_scale y)))
                       :blue-cyan 0.0 4.0)})

(defn- quasi-contour-layer [id title n scheme a step]
  (let [waves (for [j (range n)
                    :let [angle (/ (* Math/PI j) n)]]
                (list 'cos (list '* 'lattice_scale
                                 (list '+ (list '* 'x (Math/cos angle))
                                       (list '* 'y (Math/sin angle))))))
        ;; Accumulate in the same order as the original primitive loop.
        sum (reduce (fn [acc wave] (list '+ acc wave)) 0.0 waves)]
    {:id id :title title :period 16 :view [8.0 4.5] :blend :lighten
     :params (contour-params a step)
     :form (contour-form (list '- (double n) sum) scheme 0.0 (* 2.0 n))}))

(def penrose-fermi
  (quasi-contour-layer :penrose_fermi "Five-fold quasicrystal: contours that never repeat"
                       5 :orange-magenta-blue 4.0 0.45))

(def heptagonal-fermi
  (quasi-contour-layer :heptagonal_fermi "Seven-fold: a symmetry no crystal can have"
                       7 :yellow-red 7.5 0.7))

(def octagonal-fermi
  (quasi-contour-layer :octagonal_fermi "Eight-fold quasicrystal: Ammann-Beenker stars"
                       8 :green-cyan 7.5 0.8))

(def quasicrystal
  (let [waves (for [j (range 7)
                    :let [a (/ (* Math/PI j) 7.0)]]
                (list 'cos (list '+ 'phase
                                 (list '* 'wave_number
                                       (list '+ (list '* 'x (Math/cos a))
                                             (list '* 'y (Math/sin a)))))))
        sum (reduce (fn [acc w] (list '+ acc w)) 0.0 waves)
        channels (mapv (fn [a b c d]
                         (quantized-channel (list 'cospal 'palette_t a b c d)))
                       [0.32 0.38 0.48] [0.32 0.32 0.30]
                       [1.0 1.0 1.0] [0.05 0.15 0.27])]
    {:id :quasicrystal :title "Seven waves: a quasicrystal breathing"
     :period 12 :view [(/ 128.0 9.0) 8.0] :blend :lighten
     ;; Wave count is fixed; loop_rate controls phase speed without a seam.
     :params [['wave_number {:default (/ (* 2.0 Math/PI) 0.9) :min 0.5 :max 14.0 :label "Wave number (seven waves)"}]
              ['contrast {:default 1.3 :min 0.2 :max 6.0 :label "Interference softening"}]
              ['phase_offset {:default 0.0 :min 0.0 :max 1.0 :label "Wave phase offset (turns)"}]]
     :form {:params '[u wave_number contrast phase_offset]
            :body (list 'let ['phase '(+ (* tau u) (* tau phase_offset))
                              'acc sum
                              ;; (1+tanh(z))/2 = 1/(1+exp(-2z)); the DSL
                              ;; has exp, not tanh. No texture approximation.
                              's '(/ 1.0 (+ 1.0 (exp (* -2.0 (/ acc contrast)))))
                              'palette_t '(+ 0.1 (* 0.85 s))]
                        channels)}}))

(def lieb-fermi
  (let [[_ _ ab] (palette-sample :blue-magenta-orange 0.15)
        [br bg _] (palette-sample :blue-magenta-orange 0.74)]
    {:id :lieb_fermi
     :title "Lieb lattice: four-fold Fermi contours and flat-band polarization"
     :period 18 :view [8.0 4.5] :blend :lighten
     :params [['lattice_scale {:default 3.8 :min 0.5 :max 12.0 :label "Lattice wave number"}]
              ['contour_step {:default 0.65 :min 0.1 :max 2.0 :label "Energy contour spacing"}]
              ['sharpness {:default 110.0 :min 2.0 :max 180.0 :label "Contour sharpness"}]
              ['probe_amplitude {:default 3.0 :min 0.0 :max 5.0 :label "Flat-band probe excursion"}]
              ['flat_band_pulse {:default 1.0 :min 0.0 :max 3.0 :label "Flat-band pulse weight"}]]
     :form {:params '[u lattice_scale contour_step sharpness probe_amplitude flat_band_pulse]
            ;; Squared nearest-integer residuals are tie-rule independent, so
            ;; floor(t+1/2) agrees with the closure's rint in the Gaussians.
            :body (walk/postwalk-replace
                   {'AB ab 'BR br 'BG bg}
                   '(let [probe (* probe_amplitude (cos (* tau u)))
                          flat (* flat_band_pulse (exp (* -16.0 probe probe)))
                          kx (* lattice_scale x) ky (* lattice_scale y)
                          cx (cos (* 0.5 kx)) cy (cos (* 0.5 ky))
                          sx (* cx cx) sy (* cy cy) s (+ sx sy)
                          e (* 2.0 (sqrt s)) b (/ sx (+ s 1.0e-12))
                          positive (pow (wave (- (/ e contour_step) u)) sharpness)
                          negative (pow (wave (+ (- (/ e contour_step)) (- u) 0.5)) sharpness)
                          cusp (exp (* -46.0 s))
                          gx (/ kx tau) gy (/ ky tau)
                          dx (- gx (floor (+ gx 0.5))) dy (- gy (floor (+ gy 0.5)))
                          ex (- gx 0.5 (floor gx)) ey (- gy 0.5 (floor gy))
                          a_dot (exp (* -2500.0 (+ (* dx dx) (* dy dy))))
                          b_dot (exp (* -2500.0 (+ (* ex ex) (* dy dy))))
                          c_dot (exp (* -2500.0 (+ (* dx dx) (* ey ey))))
                          lattice (+ (* 0.22 a_dot) (* (+ 0.14 (* 0.75 flat)) (+ b_dot c_dot)))
                          plus (* 0.91 positive) minus (* 0.86 negative)
                          hot (* 0.16 (+ (pow positive 4.0) (pow negative 4.0)))
                          flat_glow (* flat (+ 0.025 (* 0.15 (+ a_dot b_dot c_dot))))]
                      [(+ (* plus (+ 0.10 (* 0.18 b))) (* minus (+ BR 0.12)) hot
                          (* 0.65 lattice) (* 0.25 cusp) (* 0.25 flat_glow))
                       (+ (* plus (+ 0.76 (* 0.12 b))) (* minus (* 0.50 BG)) hot
                          (* 0.86 lattice) (* 0.60 cusp) flat_glow)
                       (+ (* plus 0.94) (* minus (+ AB (* 0.20 b))) hot
                          lattice (* 0.78 cusp) (* 0.72 flat_glow))]))}}))

(def brillouin-zones
  (let [s3 (Math/sqrt 3.0)
        gs (for [i (range -24 25) j (range -24 25)
                 :let [gx (+ (* i 1.0) (* j 0.5)) gy (+ (* i 0.0) (* j (/ s3 2.0)))
                       r2 (+ (* gx gx) (* gy gy))]
                 :when (and (pos? r2) (< r2 (* 9.5 9.5)))]
             [gx gy r2])
        {:keys [bindings zone nearest]}
        (reduce (fn [{:keys [bindings zone nearest]} [i [gx gy r2]]]
                  (let [d (symbol (str "plane_" i))
                        z (symbol (str "zone_" i))
                        m (symbol (str "nearest_" i))]
                    {:bindings (into bindings
                                     [d (list '* (/ 1.0 (Math/sqrt r2))
                                              (list '- (list '+ (list '* 'x gx) (list '* 'y gy)) (* 0.5 r2)))
                                      ;; ceil(d), clipped: 1 iff d>0, including
                                      ;; exact zero as 0. No soft Bragg planes.
                                      z (list '+ zone (list 'clamp (list '- (list 'floor (list '- d))) 0.0 1.0))
                                      m (list 'min nearest (list 'abs d))])
                     :zone z :nearest m}))
                {:bindings [] :zone 1.0 :nearest 1.0e9}
                (map-indexed vector gs))
        [cr cg cb] (palette-channels :blue-magenta-orange 'palette_t)
        tail (walk/postwalk-replace
              {'ZONE zone 'NEAREST nearest 'RED cr 'GREEN cg 'BLUE cb}
              '(let [q (/ NEAREST edge_width)
                     glow (exp (- (* q q)))
                     drift (- (/ ZONE pulse_zones) u)
                     pulse_phase (frac drift)
                     pulse (+ 0.25 (* 0.75 (pow (wave pulse_phase) 3.0)))
                     palette_t (- 0.5 (* 0.5 (cos (* tau drift))))
                     cr RED cg GREEN cb BLUE
                     fill (* 0.32 pulse)
                     e (* glow (+ 0.4 (* 0.6 pulse)))]
                 [(+ (* cr fill) (* e (+ 0.5 (* 0.5 cr))))
                  (+ (* cg fill) (* e (+ 0.5 (* 0.5 cg))))
                  (+ (* cb fill) (* e (+ 0.5 (* 0.5 cb))))]))]
    {:id :brillouin_zones :title "Brillouin zones of the hexagonal lattice, stained glass"
     :period 20 :view [8.0 4.5] :blend :lighten
     :params [['edge_width {:default 0.012 :min 0.002 :max 0.12 :label "Bragg-plane glow width"}]
              ['pulse_zones {:default 12.0 :min 2.0 :max 48.0 :label "Zones per light pulse"}]]
     ;; Exactly the original 312 G vectors, radius 9.5; compile-time unrolled.
     :form {:params '[u edge_width pulse_zones]
            :body (list 'let bindings tail)}}))

(defn- glow-form [distance width]
  (list 'let ['q (list '/ distance width)
              'q2 '(* q q)
              ;; Exact finite-support cutoff, not a smooth approximation.
              'inside '(clamp (+ 1.0 (floor (- 36.0 q2))) 0.0 1.0)]
        '(* inside (+ (* 0.88 (exp (- q2))) (* 0.12 (exp (* -0.16 q2)))))))

(defn- neon-form [red green blue]
  (list 'let ['red red 'green green 'blue blue
              'hi '(max red (max green blue)) 'lo '(min red (min green blue))
              'span '(- hi lo)
              'visible '(* (clamp (+ 1.0 (floor (- hi 0.003))) 0.0 1.0)
                           (clamp (+ 1.0 (floor (- span 1.0e-12))) 0.0 1.0))
              'v '(* 0.95 (- 1.0 (exp (* -1.65 hi))))
              'scale '(/ (* 0.98 v) (max span 1.0e-12))
              'base '(* 0.02 v)]
        '[(* visible (+ base (* scale (- red lo))))
          (* visible (+ base (* scale (- green lo))))
          (* visible (+ base (* scale (- blue lo))))]))

(def kaleido-quasicrystal
  (let [waves (for [j (range 5)
                    :let [a (* 2.0 Math/PI (/ j 5.0))]]
                (list 'cos (list '+ (list '* 'wave_number
                                              (list '+ (list '* 'px (Math/cos a))
                                                    (list '* 'py (Math/sin a)))) 'phase)))
        sum (reduce (fn [acc w] (list '+ acc w)) 0.0 waves)
        tail (neon-form
              '(+ (* 0.025 blue) (* 0.02 cyan) magenta (* 0.40 lime) gold orange (* 0.10 centre))
              '(+ (* 0.10 blue) (* 0.88 cyan) (* 0.025 magenta) lime (* 0.61 gold) (* 0.10 orange) (* 0.85 centre))
              '(+ blue cyan (* 0.93 magenta) (* 0.02 lime) (* 0.015 gold) (* 0.015 orange) centre))
        body (walk/postwalk-replace
              {'WAVES sum 'TAIL tail}
              '(let [phase (* tau u)
                     pulse (+ 0.85 (* 0.15 (wave (* 2.0 u))))
                     radius (sqrt (+ (* x x) (* y y)))
                     order (floor (clamp fold_order 2.0 24.0))
                     half (/ pi order) sector (* 2.0 half)
                     a (+ (atan2 y x) half)
                     angle (abs (- (- a (* sector (floor (/ a sector)))) half))
                     rho (* lens_scale (log (+ 1.0 (* 2.0 radius))))
                     px (* rho (cos angle)) py (* rho (sin angle))
                     field (* 0.5 WAVES)
                     level (* contour_density field)
                     band (floor level)
                     primary (* (clamp (+ 1.0 (floor (+ band 1.0))) 0.0 1.0)
                                (clamp (+ 1.0 (floor (- 1.0 band))) 0.0 1.0))
                     width (mix 0.020 0.052 primary) weight (mix 0.30 1.18 primary)
                     contour (- (frac level) 0.5)
                     cyan_line (* weight (glow (- contour 0.085) width))
                     magenta_line (* weight (glow (+ contour 0.085) width))
                     g1 (* 0.78 px) g2 (* 0.78 (+ (* 0.5 px) (* 0.866025403784 py)))
                     g3 (- g1 g2)
                     d1 (abs (- (frac g1) 0.5)) d2 (abs (- (frac g2) 0.5))
                     d3 (abs (- (frac g3) 0.5))
                     grid_distance (min d1 (min d2 d3))
                     grid (+ (glow (- grid_distance 0.030) 0.010)
                             (* 0.65 (glow (- grid_distance 0.065) 0.008)))
                     meander (glow (- (frac (+ (* 3.0 d1) (* 3.0 d2))) 0.5) 0.080)
                     hatch (* meander (min 1.0 (* 7.0 grid_distance)))
                     bx (- (frac (* 3.0 g1)) 0.5)
                     beads (* (glow (- d2 0.026) 0.032) (exp (* -36.0 bx bx)))
                     ex (- (frac g1) 0.5) ey (- (frac g2) 0.5)
                     eyes (exp (* -850.0 (+ (* ex ex) (* ey ey))))
                     eye_rim (* 0.5 (glow (- (+ (abs ex) (* 1.7 (abs ey))) 0.10) 0.016))
                     petal (glow (- radius (+ 0.235 (* 0.026 (cos (* 2.0 order angle))))) 0.012)
                     centre (+ (* 1.8 petal) (* 1.2 (glow (- radius 0.148) 0.013)))
                     fade (* (min 1.0 (* 5.0 radius)) (- 1.0 (* 0.065 radius)))
                     mask (* (- 1.0 (* 0.85 hatch)) (- 1.0 beads) (- 1.0 eyes))
                     blue (* fade mask 0.28 grid)
                     cyan (* fade mask cyan_line) magenta (* fade mask magenta_line)
                     lime (* fade pulse 0.82 hatch (- 1.0 beads) (- 1.0 eyes))
                     gold (* fade 1.40 beads) orange (* fade (+ (* 1.5 eyes) eye_rim))]
                 TAIL))]
    {:id :kaleido_quasicrystal
     :title "D8 UV kaleidoscope: five-wave contours through a logarithmic radial lens"
     :period 20 :view [8.0 4.5] :blend :screen
     :params [['fold_order {:default 8.0 :min 2.0 :max 24.0 :label "Dihedral fold order (integer part)"}]
              ['wave_number {:default 2.6 :min 0.5 :max 8.0 :label "Five-wave wave number"}]
              ['lens_scale {:default 3.2 :min 0.5 :max 7.0 :label "Logarithmic radial lens scale"}]
              ['contour_density {:default 1.4 :min 0.3 :max 4.0 :label "Contour density"}]]
     :form {:params '[u fold_order wave_number lens_scale contour_density]
            :body (walk/postwalk (fn [e]
                                   (if (and (seq? e) (= 'glow (first e)))
                                     (glow-form (second e) (nth e 2)) e)) body)}}))

(def layers
  "Published live layers in stable registry order."
  [graphene-fermi kagome-fermi square-fermi penrose-fermi heptagonal-fermi
   octagonal-fermi quasicrystal lieb-fermi brillouin-zones kaleido-quasicrystal])

(def by-id (into {} (map (juxt :id identity)) layers))

(def skipped
  "Candidate audit; no painter/raster approximation is presented as a port."
  {:hofstadter
   "The shipped shader bilinearly samples a precomputed 1200x720 Harper atlas (864000 constants, max-q 72). Field forms have no indexed arrays or texture lookup. Embedding that table as scalar expressions exceeds the JVM method/code-size limits; summing analytic band edges instead would change the original raster kernel and fail parity. Needs a texture/lookup field primitive, not a fake butterfly."
   :bloch_oscillation "Precomputed site-density and phase rasters; outside the scalar field-only port."
   :quantum_carpet "Samples a per-frame precomputed raster; outside the scalar field-only port."
   :girih "Frame/vector painter, not a per-pixel scalar field."})

(def follow-ups
  "Known live-host considerations, without changing the parameter-slot ABI."
  {:kaleido_quasicrystal
   "fold_order is clamped and floored in the field. desargues c3523fc ParamSpec is closed (default/min/max/label only) and trama hardcodes integers=[], so an integer slider needs upstream :integer? metadata support. Then emit integers=[fold_order] without changing the slot."
   :brillouin_zones
   "The exact 312-plane finite sum emits about 65 KB of WGSL. Naga accepts it, but GPU compilation can hitch on first load and the per-pixel cost is high at 2560x1080. The scalar field DSL has no constant arrays/indexing/loops; retain this parity-exact version until those primitives exist upstream."})

(defn bind
  "Bind a layer's field at phase u, optionally overriding named math params.
   Overrides do not change the published slot order."
  ([layer u] (bind layer u {}))
  ([layer u overrides]
   (let [defaults (into {} (map (fn [[p spec]] [p (:default spec)])) (:params layer))
         values (merge defaults overrides {'u u})]
     (apply field/bind (:form layer) (map values (get-in layer [:form :params]))))))

(defn emit-isf!
  "Emit ISF and plain GLSL files for every live layer into dir."
  [dir]
  (emit/emit-all! layers dir))

(defn emit-trama!
  "Emit one fosfora trama WGSL source per layer into dir. loop_rate is
   implicitly slot 0; no local :params adapter or local dependency needed."
  [dir]
  (emit/emit-all! (mapv #(assoc % :target :trama) layers) dir))

(def effects
  "fosfora layer-effect metadata per layer id: the catalog display name (a
   preset key: STABLE, never rename) and a short description. Names are
   checked against fosfora's built-ins by desargues.wallpaper.pfx."
  (array-map
   :graphene_fermi {:name "Graphene Fermi" :description "Graphene band energy contours rising from the Dirac points."}
   :kagome_fermi {:name "Kagome Fermi" :description "Fermi contours of the corner-sharing triangle (kagome) lattice."}
   :square_fermi {:name "Square Fermi" :description "Square-lattice Fermi surfaces: four-fold diamonds."}
   :penrose_fermi {:name "Penrose Fermi" :description "Five-fold quasicrystal contours that never repeat."}
   :heptagonal_fermi {:name "Heptagonal Fermi" :description "Seven-fold contours: a symmetry no crystal can have."}
   :octagonal_fermi {:name "Octagonal Fermi" :description "Eight-fold Ammann-Beenker quasicrystal contours."}
   :quasicrystal {:name "Quasicrystal" :description "Seven plane waves: a quasicrystal breathing."}
   :lieb_fermi {:name "Lieb Fermi" :description "Lieb lattice: four-fold Fermi contours and flat-band polarization."}
   :brillouin_zones {:name "Brillouin Zones" :description "Brillouin zones of the hexagonal lattice as stained glass."}
   :kaleido_quasicrystal {:name "Kaleido Quasicrystal" :description "D8 kaleidoscope of five-wave contours through a log radial lens."}))

(defn pfx-layer
  "Layer `l` as a fosfora layer-effect spec (:target :pfx) with its
   `effects` display name and description, author tessera, category
   pattern. Params and their order are the trama ones, untouched."
  [l]
  (assoc l :target :pfx
         :pfx (merge {:author "tessera" :category "pattern"} (effects (:id l)))))

(defn emit-pfx!
  "Emit one fosfora layer effect per layer into dir (e.g. \"out/effects\"):
   <id>.pfx + <id>.wgsl, flat. Keep it apart from the trama folder, where a
   .wgsl is routed to the trama reloader. Returns {id Result}."
  [dir]
  (emit/emit-all! (mapv pfx-layer layers) dir))
