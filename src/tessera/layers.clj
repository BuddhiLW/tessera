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

(def layers
  "Published live layers in stable registry order."
  [graphene-fermi kagome-fermi square-fermi])

(def by-id (into {} (map (juxt :id identity)) layers))

(defn bind
  "Bind a layer's field at phase u, optionally overriding named math params.
   Overrides do not change the published slot order."
  ([layer u] (bind layer u {}))
  ([layer u overrides]
   (let [defaults (into {} (map (fn [[p spec]] [p (:default spec)])) (:params layer))
         values (merge defaults overrides {'u u})]
     (apply field/bind (:form layer) (map values (get-in layer [:form :params]))))))

(defn isf-layer
  "TEMPORARY compatibility adapter for desargues 3c36c84's map-only emitter.
   The form's ordered params, not hash-map iteration, drive its ISF inputs.
   Remove this conversion when desargues accepts the native ordered vector."
  [layer]
  (update layer :params #(into {} %)))

(defn emit-isf!
  "Emit ISF and plain GLSL files for every live layer into dir."
  [dir]
  (emit/emit-all! (mapv isf-layer layers) dir))
