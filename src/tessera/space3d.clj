(ns tessera.space3d
  "Luminous 3D flows with fixed cameras and fixed scene poses. Hopf fibres
   breathe on S2; beads, knot pulses and Lissajous comets make integer laps.
   No scene or camera rotates. Every time-dependent quantity is 1-periodic."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.space :as sp]
            [tessera.palette :as pal]))

(def view [(/ 128.0 9.0) 8.0])

(def spectrum
  "Walk a thi.ng cosine scheme there and back, so even noninteger palette
   frequencies repeat exactly when a loop's colour phase advances by one."
  (pal/around (pal/scheme :blue-magenta-orange)))

(def flow-palettes
  "Tessera cosine palettes with bounded channel ranges: each family keeps
   its own hue identity instead of every curve cycling through a rainbow."
  {:teal-ice (pal/cosine [0.20 0.72 0.88] [0.12 0.22 0.11]
                         [1.0 1.0 1.0] [0.0 0.08 0.17])
   :gold-ember (pal/cosine [0.89 0.43 0.10] [0.10 0.26 0.08]
                           [1.0 1.0 1.0] [0.0 0.12 0.24])
   :violet-rose (pal/cosine [0.68 0.18 0.83] [0.26 0.13 0.16]
                            [1.0 1.0 1.0] [0.0 0.10 0.18])
   :azure (pal/cosine [0.12 0.43 0.89] [0.08 0.23 0.10]
                      [1.0 1.0 1.0] [0.05 0.13 0.22])
   :mint (pal/cosine [0.24 0.80 0.60] [0.16 0.18 0.22]
                     [1.0 1.0 1.0] [0.0 0.10 0.20])})

(defn pigment
  "Matching CSS stroke and [0,1] splat channels from tessera.palette."
  ([phase] (pigment spectrum phase))
  ([palette phase]
   (let [c (pal/at palette (double phase))]
     {:css (format "#%06x" c)
      :rgb (mapv #(/ (double (bit-and 255 (bit-shift-right c %))) 255.0)
                 [16 8 0])})))

(defn curve-strokes
  "Short overlapping arcs retain local depth cues without thousands of lines."
  [cam cue points color opacity width]
  (mapcat #(sp/path-shapes cam % {:color color :width width
                                :opacity opacity :cue cue})
          (partition-all 17 16 points)))

(defn flow-rows
  "Additive comet heads and exponentially fading tails. `point` is a closed
   curve of period one; laps is an integer. Offsets stagger copies in time
   (the bleuje trick), never by turning their geometry."
  [cam cue point u {:keys [heads laps offset rgb radius tail steps intensity]
                    :or {heads 3 laps 1 offset 0.0 radius 0.035
                         tail 0.045 steps 12 intensity 2.4}}]
  (into []
        (mapcat
         (fn [j]
           (let [age (/ (double j) (dec steps))
                 phase (- (+ (* laps (double u)) offset) (* tail age))
                 pts (mapv #(point (+ phase (/ (double %) heads))) (range heads))]
             (sp/splat-rows cam pts
                            {:radius (* radius (- 1.0 (* 0.55 age)))
                             :color rgb :cue cue
                             :intensity (* intensity (Math/exp (* -4.0 age)))})))
         (range steps))))

(def hopf-camera
  "Fixed side perspective. Aim below the world origin to centre the
   perspective silhouette; all fibres clear both the frame and near plane."
  (sp/camera {:eye [0.0 -11.0 4.2] :target [0.0 0.0 -0.38]
              :up [0.0 0.0 1.0] :fov 29.0 :near 0.1 :view view}))

(def hopf-cue
  {:range [8.0 15.5] :alpha [1.0 0.3] :size [1.0 0.65] :fog [0.0 0.35]})

(def hopf-fibres
  "S2 base points grouped into four latitude circles, hence nested tori."
  (vec (for [[ring eta n] [[0 0.38 12] [1 0.62 16] [2 0.85 20] [3 1.03 24]]
             j (range n)]
         {:ring ring :eta eta :phi (* lp/tau (/ (double j) n))
          :offset (lp/frac (+ (* 0.61803398875 j) (* 0.17 ring)))})))

(defn hopf-point
  "Villarceau fibre (exp(it) z1, exp(it) z2) on S3, stereographic to R3.
   Latitudes breathe and longitudes rock oppositely on S2: no global spin.
   eta stays below 1.08, so 1-y2 is strictly positive; no clamped projection."
  [{:keys [eta phi ring]} t u]
  (let [e (+ eta (* 0.04 (Math/sin (+ (* lp/tau u) (* 0.7 ring)))))
        p (+ phi (* 0.10 (Math/sin (* lp/tau u)) (Math/cos (* 2.0 phi))))
        a (* lp/tau t)
        c (Math/cos e) s (Math/sin e)
        k (/ 1.0 (- 1.0 (* s (Math/sin (+ a p)))))]
    [(* k c (Math/cos a)) (* k c (Math/sin a))
     (* k s (Math/cos (+ a p)))]))

(defn hopf-frame [u]
  (sp/frame
   (mapcat (fn [{:keys [ring] :as fibre}]
             (curve-strokes hopf-camera hopf-cue
                            (mapv #(hopf-point fibre (/ (double %) 192.0) u)
                                  (range 193))
                            (:css (pigment (+ (* 0.17 ring) (* 0.035 (lp/wave u)))))
                            0.72 2.0))
           hopf-fibres)
   {:background "#02040d"}))

(defn hopf-splats [u]
  (into []
        (mapcat (fn [{:keys [ring offset] :as fibre}]
                  (flow-rows hopf-camera hopf-cue #(hopf-point fibre % u) u
                             {:heads 5 :laps 1 :offset offset :steps 7
                              :tail 0.018 :radius 0.032 :intensity 2.8
                              :rgb (:rgb (pigment (+ (* 0.17 ring)
                                                     (* 0.035 (lp/wave u)))))})))
        hopf-fibres))

(def hopf-fibration
  {:id :hopf-fibration :title "Hopf fibration: breathing tori, flowing light"
   :seconds 16 :fps 30 :size [1920 1080] :view view
   :frame hopf-frame :splat hopf-splats :exposure 1.0
   :blur {:samples 3 :shutter 0.55}
   :post (fn [_] [{:type :bloom :radius 12 :strength 0.95}
                  {:type :vignette :strength 0.3}])})

(def field-camera
  "Fixed perspective shared by the knot garden and Lissajous lattice.
   Target below the world origin compensates foreground perspective."
  (sp/camera {:eye [0.0 -16.0 9.0] :target [0.0 0.6 -1.5]
              :up [0.0 0.0 1.0] :fov 35.0 :near 0.1 :view view}))

(def field-cue
  {:range [12.0 26.0] :alpha [1.0 0.32] :size [1.0 0.7] :fog [0.0 0.4]})

(defn fixed-pose
  "Place local coordinates at a fixed inclination, independent of loop phase."
  [{:keys [centre scale tilt] :or {scale 1.0 tilt 0.0}} [x y z]]
  (let [c (Math/cos tilt) s (Math/sin tilt)]
    (sp/v+ centre (sp/v* scale [x (- (* c y) (* s z))
                                (+ (* s y) (* c z))]))))

(def knot-garden
  "Six separated knots: type-specific teal, amber and violet light, in two
   depth rows. The golden-ratio time offsets never affect the fixed poses."
  (vec (for [row (range 2) col (range 3)
             :let [i (+ col (* 3 row))]]
         {:pq (nth [[2 3] [3 5] [2 7]] col)
          :palette (nth [:teal-ice :gold-ember :violet-rose] col)
          :centre [(* 5.0 (- col 1.0)) (* 3.6 (- (* 2 row) 1.0))
                   (* 0.2 row)]
          :scale 1.35 :tilt (+ 0.6 (* 0.06 row))
          :offset (lp/frac (* 0.61803398875 i))
          :color-phase (* 0.14 row)})))

(defn knot-point
  "Closed (p,q) torus knot at parameter t; independent of animation time."
  [{[p q] :pq :as knot} t]
  (let [a (* lp/tau t)
        r (+ 1.08 (* 0.43 (Math/cos (* q a))))]
    (fixed-pose knot [(* r (Math/cos (* p a)))
                      (* r (Math/sin (* p a)))
                      (* 0.43 (Math/sin (* q a)))])))

(def knot-underlay
  "Faint thin full knots; the much thicker travelling light draws the eye."
  (sp/frame
   (mapcat (fn [{:keys [palette color-phase] :as knot}]
             (curve-strokes field-camera field-cue
                            (mapv #(knot-point knot (/ (double %) 320.0)) (range 321))
                            (:css (pigment (flow-palettes palette) color-phase))
                            0.38 1.5))
           knot-garden)
   {:background "#02070d"}))

(defn knot-splats [u]
  (into []
        (mapcat (fn [{:keys [offset palette color-phase] :as knot}]
                  (flow-rows field-camera field-cue #(knot-point knot %) u
                             {:heads 3 :laps 1 :offset offset :steps 42
                              :tail 0.085 :radius 0.064 :intensity 2.5
                              :rgb (:rgb (pigment (flow-palettes palette)
                                                  (+ color-phase (* 0.05 (lp/wave u)))))})))
        knot-garden))

(def torus-knots
  {:id :torus-knots :title "Knot garden: travelling neon pulses in perspective"
   :seconds 18 :fps 30 :size [1920 1080] :view view
   :frame (fn [_] knot-underlay) :splat knot-splats :exposure 1.1
   :blur {:samples 3 :shutter 0.5}
   :post (fn [_] [{:type :bloom :radius 13 :strength 1.1}
                  {:type :dof :focus 0.56 :band 0.26 :radius 5 :fade 0.25}
                  {:type :vignette :strength 0.28}])})

(def lissajous-lattice
  "Twelve distinct integer triples, one per separated 4x3 projected cell.
   Rows recede in real depth. Their fixed centres lie on camera-parallel
   planes; scaling the row spacing with depth keeps the lattice legible."
  (vec (for [row (range 3) col (range 4)
             :let [i (+ col (* 4 row)) depth (+ 15.0 (* 2.5 row))
                   k (/ depth (:focal field-camera))]]
         {:freq (nth [[1 2 1] [1 3 2] [2 3 1] [2 1 3]
                      [3 2 1] [1 4 3] [3 4 2] [2 5 3]
                      [3 1 4] [2 3 4] [4 3 1] [3 5 4]] i)
          :delta [0.0 (* 0.35 i) (+ 0.65 (* 0.11 i))]
          :centre (sp/v+ (:eye field-camera)
                         (sp/v+ (sp/v* depth (:forward field-camera))
                                (sp/v+ (sp/v* (* k 3.45 (- col 1.5)) (:right field-camera))
                                       (sp/v* (* k 2.55 (- row 1.0)) (:up field-camera)))))
          :scale 1.05 :row row :col col
          :palette (nth [:azure :teal-ice :mint :teal-ice] col)
          :offset (lp/frac (* 0.61803398875 i))
          :laps 1 :color-phase (* 0.13 row)})))

(defn lissajous-point
  "A 3D Lissajous of period one in t, with its fixed axes aligned to the
   camera basis. The third oscillation makes perspective depth, not spin."
  [{:keys [freq delta centre scale]} t]
  (let [[x y z] (mapv (fn [n phase amp]
                        (* scale amp (Math/sin (+ (* lp/tau n t) phase))))
                      freq delta [1.18 1.0 0.65])]
    (sp/v+ centre
           (sp/v+ (sp/v* x (:right field-camera))
                  (sp/v+ (sp/v* y (:up field-camera))
                         (sp/v* z (:forward field-camera)))))))

(def lissajous-underlay
  (sp/frame
   (mapcat (fn [{:keys [palette color-phase] :as curve}]
             (curve-strokes field-camera field-cue
                            (mapv #(lissajous-point curve (/ (double %) 320.0))
                                  (range 321))
                            (:css (pigment (flow-palettes palette) color-phase))
                            0.27 1.6))
           lissajous-lattice)
   {:background "#01070e"}))

(defn lissajous-splats [u]
  (into []
        (mapcat (fn [{:keys [offset palette color-phase laps] :as curve}]
                  (flow-rows field-camera field-cue #(lissajous-point curve %) u
                             {:heads 1 :laps laps :offset offset :steps 48
                              :tail 0.11 :radius 0.065 :intensity 2.7
                              :rgb (:rgb (pigment (flow-palettes palette)
                                                  (+ color-phase (* 0.05 (lp/wave u)))))})))
        lissajous-lattice))

(def lissajous-3d
  {:id :lissajous-3d :title "Lissajous lattice: ice-blue comets in twelve cells"
   :seconds 20 :fps 30 :size [1920 1080] :view view
   :frame (fn [_] lissajous-underlay) :splat lissajous-splats :exposure 1.0
   :blur {:samples 3 :shutter 0.45}
   :post (fn [_] [{:type :bloom :radius 14 :strength 1.05}
                  {:type :dof :focus 0.75 :band 0.24 :radius 3 :fade 0.16}
                  {:type :vignette :strength 0.25}])})

(def loops
  {:hopf-fibration hopf-fibration
   :torus-knots torus-knots
   :lissajous-3d lissajous-3d})
