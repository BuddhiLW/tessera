(ns tessera.fourier
  "Closed paths as integer Fourier harmonics. Samples exclude the repeated
   endpoint. 2D uses a complex DFT (signed frequencies); 3D groups conjugate
   real-vector coefficients into elliptical orbits for spheres on rods.
   No frame history: the pen and its fading trail are functions of tau*u."
  (:require [clojure.xml :as xml]
            [desargues.player.svg-path :as svg-path]
            [desargues.wallpaper.loop :as lp]
            [clojure.java.io :as io]
            [desargues.wallpaper.space :as sp]
            [tessera.figures :as figures]
            [desargues.tex.typeset :as typeset])
  (:import (java.awt.geom AffineTransform Path2D$Double PathIterator)
           (java.io ByteArrayInputStream)))

(defn- norm [v] (Math/sqrt (reduce + 0.0 (map #(* % %) v))))
(defn- distance [a b] (norm (mapv - a b)))
(defn- mix [a b t] (mapv #(+ %1 (* t (- %2 %1))) a b))

(defn resample-points
  "M equally spaced arc-length samples of a closed polyline in 2D or 3D.
   Closes the last point to the first, ignores zero-length edges, and never
   duplicates the endpoint. Throws on empty, non-finite or constant paths."
  [points m]
  (let [pts (mapv #(mapv double %) points)
        dim (count (first pts))]
    (when-not (and (integer? m) (>= m 2) (#{2 3} dim) (seq pts)
                   (every? #(and (= dim (count %)) (every? (fn [x] (Double/isFinite x)) %)) pts))
      (throw (ex-info "Expected a finite 2D/3D path and integer M >= 2" {:m m})))
    (let [edges (filterv #(pos? (nth % 2))
                         (mapv (fn [a b] [a b (distance a b)]) pts (conj (subvec pts 1) (first pts))))
          length (reduce + 0.0 (map #(nth % 2) edges))]
      (when (zero? length) (throw (ex-info "Closed path has zero length" {})))
      (loop [i 0 j 0 start 0.0 out []]
        (if (= i m) out
            (let [[a b len] (nth edges j) s (* length (/ (double i) m))]
              (if (and (< (inc j) (count edges)) (>= s (+ start len)))
                (recur i (inc j) (+ start len) out)
                (recur (inc i) j start (conj out (mix a b (/ (- s start) len)))))))))))

(defn resample-curve
  "Resample a tessera curve f(turns)->[x y] or [x y z]. Arc length is
   approximated on a dense polyline (:resolution, default max(2048,16M))."
  ([curve m] (resample-curve curve m {}))
  ([curve m {:keys [resolution] :or {resolution (max 2048 (* 16 m))}}]
   (when-not (and (integer? resolution) (>= resolution m) (>= m 2))
     (throw (ex-info "Curve resolution must be an integer >= M >= 2" {})))
   (resample-points (mapv #(curve (/ (double %) resolution)) (range resolution)) m)))

(defn segments->points
  "Flatten desargues.player.svg-path segments to a closed polyline.
   Every subpath is closed, even if its source lacks Z, then concatenated
   in document order with straight bridges. The final bridge closes the
   whole walk. :flatness is in SVG units, default 0.01."
  ([segments] (segments->points segments {}))
  ([segments {:keys [flatness] :or {flatness 0.01}}]
   (when-not (pos? flatness) (throw (ex-info "Flatness must be positive" {})))
   (let [path (Path2D$Double.)]
     (doseq [[op & args] segments]
       (case op
         :move (let [[x y] args] (.moveTo path x y))
         :line (let [[x y] args] (.lineTo path x y))
         :quad (let [[a b x y] args] (.quadTo path a b x y))
         :cubic (let [[a b c d x y] args] (.curveTo path a b c d x y))
         :close (.closePath path)
         (throw (ex-info "Unknown SVG segment" {:op op}))))
     (let [it (.getPathIterator path nil (double flatness)) coords (double-array 6)
           close-run (fn [run] (if (seq run) (conj run (first run)) []))]
       (loop [out [] run [] current nil]
         (if (.isDone it)
           (into out (close-run run))
           (let [op (.currentSegment it coords) p [(aget coords 0) (aget coords 1)]]
             (.next it)
             (cond
               (= op PathIterator/SEG_MOVETO) (recur (into out (close-run run)) [p] p)
               ;; SVG permits a drawing command immediately after Z. Its
               ;; current point is the just-closed contour's start.
               (= op PathIterator/SEG_LINETO) (recur out (if (seq run) (conj run p) [current p]) p)
               (= op PathIterator/SEG_CLOSE) (recur (into out (close-run run)) [] (or (first run) current))
               :else (throw (ex-info "Unflattened SVG segment" {:op op}))))))))))

(defn resample-segments
  "M arc-length samples of normalized SVG segments, including contour bridges."
  ([segments m] (resample-segments segments m {}))
  ([segments m opts] (resample-points (segments->points segments opts) m)))

(defn dft
  "Normalized DFT of finite samples. 2D: z=x+iy, all M signed frequencies,
   even-M Nyquist assigned -M/2. 3D: conjugate pairs become one real vector
   harmonic A*cos(k*tau*u)+B*sin(k*tau*u), DC/Nyquist not doubled.
   :magnitude is radius in 2D, RMS vector length in 3D (DC uses its norm)."
  [points]
  (let [pts (vec points) m (count pts) dim (count (first pts))]
    (when-not (and (pos? m) (#{2 3} dim)
                   (every? #(and (= dim (count %)) (every? (fn [x] (and (number? x) (Double/isFinite (double x)))) %)) pts))
      (throw (ex-info "DFT needs finite, consistently dimensional samples" {})))
    (mapv
     (fn [j]
       (let [k (if (and (= dim 2) (> j (quot (dec m) 2))) (- j m) j)
             cs (mapv #(Math/cos (* lp/tau k (/ (double %) m))) (range m))
             sn (mapv #(Math/sin (* lp/tau k (/ (double %) m))) (range m))
             avg (fn [xs] (/ (reduce + 0.0 xs) m))
             [a b] (if (= dim 2)
                     (let [re (avg (map (fn [[x y] c s] (+ (* x c) (* y s))) pts cs sn))
                           im (avg (map (fn [[x y] c s] (- (* y c) (* x s))) pts cs sn))]
                       [[re im] [(- im) re]])
                     (let [factor (if (or (zero? j) (and (even? m) (= j (quot m 2)))) 1.0 2.0)]
                       [(mapv (fn [axis] (* factor (avg (map #(* (nth %1 axis) %2) pts cs)))) (range dim))
                        (mapv (fn [axis] (* factor (avg (map #(* (nth %1 axis) %2) pts sn)))) (range dim))]))
             magnitude (if (or (= dim 2) (zero? k)) (norm a)
                           (Math/sqrt (* 0.5 (+ (reduce + (map #(* % %) a)) (reduce + (map #(* % %) b))))))]
         {:frequency k :cos a :sin b :magnitude magnitude}))
     (range (if (= dim 2) m (inc (quot m 2)))))))

(defn top-terms
  "Keep at most N terms, largest magnitude first, with deterministic ties."
  [terms n]
  (when-not (and (integer? n) (<= 0 n)) (throw (ex-info "N must be a nonnegative integer" {:n n})))
  (vec (take n (sort-by (juxt (comp - :magnitude) #(abs (:frequency %)) :frequency) terms))))

(defn term-point
  "One harmonic at phase u. All temporal angles are integer multiples of tau*u."
  [{:keys [frequency cos sin]} u]
  (let [a (* (double frequency) lp/tau (lp/frac (double u))) c (Math/cos a) s (Math/sin a)]
    (mapv #(+ (* c %1) (* s %2)) cos sin)))

(defn chain
  "The origin followed by every rod endpoint, in magnitude order."
  [terms u]
  (vec (reductions #(mapv + %1 (term-point %2 u))
                   (vec (repeat (count (:cos (first terms) [0.0 0.0])) 0.0)) terms)))

(defn point
  "The Fourier pen (the head of the epicycle chain), without frame history."
  [terms u]
  (peek (chain terms u)))

(defn fit-points
  "Centre the bounding box and uniformly fit points into width x height.
   SVG sources can use :flip-y? true to change y-down to scene y-up."
  [points width height & [{:keys [flip-y?]}]]
  (let [pts (vec points) axes (apply mapv vector pts)
        lo (mapv #(apply min %) axes) hi (mapv #(apply max %) axes)
        centre (mapv #(* 0.5 (+ %1 %2)) lo hi)
        scale (min (/ width (max 1.0e-12 (- (hi 0) (lo 0))))
                   (/ height (max 1.0e-12 (- (hi 1) (lo 1)))))]
    (mapv (fn [p] (let [q (mapv #(* scale (- %1 %2)) p centre)]
                   (if flip-y? (update q 1 -) q))) pts)))

(defn- numbers [s]
  (mapv parse-double (re-seq #"[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?" (or s ""))))

(defn- transform [s]
  (let [t (AffineTransform.)]
    (doseq [[_ op args] (re-seq #"([A-Za-z]+)\s*\(([^)]*)\)" (or s ""))]
      (let [[a b c d e f] (numbers args)]
        (case op
          "matrix" (.concatenate t (AffineTransform. a b c d e f))
          "translate" (.translate t a (or b 0.0))
          "scale" (.scale t a (or b a))
          "rotate" (if b (.rotate t (Math/toRadians a) b c) (.rotate t (Math/toRadians a)))
          (throw (ex-info "Unsupported SVG transform" {:transform op})))))
    t))

(defn svg->segments
  "Drawing segments of an SVG document, including dvisvgm's defs/use glyphs
   and fraction rects, nested translate/scale/rotate/matrix transforms.
   Definitions are not drawn until referenced; use x/y offsets are applied.
   This is a path/rect/use importer, not a general SVG renderer."
  [svg]
  (let [root (xml/parse (ByteArrayInputStream. (.getBytes ^String svg "UTF-8")))
        nodes (filter map? (tree-seq map? :content root))
        defs (into {} (keep #(when-let [id (get-in % [:attrs :id])] [id %])) nodes)
        apply-t (fn [^AffineTransform t seg]
                  (into [(first seg)] (mapcat (fn [[x y]]
                                               (let [p (double-array [x y])]
                                                 (.transform t p 0 p 0 1) (vec p)))
                                             (partition 2 (rest seg)))))]
    (letfn [(visit [{:keys [tag attrs content]} ^AffineTransform parent seen]
              (let [t (AffineTransform. parent)]
                (.concatenate t (transform (:transform attrs)))
                (case tag
                  :defs []
                  :path (mapv #(apply-t t %) (svg-path/segments (:d attrs)))
                  :rect (let [x (parse-double (get attrs :x "0")) y (parse-double (get attrs :y "0"))
                              w (parse-double (:width attrs)) h (parse-double (:height attrs))]
                          (mapv #(apply-t t %) [[:move x y] [:line (+ x w) y] [:line (+ x w) (+ y h)] [:line x (+ y h)] [:close]]))
                  :use (let [href (or (:xlink:href attrs) (:href attrs)) id (subs href 1)]
                         (when (or (seen id) (nil? (defs id))) (throw (ex-info "Missing or cyclic SVG use" {:id id})))
                         (.translate t (parse-double (get attrs :x "0")) (parse-double (get attrs :y "0")))
                         (visit (defs id) t (conj seen id)))
                  (mapcat #(visit % t seen) (filter map? content)))))]
      (vec (visit root (AffineTransform.) #{})))))

(def gauss-law "\\nabla\\!\\cdot\\!\\mathbf{E}=\\frac{\\rho}{\\varepsilon_0}")

(defn typeset-formula
  "Boundary: typeset a formula through desargues.tex.typeset. Returns
   :skipped if latex/dvisvgm are absent, :error on typesetting failure, or
   SVG plus drawing segments. Registry loading NEVER launches a process."
  ([] (typeset-formula (typeset/latex-typesetter) gauss-law))
  ([typesetter formula]
   (if-not (typeset/available? typesetter)
     {:skipped "latex and dvisvgm must both be installed"}
     (let [r (typeset/typeset typesetter formula "#ffffff")]
       (if (:error r) r (assoc r :segments (svg->segments (:svg r))))))))

(def view [(/ 128.0 9.0) 8.0])

(def trefoil-terms
  "Top 24 harmonics of the arc-length resampled trefoil's XY projection."
  (top-terms (dft (resample-curve #(subvec (figures/trefoil %) 0 2) 512)) 24))

(def space-terms
  "Top 24 real-vector harmonics of the arc-length resampled space trefoil."
  (top-terms (dft (resample-curve figures/trefoil 512)) 24))

(def formula-terms
  "The checked-in dvisvgm Gauss-law asset, fitted and resampled. Delayed so
   numerical clients do not need an asset. No latex dependency at runtime."
  (delay
    (when-let [url (io/resource "tessera/fourier/gauss-law.svg")]
      (-> (slurp url) svg->segments (resample-segments 768)
          (fit-points 10.0 4.6 {:flip-y? true}) dft (top-terms 96)))))

(defn trail
  "Analytic Fourier pen samples from u-length to u, oldest first. Fixed
   age-indexed opacities avoid a history buffer or a reset at the seam."
  [terms u length samples]
  (mapv #(point terms (- (double u) (* length (- 1.0 (/ (double %) samples)))))
        (range (inc samples))))

(defn epicycle-frame
  "2D circle chain, rods, fading trail and the pen at the chain's head."
  [terms guide u]
  (let [u (lp/frac (double u)) ends (chain terms u)
        pts (trail terms u 0.88 600)]
    {:background "#000000"
     :shapes
     (vec
      (concat
       [{:node :path :points guide :opts {:closed? true} :opacity 0.16
         :stroke {:color "#14768f" :width 1.4}}]
       (mapcat (fn [term a b]
                 (when (> (:magnitude term) 0.006)
                   (concat
                    (when-not (zero? (:frequency term))
                      [{:node :circle :at a :opts {:radius (:magnitude term)} :opacity 0.26
                        :stroke {:color "#27bed6" :width 1.2}}])
                    [{:node :line :from a :to b :opacity 0.64
                      :stroke {:color "#ee9659" :width 1.8}}])))
               terms ends (rest ends))
       (mapcat (fn [i a b]
                 (let [k (Math/pow (/ (double (inc i)) 600.0) 2.2)]
                   [{:node :line :from a :to b :opacity (* 0.25 k)
                     :stroke {:color "#d62bea" :width 8.0}}
                    {:node :line :from a :to b :opacity (+ 0.02 (* 0.96 k))
                     :stroke {:color "#53e4f5" :width 2.6}}]))
               (range 600) pts (rest pts))
       [{:node :dot :at (peek ends) :opts {:radius 0.12} :opacity 0.3
         :fill {:color "#fd36d5"} :stroke {:color nil}}
        {:node :dot :at (peek ends) :opts {:radius 0.048}
         :fill {:color "#ffe4bc"} :stroke {:color nil}}]))}))

(defn epicycle-loop
  "An offline epicycle spec from a precomputed 2D spectrum."
  [id title terms]
  (let [guide (mapv #(point terms (/ % 720.0)) (range 720))]
    {:id id :title title :seconds 20 :fps 30 :size [1920 1080] :view view
     :frame #(epicycle-frame terms guide %)
     :post (fn [_] [{:type :bloom :radius 10 :strength 0.75}
                    {:type :vignette :strength 0.25}])}))

(def fourier-trefoil
  (epicycle-loop :fourier-trefoil "Fourier trefoil: 24 rotating circles and a pen of light" trefoil-terms))

(def fourier-formula
  (when-let [terms @formula-terms]
    (assoc (epicycle-loop :fourier-formula "Gauss's law: a TeX formula drawn by 96 integer harmonics" terms)
           :seconds 32)))

(def camera
  (sp/camera {:eye [5.0 -14.0 8.0] :target [0.0 0.0 0.0] :fov 34.0 :view view}))

(defn- sphere
  "A projected sphere with five nested Lambert-like light bands; its world
   radius is projected at its depth, as are the rods and tube segments."
  [p radius]
  (when-let [[x y d] (sp/project camera p)]
    (let [r (* radius (sp/magnification camera d))]
      (map-indexed
       (fn [i [scale dx dy colour]]
         {:node :dot :at [(+ x (* dx r)) (+ y (* dy r))]
          :depth (- d (* i 1.0e-5)) :opts {:radius (* r scale)}
          :fill {:color colour} :stroke {:color nil}})
       [[1.0 0.0 0.0 "#462478"] [0.83 -0.10 0.10 "#a832b5"]
        [0.59 -0.23 0.22 "#f35cce"] [0.31 -0.35 0.32 "#ffa4d7"]
        [0.12 -0.42 0.38 "#ffe9ca"]]))))

(defn space-frame
  "Vector Fourier orbits carrying spheres on rods, drawing a trefoil tube.
   Real 3D conjugate pairs describe ellipses (sometimes lines), not complex
   circles; each is traversed at its integer harmonic frequency."
  [u]
  (let [u (lp/frac (double u)) ends (chain space-terms u)
        pts (trail space-terms u 0.88 480)]
    (sp/frame
     (concat
      (mapcat (fn [term a b]
                (when (> (:magnitude term) 0.008)
                  (let [orbit (mapv #(mapv + a (term-point (assoc term :frequency 1) (/ % 96.0))) (range 97))]
                    (concat
                     (sp/path-shapes camera orbit {:color "#207a9d" :width 1.0 :opacity 0.27})
                     (sp/segment-shapes camera [a b] {:color "#eea252" :width 2.0 :opacity 0.7})
                     (sphere b (min 0.15 (+ 0.038 (* 0.07 (Math/sqrt (:magnitude term))))))))))
              space-terms ends (rest ends))
      (mapcat (fn [i a b]
                (let [k (Math/pow (/ (double (inc i)) 480.0) 1.8)]
                  (concat
                   (sp/segment-shapes camera [a b] {:color "#ad27e8" :width 10.0 :opacity (* 0.22 k)})
                   (sp/segment-shapes camera [a b] {:color "#3fe3ea" :width 4.0 :opacity (+ 0.02 (* 0.94 k))}))))
              (range 480) pts (rest pts))
      (sphere (peek ends) 0.075))
     {:background "#000000"})))

(def fourier-space
  {:id :fourier-space :title "Fourier in space: spheres on rods draw a trefoil tube"
   :seconds 20 :fps 30 :size [1920 1080] :view view :frame space-frame
   :post (fn [_] [{:type :bloom :radius 12 :strength 0.65}
                  {:type :vignette :strength 0.25}])})

(def loops
  "Offline Fourier loops; the formula is omitted if its asset is absent."
  (into (array-map :fourier-trefoil fourier-trefoil :fourier-space fourier-space)
        (when fourier-formula [[:fourier-formula fourier-formula]])))
