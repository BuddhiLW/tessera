(ns tessera.fourier
  "Closed paths as integer Fourier harmonics. Samples exclude the repeated
   endpoint. 2D uses a complex DFT (signed frequencies); 3D groups conjugate
   real-vector coefficients into elliptical orbits for spheres on rods.
   No frame history: the pen and its fading trail are functions of tau*u."
  (:require [clojure.xml :as xml]
            [desargues.player.svg-path :as svg-path]
            [desargues.wallpaper.loop :as lp])
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
       (loop [out [] run []]
         (if (.isDone it)
           (into out (close-run run))
           (let [op (.currentSegment it coords) p [(aget coords 0) (aget coords 1)]]
             (.next it)
             (cond
               (= op PathIterator/SEG_MOVETO) (recur (into out (close-run run)) [p])
               (= op PathIterator/SEG_LINETO) (recur out (conj run p))
               (= op PathIterator/SEG_CLOSE) (recur (into out (close-run run)) [])
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
