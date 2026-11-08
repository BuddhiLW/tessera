(ns tessera.compose.layout
  "Layouts: where motifs go.

   Continuous layouts are CHART STEPS: coordinate maps run per pixel over a
   double[] context of named slots (:x :y are the screen point). A step
   spec {:type k :in [..] :out [..] ...} compiles, given a slot allocator,
   to (fn [u] IStep). Steps are folds, lenses and lattices; none rotates
   the frame (decision 20261107180334): dihedral folds are stationary
   mirror quotients and zooms advance by an exact self-similar ratio.

   Discrete layouts produce SITES (a vector of maps with :x :y and extras)
   for sprite layers: Poisson disc, lattice, isometric grid, curves."
  (:require [desargues.wallpaper.loop :as lp]
            [tessera.compose.core :as c])
  (:import (java.util Random)
           (tessera.compose.core IStep)))

(set! *warn-on-reflection* true)


(defn- io [spec k default] (get spec k default))

(defmethod c/node :polar [spec slot]
  (let [[x y] (io spec :in [:x :y]) [r t] (io spec :out [:r :theta])
        ix (long (slot x)) iy (long (slot y)) ir (long (slot r)) it (long (slot t))]
    (fn [_]
      (reify IStep
        (run [_ c]
          (let [x (aget c ix) y (aget c iy)]
            (aset c ir (Math/hypot x y))
            (aset c it (Math/atan2 y x))))))))

(defmethod c/node :dihedral-fold
  ;; D_n mirror quotient of a polar angle: :angle in [0, pi/n] (:signed? ->
  ;; [-pi/n, pi/n], the unreflected rotation quotient), :wedge the index.
  [{:keys [n signed?] :as spec} slot]
  (let [[t] (io spec :in [:theta]) [a w] (io spec :out [:angle :wedge])
        n (long n) _ (when (< n 2) (throw (ex-info "D_n needs n>=2" {:n n})))
        it (long (slot t)) ia (long (slot a)) iw (long (slot w))
        half (/ Math/PI n)]
    (fn [_]
      (reify IStep
        (run [_ c]
          (let [theta (aget c it)
                folded (- (mod (+ theta half) (* 2.0 half)) half)]
            (aset c ia (if signed? folded (Math/abs folded)))
            (aset c iw (Math/floor (/ (+ theta half) (* 2.0 half))))))))))

(defmethod c/node :log-lens
  ;; rho = a log(1 + b r): a stationary radial lens.
  [{:keys [a b] :as spec} slot]
  (let [[r] (io spec :in [:r]) [o] (io spec :out [:rho])
        ir (long (slot r)) io* (long (slot o)) a (double a) b (double b)]
    (fn [_] (reify IStep (run [_ c] (aset c io* (* a (Math/log1p (* b (aget c ir))))))))))

(defn travel-phase
  "One texture period per loop, gently paced: C2 speed is minimal at the wrap."
  ^double [^double u ^double ease]
  (- u (* ease (/ (Math/sin (* lp/tau u)) lp/tau))))

(defmethod c/node :log-zoom
  ;; q = log2(r / r0) - v(u): advancing q by one per loop is an exact
  ;; dyadic self-similar zoom (u+1 relabels the octave), never a spin.
  [{:keys [r0 ease] :or {ease 0.72} :as spec} slot]
  (let [[r] (io spec :in [:r]) [q] (io spec :out [:q])
        ir (long (slot r)) iq (long (slot q)) r0 (double r0) ease (double ease)]
    (fn [u]
      (let [v (travel-phase (double u) ease)]
        (reify IStep
          (run [_ c]
            (aset c iq (- (/ (Math/log (max 1.0e-12 (/ (aget c ir) r0))) (Math/log 2.0)) v))))))))

(defmethod c/node :cartesian [spec slot]
  (let [[r a] (io spec :in [:rho :angle]) [x y] (io spec :out [:px :py])
        ir (long (slot r)) ia (long (slot a)) ix (long (slot x)) iy (long (slot y))]
    (fn [_]
      (reify IStep
        (run [_ c]
          (let [r (aget c ir) a (aget c ia)]
            (aset c ix (* r (Math/cos a)))
            (aset c iy (* r (Math/sin a)))))))))

(defmethod c/node :tunnel-map
  ;; Exact pinhole ray / unit cylinder, focal length f: depth z = f/r,
  ;; axial t = z - u (one axial period per loop), circumferential
  ;; s = cols theta/tau + shear sin(tau t) (a local wave, not a spin).
  [{:keys [focal cols shear] :or {focal 3.0 cols 12 shear 0.12} :as spec} slot]
  (let [[r t] (io spec :in [:r :theta]) [s a z] (io spec :out [:s :t :z])
        ir (long (slot r)) it (long (slot t)) is (long (slot s)) ia (long (slot a)) iz (long (slot z))
        focal (double focal) cols (double cols) shear (double shear)]
    (fn [u]
      (let [u (double u)]
        (reify IStep
          (run [_ c]
            (let [z (/ focal (aget c ir)) t (- z u)]
              (aset c iz z) (aset c ia t)
              (aset c is (+ (* cols (/ (aget c it) lp/tau)) (* shear (Math/sin (* lp/tau t))))))))))))

(defmethod c/node :lattice-cell
  ;; Square cell-local coordinates a, b in [-1/2, 1/2) and the cell index.
  [spec slot]
  (let [[s t] (io spec :in [:s :t]) [a b i j] (io spec :out [:a :b :i :j])
        is (long (slot s)) it (long (slot t)) ia (long (slot a)) ib (long (slot b))
        ii (long (slot i)) ij (long (slot j))]
    (fn [_]
      (reify IStep
        (run [_ c]
          (let [s (aget c is) t (aget c it)]
            (aset c ia (- (lp/frac s) 0.5)) (aset c ib (- (lp/frac t) 0.5))
            (aset c ii (Math/floor s)) (aset c ij (Math/floor t))))))))

(defmethod c/node :tri-lattice
  ;; A2 net: three line families g1, g2, g3 = g1 - g2 at `scale`, their
  ;; distances to the nearest half-integer line, and the minimum.
  [{:keys [scale] :as spec} slot]
  (let [[x y] (io spec :in [:px :py])
        [g1 g2 d1 d2 d3 dm] (io spec :out [:g1 :g2 :d1 :d2 :d3 :grid-d])
        ix (long (slot x)) iy (long (slot y)) i1 (long (slot g1)) i2 (long (slot g2))
        j1 (long (slot d1)) j2 (long (slot d2)) j3 (long (slot d3)) jm (long (slot dm))
        k (double scale)]
    (fn [_]
      (reify IStep
        (run [_ c]
          (let [px (aget c ix) py (aget c iy)
                a (* k px) b (* k (+ (* 0.5 px) (* 0.866025403784 py))) g3 (- a b)
                e1 (Math/abs (- (lp/frac a) 0.5)) e2 (Math/abs (- (lp/frac b) 0.5))
                e3 (Math/abs (- (lp/frac g3) 0.5))]
            (aset c i1 a) (aset c i2 b) (aset c j1 e1) (aset c j2 e2) (aset c j3 e3)
            (aset c jm (min e1 e2 e3))))))))

(defn poisson-disc
  "Deterministic dart throwing with a strict minimum distance over the
   rectangle [-w/2, w/2] x [-h/2, h/2]. Exhausted packing returns the
   accepted set, never a fallback that breaks exclusion."
  ([seed target distance] (poisson-disc seed target distance [8.8 5.1]))
  ([seed target distance [w h]]
   (let [rng (Random. (long seed)) d2 (* (double distance) (double distance))
         w (double w) h (double h)]
     (loop [sites [] attempts 0]
       (if (or (= (count sites) target) (= attempts 40000)) sites
           (let [x (- (* w (.nextDouble rng)) (/ w 2.0))
                 y (- (* h (.nextDouble rng)) (/ h 2.0))
                 okay? (every? (fn [[a b]]
                                 (let [dx (- x (double a)) dy (- y (double b))]
                                   (>= (+ (* dx dx) (* dy dy)) d2))) sites)]
             (recur (if okay? (conj sites [x y]) sites) (inc attempts))))))))

(defmulti sites
  "Discrete layout spec {:type k ...} -> vector of site maps {:x :y :i ...}.
   Open: register further placements with defmethod."
  :type)

(defn- indexed [pts] (into [] (map-indexed (fn [i [x y]] {:i i :x (double x) :y (double y)})) pts))

(defmethod sites :poisson [{:keys [seed count distance extent sort-by-y?]}]
  (let [pts (poisson-disc seed count distance (or extent [8.8 5.1]))]
    (indexed (if sort-by-y? (sort-by second pts) pts))))

(defmethod sites :lattice [{:keys [spacing extent] :or {extent [8.8 5.1]}}]
  (let [[w h] extent s (double spacing)]
    (indexed (for [j (range (- (long (/ h s 2))) (inc (long (/ h s 2))))
                   i (range (- (long (/ w s 2))) (inc (long (/ w s 2))))]
               [(* s i) (* s j)]))))

(defmethod sites :iso-grid
  ;; Isometric (A2) rows: alternate rows offset by half a cell, row pitch
  ;; s sqrt(3)/2, sorted back to front so lower blocks overpaint.
  [{:keys [spacing extent] :or {extent [8.8 5.1]}}]
  (let [[w h] extent s (double spacing) dy (* s 0.866025403784)]
    (indexed (sort-by (comp - second)
                      (for [j (range (- (long (/ h dy 2))) (inc (long (/ h dy 2))))
                            i (range (- (long (/ w s 2))) (inc (long (/ w s 2))))]
                        [(* s (+ i (* 0.5 (mod j 2)))) (* dy j)])))))

(defmethod sites :curve
  ;; n sites evenly in parameter along a closed curve given as
  ;; :curve (fn [t]) -> [x y] with t in [0, 1).
  [{:keys [curve n]}]
  (indexed (map #(curve (/ (double %) n)) (range n))))

(defmethod sites :ring [{:keys [n radius]}]
  (let [r (double radius)]
    (indexed (map #(let [a (* lp/tau (/ (double %) n))] [(* r (Math/cos a)) (* r (Math/sin a))]) (range n)))))

(defn spatial-bins
  "Coarse finite-support index over a 30 x 20 grid of half-unit cells on
   [-5, 10] x [-3, 7]: bin -> int[] of record ids whose support (radius)
   meets the cell. Packed records of `stride` doubles start with x, y."
  [^doubles data stride radius]
  (let [bins (object-array 600) stride (long stride) radius (double radius)]
    (dotimes [i (quot (alength data) stride)]
      (let [o (* i stride) x (aget data o) y (aget data (inc o))
            r0 (max 0 (long (Math/floor (* 2.0 (- (+ y 3.0) radius)))))
            r1 (min 20 (inc (long (Math/floor (* 2.0 (+ y 3.0 radius))))))
            c0 (max 0 (long (Math/floor (* 2.0 (- (+ x 5.0) radius)))))
            c1 (min 30 (inc (long (Math/floor (* 2.0 (+ x 5.0 radius))))))]
        (doseq [row (range r0 r1) col (range c0 c1)]
          (let [b (+ col (* row 30))] (aset bins b (conj (aget bins b) i))))))
    (dotimes [i (alength bins)] (aset bins i (int-array (aget bins i))))
    bins))

(defn bin-of
  "The spatial-bins cell index of screen point x, y (clamped)."
  ^long [^double x ^double y]
  (let [col (long (max 0.0 (min 29.0 (Math/floor (* 2.0 (+ x 5.0))))))
        row (long (max 0.0 (min 19.0 (Math/floor (* 2.0 (+ y 3.0))))))]
    (+ col (* row 30))))
