(ns tessera.compose.motif
  "The motif vocabulary: an open registry of signed-distance glyphs.

   (motif {:motif k ...}) -> {:sdf (fn ^double [x y]) :style {...}}: the
   glyph's exact signed distance in its local unit cell (negative inside)
   and its default stroke style. Styles turn a distance into light:
     {:lines [[offset width weight] ..]}  sum weight * glow(d - offset, width)
     {:blob k}                            exp(-k d^2), for point glyphs
   Motifs are geometric constructions, not copies of ceremonial designs.
   Painted (source-over) sprites with shading are `painter` methods in
   tessera.compose.sprite."
  (:require [desargues.wallpaper.loop :as lp]
            [tessera.compose.core :as c]
            [tessera.compose.render :as cr])
  (:import (tessera.compose.core IStep)))

(set! *warn-on-reflection* true)

(defn stepped-cross
  "Signed L-infinity distance to a three-step cross at the origin: the union
   of three axis-aligned boxes, twelve re-entrant corners."
  ^double [^double x ^double y]
  (let [a (Math/abs x) b (Math/abs y)]
    (min (max (- a 0.39) (- b 0.12))
         (max (- a 0.27) (- b 0.27))
         (max (- a 0.12) (- b 0.39)))))

(defn diamond
  "L1 ball |x| + |y| - r, scaled to a true Euclidean distance when exact?."
  ^double [^double x ^double y ^double r]
  (- (+ (Math/abs x) (Math/abs y)) r))

(defn chevron
  "A V of half-width .39 and arm thickness .12, opening upward."
  ^double [^double x ^double y]
  (let [ax (Math/abs x)]
    (max (- ax 0.39) (- (Math/abs (- (Math/abs y) (* 0.68 ax))) 0.12))))

(defn meander
  "Banded distance to a square-spiral (Greek key) hatch of frequency f:
   the L1 level sets of the cell, folded to [-1/2, 1/2)."
  ^double [^double x ^double y ^double f]
  (- (lp/frac (+ (* f x) (* f y))) 0.5))

(defn almond
  "Eye outline: |y| + a |x| - r (an almond / lozenge), aspect a."
  ^double [^double x ^double y ^double a ^double r]
  (- (+ (Math/abs x) (* a (Math/abs y))) r))

(defn star4
  "Four-point astroid star sqrt|x| + sqrt|y| = 1, scaled by r."
  ^double [^double x ^double y ^double r]
  (* r (- (+ (Math/sqrt (/ (Math/abs x) r)) (Math/sqrt (/ (Math/abs y) r))) 1.0)))

(defn ring
  "Distance to a circle of radius r, optionally petalled: r + a cos(n theta)."
  ^double [^double x ^double y ^double r [amp lobes]]
  (- (Math/hypot x y) (+ r (* (double amp) (Math/cos (* (double lobes) (Math/atan2 y x)))))))

(defn iso-diamond
  "Top face of an isometric prism of half-size s: the 30-degree rhombus
   max(|a|, |b|) - s with a, b the isometric axes. Returns [d a b]."
  [^double x ^double y ^double s]
  (let [a (+ (* 0.57735026919 x) y) b (+ (* -0.57735026919 x) y)]
    [(* 0.5 (- (max (Math/abs a) (Math/abs b)) s)) a b]))

(defmulti motif
  "Motif spec {:motif k ...params} -> {:sdf (fn ^double [x y]) :style s}.
   Open: register further glyphs with defmethod."
  :motif)

(defmethod motif :stepped-cross [_]
  {:sdf stepped-cross :style {:lines [[0.014 0.011 1.0]]}})

(defmethod motif :diamond [{:keys [r] :or {r 0.125}}]
  (let [r (double r)]
    {:sdf (fn ^double [^double x ^double y] (diamond x y r)) :style {:lines [[0.0 0.010 1.0]]}}))

(defmethod motif :chevron [_]
  {:sdf chevron :style {:lines [[0.0 0.009 1.0]]}})

(defmethod motif :meander [{:keys [freq] :or {freq 3.0}}]
  (let [f (double freq)]
    {:sdf (fn ^double [^double x ^double y] (meander x y f)) :style {:lines [[0.0 0.080 1.0]]}}))

(defmethod motif :eye [{:keys [aspect r] :or {aspect 1.7 r 0.10}}]
  (let [a (double aspect) r (double r)]
    {:sdf (fn ^double [^double x ^double y] (almond x y a r)) :style {:lines [[0.0 0.016 0.5]]}}))

(defmethod motif :dot [_]
  {:sdf (fn ^double [^double x ^double y] (Math/hypot x y)) :style {:blob 850.0}})

(defmethod motif :star4 [{:keys [r] :or {r 0.3}}]
  (let [r (double r)]
    {:sdf (fn ^double [^double x ^double y] (star4 x y r)) :style {:lines [[0.0 0.008 1.0]]}}))

(defmethod motif :dashed-circle-ring [{:keys [r amp lobes] :or {r 0.3 amp 0.0 lobes 0}}]
  (let [r (double r) petals [(double amp) (double lobes)]]
    {:sdf (fn ^double [^double x ^double y] (ring x y r petals)) :style {:lines [[0.0 0.008 1.0]]}}))

(defmethod motif :isometric-pyramid [{:keys [s] :or {s 0.39}}]
  (let [s (double s)]
    {:sdf (fn ^double [^double x ^double y] (double (first (iso-diamond x y s))))
     :style {:lines [[0.0 0.008 1.0]]}}))

(defn styled
  "Light from a distance under `style` (see ns doc)."
  ^double [style ^double d]
  (if-let [k (:blob style)]
    (Math/exp (* (- (double k)) (* d d)))
    (reduce (fn [^double acc [off w weight]]
              (+ acc (* (double weight) (c/glow (- d (double off)) (double w)))))
            0.0 (:lines style))))

(defn- line-light
  "Sum weight * glow(d - offset, width) over packed [off w weight] triples."
  ^double [^doubles lines ^double d]
  (let [n (quot (alength lines) 3)]
    (loop [j 0 acc 0.0]
      (if (= j n) acc
          (let [o (* 3 j)
                g (c/glow (- d (aget lines o)) (aget lines (inc o)))]
            (recur (inc j) (+ acc (* (aget lines (+ o 2)) g))))))))

(defmethod c/node :stroke
  ;; {:type :stroke :motif {...} :at [sx sy] :out k :style s?}: the motif's
  ;; styled light at the chart point (slot sx, slot sy), into slot k.
  [{:keys [at out style] :as spec} slot]
  (let [{:keys [sdf] :as m} (motif (:motif spec))
        style (or style (:style m))
        [sx sy] at ix (long (slot sx)) iy (long (slot sy)) io (long (slot out))
        ^clojure.lang.IFn$DDD sdf sdf
        blob (double (or (:blob style) 0.0))
        lines (double-array (mapcat identity (:lines style)))]
    (fn [_]
      (reify IStep
        (run [_ c]
          (let [d (.invokePrim sdf (aget c ix) (aget c iy))
                v (if (pos? blob) (Math/exp (* (- blob) (* d d))) (line-light lines d))]
            (aset c io v)))))))

(defmethod c/node :blob
  ;; A Gaussian point glyph exp(-k (a^2 + b^2)) at cell coordinates.
  [{:keys [at out k]} slot]
  (let [[sa sb] at ia (long (slot sa)) ib (long (slot sb)) io (long (slot out)) k (- (double k))]
    (fn [_]
      (reify IStep
        (run [_ c]
          (let [a (aget c ia) b (aget c ib)]
            (aset c io (Math/exp (* k (+ (* a a) (* b b)))))))))))

(defmethod c/node :bead-chain
  ;; Beads strung along a line family: glow(across - offset, width) times a
  ;; Gaussian bead envelope exp(-k bx^2), bx = frac(freq along) - 1/2.
  [{:keys [along across out freq offset width k]} slot]
  (let [ia (long (slot along)) ic (long (slot across)) io (long (slot out))
        f (double freq) off (double offset) w (double width) k (- (double k))]
    (fn [_]
      (reify IStep
        (run [_ c]
          (let [bx (- (lp/frac (* f (aget c ia))) 0.5)]
            (aset c io (* (c/glow (- (aget c ic) off) w) (Math/exp (* k bx bx))))))))))

(defmethod c/node :radial-ring
  ;; Petalled circle at polar slots: glow(r - (radius + amp cos(lobes t))).
  [{:keys [r theta out radius amp lobes width]} slot]
  (let [ir (long (slot r)) it (long (slot theta)) io (long (slot out))
        rad (double radius) amp (double amp) lobes (double lobes) w (double width)]
    (fn [_]
      (reify IStep
        (run [_ c]
          (let [target (+ rad (* amp (Math/cos (* lobes (aget c it)))))]
            (aset c io (c/glow (- (aget c ir) target) w))))))))
