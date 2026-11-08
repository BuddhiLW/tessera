(ns tessera.compose.sprite
  "Painted sprites: shaded glyphs drawn source-over at layout sites.

   A sprite KIND (open multimethod `kind`) compiles, at phase u, to an
   ISprite that paints one glyph given the pixel offset from its site and
   its packed site record [x y radius kind phase pulse] (stride 6). The
   :sprites painter places records from a layout, bins them spatially and
   paints every glyph whose support meets the pixel, in record order."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [tessera.compose.core :as c]
            [tessera.compose.layout :as layout]
            [tessera.compose.motif :as m]
            [tessera.compose.render :as cr])
  (:import (tessera.compose.render IPaint)))

(set! *warn-on-reflection* true)

(definterface ISprite
  (^long paint [^double dx ^double dy ^long base ^doubles rec ^long o]))

(defmulti kind
  "Sprite kind spec {:kind k ...} -> (fn [u] ISprite). Open."
  :kind)

(def ^:private ^:const stride 6)

(defmethod kind :droplet-on-stem
  ;; A glossy rainbow droplet (red rim to blue core, radially) with a
  ;; specular highlight, on a thin wavy green stem.
  [_]
  (fn [_]
    (reify ISprite
      (paint [_ dx dy color rec o]
        (let [radius (aget rec (+ o 2)) pulse (aget rec (+ o 5))
              stem-d (max (- (Math/abs (+ dx (* 0.032 (Math/sin (* 9.0 dy))))) 0.006)
                          (- (Math/abs (+ dy 0.22)) 0.23))
              stem (cr/over color (r/rgb 0.08 0.81 0.26) (cr/cover stem-d))
              width (* radius (+ 0.93 (* -0.18 (max -1.0 (min 1.0 (/ dy radius))))))
              qx (/ dx width) qy (/ dy (* radius 1.12))
              q (Math/hypot qx qy) d (* radius (- q 1.0))
              z (Math/sqrt (max 0.0 (- 1.0 (* q q))))
              hue (* 0.666666666667 (max 0.0 (- 1.0 (min 1.0 q))))
              shade (* pulse (+ 0.48 (* 0.46 z)))
              highlight (Math/exp (* -85.0 (+ (Math/pow (+ qx 0.26) 2.0) (Math/pow (- qy 0.32) 2.0))))
              drop (cr/over (cr/rainbow hue shade) (r/rgb 0.94 0.94 0.94) highlight)]
          (cr/over stem drop (cr/cover d)))))))

(defmethod kind :eye
  ;; A sawtooth-lashed almond with a ringed ember iris, gold rim, black pupil.
  [_]
  (fn [_]
    (reify ISprite
      (paint [_ dx dy color rec o]
        (let [radius (aget rec (+ o 2)) pulse (aget rec (+ o 5))
              rr (Math/hypot dx dy)
              ex (/ dx (* 1.55 radius)) ey (/ dy (* 0.84 radius))
              angle (Math/atan2 ey ex)
              almond (+ (Math/abs ey) (* ex ex))
              saw (* 0.24 (max 0.0 (- 1.0 (* 2.0 (lp/frac (* 14.0 (/ angle lp/tau)))))))
              d (* radius (- almond (+ 1.0 saw)))
              iris (/ rr radius) ring (Math/pow (lp/wave (* 2.4 iris)) 0.45)
              pupil (cr/cover (* radius (- iris 0.22)))
              rim (c/glow (* radius (- almond 0.92)) 0.010)
              eye (r/rgb (* pulse 0.94) (* pulse (+ 0.045 (* 0.60 ring))) 0.008)
              eye (cr/over eye (r/rgb 0.94 0.71 0.025) (* 0.75 rim))
              eye (cr/over eye 0 pupil)]
          (cr/over color eye (cr/cover d)))))))

(defmethod kind :star4
  ;; A warm four-point astroid star with a glowing rim.
  [_]
  (fn [_]
    (reify ISprite
      (paint [_ dx dy color rec o]
        (let [radius (aget rec (+ o 2)) pulse (aget rec (+ o 5))
              xx (/ (Math/abs dx) radius) yy (/ (Math/abs dy) radius)
              d (* radius (- (+ (Math/sqrt xx) (Math/sqrt yy)) 1.0))
              rim (c/glow d 0.008)
              star (r/rgb (* pulse 0.94) (* pulse (+ 0.52 (* 0.32 rim))) 0.012)]
          (cr/over color star (min 1.0 (+ (cr/cover d) (* 0.24 rim)))))))))

(defmethod kind :bead-chain
  ;; A short wavy gold trail of beads marching one bead per loop.
  [{:keys [ease] :or {ease 0.72}}]
  (fn [u]
    (let [v (layout/travel-phase (double u) (double ease))]
      (reify ISprite
        (paint [_ dx dy color rec o]
          (let [phase (aget rec (+ o 4)) pulse (aget rec (+ o 5))
                path (- dy (* 0.065 (Math/sin (+ (* 15.0 dx) phase))))
                along (- (lp/frac (+ (* 17.0 dx) v)) 0.5)
                d (max (- (Math/hypot (* 0.058 along) path) 0.017) (- (Math/abs dx) 0.21))
                chain (r/rgb (* 0.94 pulse) (* 0.67 pulse) 0.015)]
            (cr/over color chain (cr/cover d))))))))

(defmethod kind :glyph
  ;; Any registry motif as a sprite: double outline (outer + inner), scaled
  ;; by the site radius, painted with alpha = light so glyphs overlap
  ;; cleanly. Colour: :colors [outer inner] (palette tiers, linear rgb) or,
  ;; without it, a rainbow hue set by the site phase. :gain (<= 1) bounds
  ;; the peak, so a background lattice stays dim under the foreground.
  [{:keys [motif hue-spread inner colors gain] :or {hue-spread 1.0 inner 0.045 gain 1.0}}]
  (let [sdf ^clojure.lang.IFn$DDD (:sdf (m/motif motif))
        spread (double hue-spread) inner (double inner) gain (double gain)
        [[r1 g1 b1] [r2 g2 b2]] (map #(map double %) colors)]
    (fn [_]
      (reify ISprite
        (paint [_ dx dy color rec o]
          (let [radius (aget rec (+ o 2)) phase (aget rec (+ o 4)) pulse (aget rec (+ o 5))
                d (* radius (.invokePrim sdf (/ dx radius) (/ dy radius)))]
            (if (> d 0.12) color
                (let [outline (* 1.3 (c/glow (- d 0.009) 0.017))
                      inline (* 0.72 (c/glow (+ d inner) 0.012))
                      v (* gain pulse)
                      mix (/ inline (max 1.0e-9 (+ outline inline)))
                      hue (lp/frac (* spread (/ phase lp/tau)))
                      outer-c (if colors (r/rgb (* v (double r1)) (* v (double g1)) (* v (double b1)))
                                  (cr/rainbow hue (* 0.94 v)))
                      inner-c (if colors (r/rgb (* v (double r2)) (* v (double g2)) (* v (double b2)))
                                  (cr/rainbow (+ hue 0.5) (* 0.90 v)))
                      a (min 1.0 (+ outline inline))]
                  (cr/over color (cr/over outer-c inner-c mix) a)))))))))

(defn site-records
  "Packed records [x y radius kind phase pulse] at phase u.
   The kind is :kind-of evaluated per site (default: cycle through `kinds`
   by site index); a kind's :bouquet adds extra heads [dx dy r-scale dphase]
   beside its site. Sites whose :visible is <= 0 are dropped."
  ^doubles [sites kinds {:keys [nodes bind]} u]
  (let [values ((layout/site-values nodes bind) u) nk (count kinds)
        keys* (set (map first bind))]
    (double-array
     (mapcat
      (fn [{:keys [i x y] :as site}]
        (let [v (values site)
              k (if (keys* :kind-of) (mod (long (v :kind-of)) nk) (mod (long i) nk))
              radius (v :radius) phase (v :phase) pulse (v :pulse)
              yy (+ (double y) (if (keys* :bob) (v :bob) 0.0))
              base [(double x) yy radius (double k) phase pulse]]
          (when (or (not (keys* :visible)) (pos? (v :visible)))
            (concat base
                    (mapcat (fn [[hx hy hr hp]]
                              [(+ (double x) (double hx)) (+ yy (double hy)) (* (double hr) radius)
                               (double k) (+ phase (double hp)) pulse])
                            (:bouquet (nth kinds k)))))))
      sites))))

(defmethod cr/painter :sprites
  ;; {:paint :sprites :layout {...sites} :kinds [{:kind k ..} ..]
  ;;  :bind [[:phase e] [:radius e] [:bob e] [:pulse e]] :support r}
  [{:keys [kinds support] :as spec} slot]
  (let [[ix iy] (cr/at-slots spec slot) ix (long ix) iy (long iy)
        sites (layout/sites (:layout spec))
        makers (mapv kind kinds)
        support (double (or support 0.5))]
    (fn [u]
      (let [^doubles rec (site-records sites kinds spec u)
            ^objects bins (layout/spatial-bins rec stride support)
            sprites (object-array (map #(% u) makers))]
        (reify IPaint
          (paint [_ c base]
            (let [x (aget c ix) y (aget c iy)
                  ^ints ids (aget bins (layout/bin-of x y))]
              (loop [j 0 color base]
                (if (= j (alength ids)) color
                    (let [o (* stride (aget ids j))
                          s (aget sprites (long (aget rec (+ o 3))))]
                      (recur (inc j)
                             (.paint ^ISprite s (- x (aget rec o)) (- y (aget rec (inc o)))
                                     color rec o))))))))))))
