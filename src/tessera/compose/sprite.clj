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

(defn- site-values
  "Evaluate per-site bindings [[slot expr] ..] over :i :u :x :y."
  [bind]
  (let [slot (c/allocator) ii (long (slot :i)) iu (long (slot :u))
        outs (mapv (comp slot first) bind)
        es (mapv #(c/expr (second %) slot) bind)
        n @slot]
    (fn [{:keys [i x y]} u]
      (let [ctx (double-array (max n 4))]
        (aset ctx 0 (double x)) (aset ctx 1 (double y))
        (aset ctx ii (double i)) (aset ctx iu (double u))
        (doseq [[o e] (map vector outs es)]
          (aset ctx (long o) (.at ^tessera.compose.core.IFactor e ctx)))
        (fn [k] (aget ctx (long (slot k))))))))

(defn site-records
  "Packed records [x y radius kind phase pulse] at phase u.
   kind cycles through `kinds` by site index; a kind's :bouquet adds extra
   heads [dx dy radius-scale dphase] beside its site."
  ^doubles [sites kinds bind u]
  (let [values (site-values bind) nk (count kinds)]
    (double-array
     (mapcat
      (fn [{:keys [i x y] :as site}]
        (let [v (values site u) k (mod (long i) nk)
              radius (v :radius) phase (v :phase) pulse (v :pulse)
              yy (+ (double y) (v :bob))
              base [(double x) yy radius (double k) phase pulse]]
          (concat base
                  (mapcat (fn [[hx hy hr hp]]
                            [(+ (double x) (double hx)) (+ yy (double hy)) (* (double hr) radius)
                             (double k) (+ phase (double hp)) pulse])
                          (:bouquet (nth kinds k))))))
      sites))))

(defmethod cr/painter :sprites
  ;; {:paint :sprites :layout {...sites} :kinds [{:kind k ..} ..]
  ;;  :bind [[:phase e] [:radius e] [:bob e] [:pulse e]] :support r}
  [{:keys [kinds bind support] :as spec} _slot]
  (let [sites (layout/sites (:layout spec))
        makers (mapv kind kinds)
        support (double (or support 0.5))]
    (fn [u]
      (let [^doubles rec (site-records sites kinds bind u)
            ^objects bins (layout/spatial-bins rec stride support)
            sprites (object-array (map #(% u) makers))]
        (reify IPaint
          (paint [_ c base]
            (let [x (aget c 0) y (aget c 1)
                  ^ints ids (aget bins (layout/bin-of x y))]
              (loop [j 0 color base]
                (if (= j (alength ids)) color
                    (let [o (* stride (aget ids j))
                          s (aget sprites (long (aget rec (+ o 3))))]
                      (recur (inc j)
                             (.paint ^ISprite s (- x (aget rec o)) (- y (aget rec (inc o)))
                                     color rec o))))))))))))
