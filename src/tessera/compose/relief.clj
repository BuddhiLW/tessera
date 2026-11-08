(ns tessera.compose.relief
  "Painted relief: isometric stepped pyramids and serpent ribbons.

   :iso-pyramids  stacked isometric prisms (tiers) at layout sites; each
                  tier a dark violet west face, darker blue east face and a
                  bright top carrying a motif in double outline + hatch.
   :serpents      finite ribbons along two-harmonic curves, skins
                  :stripes :scales :segments, the skin advected one texture
                  period per loop along the integrated arclength."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [tessera.compose.core :as c]
            [tessera.compose.layout :as layout]
            [tessera.compose.motif :as motif]
            [tessera.compose.render :as cr])
  (:import (tessera.compose.render IPaint)))

(set! *warn-on-reflection* true)

(defn- tier-colors
  "The face and top colours of one tier at light level `light`."
  [tone light outline inline hatch border flutes west?]
  (let [light (double light) outline (double outline) inline (double inline)
        hatch (double hatch) border (double border) flutes (double flutes)]
    [(if west?
       (r/rgb (* light (+ 0.16 (* 0.14 flutes))) 0.012 (* light (+ 0.42 (* 0.16 flutes))))
       (r/rgb 0.008 (* light (+ 0.035 (* 0.10 flutes))) (* light (+ 0.16 (* 0.18 flutes)))))
     (.invokePrim ^clojure.lang.IFn$DDDL tone
                  (+ 0.07 (* 0.55 inline) (* 0.018 outline))
                  (* light (+ 0.10 (* 0.92 outline) (* 0.20 hatch) (* 0.50 border)))
                  (* light (+ 0.40 outline (* 0.78 inline) (* 0.44 hatch) (* 0.74 border))))]))

(defmethod cr/painter :iso-pyramids
  ;; {:paint :iso-pyramids :layout {...} :tiers 3 :motif {...} :scale expr
  ;;  :tone t :support r}. Site scale from expr over :i; light breathes
  ;; once per loop with a per-site phase (integer frequency).
  [{:keys [tiers scale support tone] :or {tiers 3 support 1.05 tone :uv} :as spec} _slot]
  (let [sites (layout/sites (:layout spec))
        slot (c/allocator) ii (long (slot :i))
        scale-e (c/expr scale slot) n @slot
        sdf ^clojure.lang.IFn$DDD (:sdf (motif/motif (or (:motif spec) {:motif :stepped-cross})))
        data (double-array (mapcat (fn [{:keys [i x y]}]
                                     (let [ctx (double-array (max n 3))]
                                       (aset ctx ii (double i))
                                       [(double x) (double y) (.at ^tessera.compose.core.IFactor scale-e ctx)]))
                                   sites))
        ^objects bins (layout/spatial-bins data 3 support)
        tone (cr/->tone tone) tiers (long tiers)]
    (fn [u]
      (let [phase (* lp/tau (double u))]
        (reify IPaint
          (paint [_ ctx color]
            (let [x (aget ctx 0) y (aget ctx 1)
                  ^ints ids (aget bins (layout/bin-of x y))]
              (loop [j 0 color (long color)]
                (if (= j (alength ids)) color
                    (let [i (aget ids j) o (* 3 i) sc (aget data (+ o 2))
                          dx (/ (- x (aget data o)) sc) dy (/ (- y (aget data (inc o))) sc)
                          light (+ 0.77 (* 0.20 (Math/sin (+ phase (* 1.7 i)))))]
                      (recur (inc j)
                             (loop [tier 0 c (long color)]
                               (if (= tier tiers) c
                                   (let [s (- 0.39 (* tier 0.105)) elevation (+ -0.12 (* tier 0.16))
                                         yy (- dy elevation) a (+ (* 0.57735026919 dx) yy)
                                         b (+ (* -0.57735026919 dx) yy)
                                         top (* 0.5 (- (max (Math/abs a) (Math/abs b)) s))
                                         lower (+ (- s) (* 0.57735026919 (Math/abs dx)))
                                         side (max (* 0.5 (- (Math/abs dx) (* 1.73205080757 s)))
                                                   (- (Math/abs (+ (- yy lower) 0.080)) 0.080))
                                         raw (.invokePrim sdf (/ (* 0.37 a) s) (/ (* 0.37 b) s))
                                         outline (c/glow raw 0.014)
                                         inline (c/glow (+ raw 0.060) 0.010)
                                         hatch (if (< raw -0.075)
                                                 (c/glow (- (lp/frac (* 5.0 (+ (Math/abs (/ a s)) (Math/abs (/ b s))))) 0.5) 0.060) 0.0)
                                         border (c/glow top 0.008)
                                         flutes (c/glow (- (lp/frac (+ (* 14.0 dx) (* 8.0 yy))) 0.5) 0.10)
                                         [side-color top-color] (tier-colors tone light outline inline hatch border flutes (neg? dx))
                                         c (cr/over c (long side-color) (cr/cover side))]
                                     (recur (inc tier) (cr/over c (long top-color) (cr/cover top)))))))))))))))))

(def skins
  "Closed set of serpent skins, by index in the packed table."
  [:stripes :scales :segments])

(defn ribbon-table
  "Tabulate one serpent: centre cx cy, direction angle, half-length, skin,
   and index k (its harmonic phase). y(x) has two integer harmonics of the
   ribbon's own length; stores normal factors and integrated arclength."
  [k [cx cy angle length skin]]
  (let [n 1537 ys (double-array n) normals (double-array n) arcs (double-array n)
        length (double length) phase (* 1.21 k) omega (/ lp/tau (* 2.0 length))]
    (dotimes [j n]
      (let [x (* length (- (* 2.0 (/ j (double (dec n)))) 1.0))
            t (+ (* omega x) phase)
            y (+ (* 0.40 (Math/sin (* 2.0 t))) (* 0.18 (Math/sin (+ (* 3.0 t) k))))
            slope (* omega (+ (* 0.80 (Math/cos (* 2.0 t))) (* 0.54 (Math/cos (+ (* 3.0 t) k)))))
            normal (/ 1.0 (Math/sqrt (+ 1.0 (* slope slope))))]
        (aset ys j y) (aset normals j normal)
        (when (pos? j)
          (aset arcs j (+ (aget arcs (dec j)) (/ (* 2.0 length) (* (dec n) normal)))))))
    [(double cx) (double cy) (Math/cos angle) (Math/sin angle)
     length (long (.indexOf ^java.util.List skins skin)) ys normals arcs]))

(defn- skin-core
  "Shaded skin colour of a serpent at arclength phase s, across in [-1,1]."
  ^long [tone ^long skin ^double s [^double across ^double d]]
  (let [shade (+ 0.28 (* 0.70 (Math/sqrt (max 0.0 (- 1.0 (* across across))))))
        stripe (cr/cover (* 0.10 (Math/sin (* lp/tau s))))
        scale (c/glow (- (lp/frac (+ s (* 0.70 (Math/abs across)))) 0.5) 0.13)
        rim (c/glow d 0.007)
        core (case skin
               0 (r/rgb (* shade (+ 0.012 (* 0.88 stripe)))
                        (* shade (+ 0.015 (* 0.90 stripe)))
                        (* shade (+ 0.028 (* 0.91 stripe))))
               1 (.invokePrim ^clojure.lang.IFn$DDDL tone (* 0.018 scale) (* shade (+ 0.08 (* 0.87 scale)))
                              (* shade (+ 0.21 (* 0.80 scale))))
               2 (r/rgb (* shade 0.94) (* shade (+ 0.06 (* 0.29 (Math/pow (lp/wave s) 4.0)))) 0.018))]
    (if (= skin 2) core (cr/over core (r/rgb 0.06 0.67 0.94) (* 0.65 rim)))))

(defmethod cr/painter :serpents
  ;; {:paint :serpents :ribbons [[cx cy angle half-length skin] ..]
  ;;  :period p :ease e :tone t}: the skin advances one texture period
  ;; (arclength p) per loop; :segments skins are bead-segmented bodies.
  [{:keys [ribbons period ease tone] :or {period 0.20 ease 0.72 tone :uv}} _slot]
  (let [tables (vec (map-indexed ribbon-table ribbons))
        period (double period) tone (cr/->tone tone)]
    (fn [u]
      (let [v (layout/travel-phase (double u) (double ease))]
        (reify IPaint
          (paint [_ ctx base]
            (let [x (aget ctx 0) y (aget ctx 1)]
              (loop [k 0 color base]
                (if (= k (count tables)) color
                    (let [[cx cy ca sa length skin ^doubles ys ^doubles normals ^doubles arcs] (nth tables k)
                          dx (- x (double cx)) dy (- y (double cy))
                          xx (+ (* dx (double ca)) (* dy (double sa)))
                          yy (- (* dy (double ca)) (* dx (double sa)))
                          length (double length) skin (long skin)]
                      (if (> (Math/abs xx) length) (recur (inc k) color)
                          (let [index (long (* 1536.0 (/ (+ xx length) (* 2.0 length))))
                                normal (* (- yy (aget ys index)) (aget normals index))
                                taper (Math/pow (max 0.0 (- 1.0 (Math/pow (/ xx length) 2.0))) 0.55)
                                radius (* (+ 0.085 (* 0.055 (lp/wave (+ (* 0.45 xx) (* k 0.17))))) taper)
                                d (- (Math/abs normal) radius)]
                            (if (or (< radius 0.001) (> d 0.045)) (recur (inc k) color)
                                (let [s (- (/ (aget arcs index) period) v) across (/ normal radius)
                                      bead (Math/sqrt (+ (Math/pow (* 1.5 (- (lp/frac s) 0.5)) 2.0) (* across across)))
                                      core (skin-core tone skin s [across d])
                                      d (if (= skin 2) (max d (* radius (- bead 0.97))) d)]
                                  (recur (inc k) (cr/over color core (cr/cover d)))))))))))))))))
