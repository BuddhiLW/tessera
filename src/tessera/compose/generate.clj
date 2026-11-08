(ns tessera.compose.generate
  "A seeded scene generator: (generate seed constraints) -> a valid scene.

   Every choice is drawn from closed menus whose members are seamless by
   construction: charts are stationary folds or self-similar / one-period
   advections; beats are integers; delays enter only as f(u - d) with
   integer-period f; serpent skins and sprite pulses advance by whole
   periods. So any generated scene is periodic in u, black-ground and
   free of white clip (all tones peak below one). Constraints narrow the
   menus: {:symmetry #{4 6 8 12} :chart #{:fold :tunnel :plain}
   :density [lo hi] :palette #{:uv :ember :ice} :serpents? bool}."
  (:require [tessera.compose.modulate :as m])
  (:import (java.util Random)))

(def palettes
  "Palette tiers (primary, secondary, accent, highlight)."
  {:uv {:primary [1.0 0.025 0.93] :secondary [0.02 0.88 1.0]
        :accent [0.40 1.0 0.02] :highlight [1.0 0.61 0.015]}
   :ember {:primary [1.0 0.10 0.015] :secondary [1.0 0.61 0.015]
           :accent [0.62 0.015 1.0] :highlight [0.02 0.88 1.0]}
   :ice {:primary [0.03 0.25 1.0] :secondary [0.02 0.88 1.0]
         :accent [1.0 0.025 0.93] :highlight [0.40 1.0 0.02]}})

(def ^:private glyphs
  [{:motif :stepped-cross} {:motif :chevron} {:motif :diamond :r 0.43}
   {:motif :star4 :r 0.40} {:motif :dashed-circle-ring :r 0.36 :amp 0.05 :lobes 8}])

(defn- pick [^Random rng xs] (nth (vec xs) (.nextInt rng (count xs))))
(defn- between ^double [^Random rng lo hi] (+ (double lo) (* (- (double hi) (double lo)) (.nextDouble rng))))

(def ^:private tau (* 2.0 Math/PI))

(defn- shuffle* [^Random rng xs]
  (let [a (java.util.ArrayList. ^java.util.Collection (vec xs))]
    (java.util.Collections/shuffle a rng)
    (vec a)))

(defn- chart-nodes
  "The chart: returns [nodes plane-slots]. Every chart is stationary or
   advects by an exact period."
  [rng chart symmetry]
  (case chart
    :fold [[{:type :polar} {:type :dihedral-fold :n symmetry}
            {:type :cartesian :in [:r :angle]}] [:px :py]]
    :tunnel [[{:type :polar} {:type :let :bind [[:rr [:max :r 1.0e-9]]]}
              {:type :tunnel-map :in [:rr :theta] :cols (* 2 symmetry) :shear (between rng 0.04 0.14)}
              {:type :lattice-cell :in [:s :t]}] [:a :b]]
    :plain [[{:type :let :bind [[:px :x] [:py :y] [:r [:hypot :x :y]]]}] [:px :py]]))

(defn- field-layers
  "Contour ink layers of a plane-wave field on the chart plane."
  [rng [sx sy] chart]
  (let [n (pick rng [5 7 9]) k (between rng 2.2 3.6) speed (pick rng [1 1 2])
        fade (if (= chart :tunnel) [:exp [:* -0.047 :z :z]] (m/fade-in :r 2.5 0.07))]
    {:nodes [{:type :plane-waves :n n :k k :scale 0.4 :dirs :half :speed speed :in [sx sy]}
             {:type :let :bind [[:fade fade] [:level [:- [:frac [:* 1.6 :field]] 0.5]]]}
             {:type :phase :bind [[:pulse (m/beat 0.7 0.3 (pick rng [1 2 3]))]]}]
     ;; Full-frame contours: background role, dimmed by background-gain.
     :layers [{:ink :primary :role :background
               :value [:* :fade 0.55 background-gain [:glow [:- :level 0.10] 0.035]]}
              {:ink :secondary :role :background
               :value [:* :fade 0.45 background-gain :pulse [:glow [:+ :level 0.10] 0.030]]}]}))

(def background-gain
  "Peak brightness of any full-frame (background) layer relative to the
   foreground: background layers are dimmed to this fraction."
  0.30)

(defn- tier-colors
  "Two palette tiers [outer inner] for background strokes: never a rainbow."
  [palette]
  [(:secondary palette) (:primary palette)])

(defn- sprite-layer
  "The ONE full-frame dense layer: a brightness-to-size glyph lattice
   driven by the field or graphene band, drawn in the palette's tiers and
   dimmed to `background-gain` so serpents and ornaments read over it."
  [rng at [lo hi] palette]
  (let [spacing (between rng 0.36 0.55)
        driver (pick rng [:graphene :waves])
        nodes (if (= driver :graphene)
                [{:type :graphene :a [(between rng 1.2 1.6) 0.55] :b [1.48 (- (between rng 1.4 1.8))] :out :drive}
                 {:type :let :bind [[:drive01 [:/ :drive 3.0]]]}]
                [{:type :plane-waves :n 5 :k (between rng 1.5 2.5) :scale 0.2 :in [:x :y] :out :drive}
                 {:type :let :bind [[:drive01 [:clamp01 [:+ 0.5 :drive]]]]}])
        kinds (vec (take (pick rng [2 3]) (shuffle* rng glyphs)))]
    {:paint :sprites :at at :support 0.45 :role :background
     :layout {:type :lattice :spacing spacing :extent [9.0 5.2]}
     :kinds (mapv (fn [g] {:kind :glyph :motif g :colors (tier-colors palette) :gain background-gain}) kinds)
     :nodes nodes
     :bind [[:radius [:+ 0.02 [:* (* spacing (between rng lo hi)) [:pow :drive01 1.2]]]]
            [:phase [:* tau [:+ (between rng 0.0 1.0) [:* 0.3 [:sin [:+ [:* 0.9 :x] [:* 0.4 :y]]]]]]]
            [:pulse [:+ 0.5 [:* 0.4 :drive01]]]]}))

(defn- ornament-layer
  "Droplet bouquets, eyes and stars with integer bobs and bleuje delays."
  [rng at]
  {:paint :sprites :at at :support 0.5 :role :foreground
   :layout {:type :poisson :seed (.nextInt ^Random rng 100000) :count (pick rng [18 24 30])
            :distance 0.9 :extent [8.6 4.6]}
   ;; The droplet bouquet is always present: its glossy highlight sets the
   ;; foreground peak the dimmed background is measured against.
   :kinds (into [{:kind :droplet-on-stem :bouquet [[0.18 -0.15 0.72 0.6] [-0.17 -0.11 0.62 -0.6]]}]
                (take (pick rng [0 1 2]) (shuffle* rng [{:kind :eye} {:kind :star4}])))
   :nodes [{:type :loop-noise :freq (between rng 0.3 0.7) :radius 0.0 :seed (.nextInt ^Random rng 1000) :out :delay}]
   :bind [[:phase [:* 2.399963229728653 :i]]
          [:radius [:+ 0.07 [:* 0.06 [:frac [:* :i 0.61803398875]]]]]
          [:bob [:* 0.035 [:sin [:+ [:* tau [:* [:+ [:mod :i 2] 1] :u]] :phase]]]]
          [:pulse [:+ 0.45 [:* 0.50 (m/delayed (pick rng [1 2]) :delay)]]]]})

(defn- serpent-layer [rng]
  {:paint :serpents :role :foreground :tone :uv :period (between rng 0.18 0.26)
   :ribbons (vec (for [_ (range (pick rng [2 3 4]))]
                   [(between rng -2.6 2.6) (between rng -1.6 1.6) (between rng -0.7 0.7)
                    (between rng 1.8 3.0) (pick rng [:stripes :scales :segments])]))})

(def defaults
  {:symmetry #{6 8 12} :chart #{:fold :tunnel :plain} :density [0.7 1.1]
   :palette #{:uv :ember :ice} :serpents? nil})

(defn generate
  "A valid, seamless scene from `seed` within `constraints` (see ns doc).
   Deterministic: the same seed and constraints give the same scene."
  ([seed] (generate seed {}))
  ([seed constraints]
   (let [rng (Random. (long seed))
         {:keys [symmetry chart density palette serpents?]} (merge defaults constraints)
         n (pick rng (sort symmetry)) chart (pick rng (sort chart))
         [nodes at] (chart-nodes rng chart n)
         field (field-layers rng at chart)
         serp? (if (nil? serpents?) (.nextBoolean rng) serpents?)
         pal (pick rng (sort palette))]
     {:id (keyword (str "generated-" seed))
      :title (str "Generated scene " seed ": " (name chart) " D" n " " (name pal))
      :seconds (pick rng [16 18 20]) :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
      :ground 0 :tone :neon
      :palette (palettes pal)
      :nodes (into nodes (:nodes field))
      :layers (cond-> (:layers field)
                ;; In the tunnel chart the glyph lattice lives in one cell
                ;; (it repeats with the cells); ornaments float on screen.
                true (conj (cond-> (sprite-layer rng at density (palettes pal))
                             (= chart :tunnel) (assoc-in [:layout :extent] [1.2 1.2])))
                serp? (conj (serpent-layer rng))
                true (conj (ornament-layer rng [:x :y])))})))

(defn- dense? [layer] (and (= :background (:role layer)) (:paint layer)))

(defn constraint-violations
  "The layer-role rules a generated scene must keep, as data (empty when
   it keeps them all):
     - every layer has a :role, :background or :foreground;
     - at most one full-frame DENSE (painted background) layer;
     - background glyphs are dimmed to <= background-gain and coloured from
       the palette tiers, never the rainbow;
     - background inks are palette tiers scaled by background-gain."
  [scene]
  (let [layers (:layers scene) tiers (set (vals (:palette scene)))]
    (cond-> []
      (some #(not (#{:background :foreground} (:role %))) layers)
      (conj :missing-role)
      (> (count (filter dense? layers)) 1)
      (conj :more-than-one-dense-background)
      (some (fn [l] (some #(or (nil? (:colors %)) (> (:gain % 1.0) background-gain)
                               (not-every? tiers (:colors %)))
                          (:kinds l)))
            (filter dense? layers))
      (conj :bright-or-rainbow-background-glyphs)
      (some (fn [l] (not (and (keyword? (:ink l)) (some #{background-gain} (flatten (:value l))))))
            (filter #(and (= :background (:role %)) (:ink %)) layers))
      (conj :undimmed-background-ink))))
