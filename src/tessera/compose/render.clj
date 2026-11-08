(ns tessera.compose.render
  "Compositor primitives for composed scenes: stroke profiles, coverage,
   bounded blends, hue-preserving tone maps and the layer stack.

   A compiled layer is a map {:blend k :f f}. :add layers add linear ink
   into a double[3] accumulator (f x y acc); :over layers paint a packed
   0xRRGGBB colour (f x y base) -> colour. The stack flushes the ink
   accumulator through the scene tone map, then screens it over the base,
   whenever a painted layer follows ink, and once at the end. No step adds
   white: every tone map peaks below one."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [tessera.compose.core :as c])
  (:import (tessera.compose.core IStep IFactor)))

(set! *warn-on-reflection* true)

(defn cover
  "Antialiased coverage of a signed distance, in scene units."
  ^double [^double d]
  (max 0.0 (min 1.0 (- 0.5 (* 160.0 d)))))

(defn channel ^long [^long c ^long shift] (bit-and 255 (bit-shift-right c shift)))

(defn over
  "Bounded source-over of packed colours; never additive white."
  ^long [^long background ^long foreground ^double alpha]
  (let [a (max 0.0 (min 1.0 alpha)) b (- 1.0 a)]
    (bit-or
     (bit-shift-left (long (+ (* b (channel background 16)) (* a (channel foreground 16)))) 16)
     (bit-shift-left (long (+ (* b (channel background 8)) (* a (channel foreground 8)))) 8)
     (long (+ (* b (channel background 0)) (* a (channel foreground 0)))))))

(defn screen
  "Screen blend 1-(1-a)(1-b) of packed colours, exact when either is black."
  ^long [^long a ^long b]
  (cond (zero? a) b (zero? b) a
        :else (let [s (fn ^long [^long k]
                        (let [x (channel a k) y (channel b k)]
                          (- (+ x y) (quot (* x y) 255))))]
                (bit-or (bit-shift-left (s 16) 16) (bit-shift-left (s 8) 8) (s 0)))))

(defn add-packed
  "Saturating additive blend of packed colours."
  ^long [^long a ^long b]
  (let [s (fn ^long [^long k] (min 255 (+ (channel a k) (channel b k))))]
    (bit-or (bit-shift-left (s 16) 16) (bit-shift-left (s 8) 8) (s 0))))

(defn rainbow
  "Full HSV rainbow, red at zero through green to blue at 2/3."
  ^long [^double hue ^double value]
  (let [t (* 6.0 (lp/frac hue))
        ch (fn ^double [^double offset]
             (* value (max 0.0 (min 1.0 (- (Math/abs (- (mod (+ t offset) 6.0) 3.0)) 1.0)))))]
    (r/rgb (ch 0.0) (ch 4.0) (ch 2.0))))

(defmulti tone-map
  "Tone map spec {:type k ...} -> (fn ^long [r g b]) from linear ink to a
   packed colour. Open: register further operators with defmethod."
  :type)

(defmethod tone-map :neon
  [{:keys [gain peak saturation floor cutoff]
    :or {gain 1.65 peak 0.95 saturation 0.98 floor 0.02 cutoff 0.003}}]
  ;; Hue-preserving bounded exposure: max channel peak*(1-e^{-gain hi}),
  ;; HSV saturation >= `saturation`, so no channel ever clips to white.
  (let [gain (double gain) peak (double peak) sat (double saturation)
        pedestal (double floor) cutoff (double cutoff)]
    (fn ^long [^double red ^double green ^double blue]
      (let [hi (max red green blue) lo (min red green blue) span (- hi lo)]
        (if (or (< hi cutoff) (< span 1.0e-12))
          0
          (let [v (* peak (- 1.0 (Math/exp (* (- gain) hi))))
                k (/ (* sat v) span) floor (* pedestal v)]
            (r/rgb (+ floor (* k (- red lo)))
                   (+ floor (* k (- green lo)))
                   (+ floor (* k (- blue lo))))))))))

(defmethod tone-map :clamp [_]
  (fn ^long [^double red ^double green ^double blue]
    (r/rgb (min 0.95 red) (min 0.95 green) (min 0.95 blue))))

(def tones
  "Named tone maps: the vj neon exposure and the vj2 uv exposure."
  {:neon {:type :neon}
   :uv {:type :neon :gain 1.8 :peak 0.94 :saturation 0.99 :floor 0.01}})

(defn ->tone [t] (tone-map (if (keyword? t) (tones t) t)))

(definterface IPaint
  (^long paint [^doubles c ^long base]))

(defn- flush-ink
  "Tone-map the accumulator and combine it with `base` by `blend`."
  ^long [tone blend ^long base ^doubles acc]
  (let [c (long (.invokePrim ^clojure.lang.IFn$DDDL tone (aget acc 0) (aget acc 1) (aget acc 2)))]
    (case blend
      :add (add-packed base c)
      :over (if (zero? c) base c)
      (screen base c))))

(defmulti painter
  "Painted layer spec {:paint k ...} -> (fn [u] IPaint), compiled with the
   scene's slot allocator. Open: sprite and ribbon painters extend it."
  (fn [spec _slot] (:paint spec)))

(defn at-slots
  "The slot indices a painter reads its plane point from: the layer's
   :at [sx sy] chart slots (a fold, a lens, a tunnel), default screen x y.
   This is how any painter is placed in any layout chart."
  [spec slot]
  (let [[sx sy] (:at spec [:x :y])]
    [(long (slot sx)) (long (slot sy))]))

(defn- ink-of
  "A layer's ink: [r g b] linear weights, or a palette key."
  [palette ink]
  (let [v (if (keyword? ink) (get palette ink) ink)]
    (when-not (and (vector? v) (= 3 (count v)))
      (throw (ex-info "Ink must be [r g b] or a palette key" {:ink ink})))
    (mapv double v)))

(defn- layer-value
  "The layer's scalar: explicit :value, else (* modulate.. motif-light)."
  [{:keys [value modulate] :as layer} raw]
  (or value (if (seq modulate) (into [:*] (concat modulate [raw])) raw)))

(defn- compile-ink-group
  "Consecutive ink layers: acc_ch = sum_i value_i ink_i,ch (left fold, in
   layer order), tone-mapped, then blended onto the base by :blend."
  [layers palette tone slot]
  (let [n (count layers)
        es (object-array (map #(c/expr (:expr %) slot) layers))
        inks (double-array (mapcat #(ink-of palette (:ink %)) layers))
        blend (:blend (first layers) :screen)
        tone (->tone tone)]
    (fn [_u]
      (fn ^long [^doubles c ^long base]
        (loop [i 0 r 0.0 g 0.0 b 0.0]
          (if (= i n)
            (flush-ink tone blend base (doto (double-array 3) (aset 0 r) (aset 1 g) (aset 2 b)))
            (let [v (.at ^IFactor (aget es i) c) o (* 3 i)]
              (recur (inc i)
                     (+ r (* v (aget inks o)))
                     (+ g (* v (aget inks (inc o))))
                     (+ b (* v (aget inks (+ o 2))))))))))))

(defn- prepare-layer
  "Normalise one layer: its nodes and its value. An ink layer's :layout is
   chart nodes and its motif a stroke node; a paint layer's :layout is its
   own site spec, and only its :nodes join the graph."
  [i {:keys [paint layout motif at style nodes] :as layer}]
  (if paint
    (assoc layer :nodes (vec nodes))
    (let [raw (keyword (str "layer-" i))
          stroke (when motif [{:type :stroke :motif motif :at at :out raw :style style}])]
      (assoc layer
             :nodes (vec (concat layout nodes stroke))
             :expr (layer-value layer (if motif raw (:raw layer)))))))

(defn compile-scene
  "Compile a scene map (see tessera.compose.scene/Scene) to the field
   (fn [u] -> ^long [x y]) tessera.loops expects.

   :nodes run first per pixel (shared charts and fields), then each layer's
   own :layout nodes and motif stroke. Ink layers (:ink) gather into groups
   that are tone-mapped together; a :paint layer is a painter over the
   packed colour so far. :ground is the base colour."
  [{:keys [ground palette tone nodes layers] :or {ground 0 tone :neon}}]
  (let [slot (c/allocator)
        iu (long (slot :u))
        prepared (map-indexed prepare-layer layers)
        graph (c/chain (distinct (concat nodes (mapcat :nodes prepared))) slot)
        groups (partition-by #(if (:paint %) (gensym) (:group % :ink)) prepared)
        compiled (mapv (fn [g]
                         (if (:paint (first g))
                           (let [p (painter (first g) slot)]
                             (fn [u] (let [^tessera.compose.render.IPaint q (p u)]
                                       (fn ^long [^doubles c ^long base]
                                         (.paint q c base)))))
                           (compile-ink-group g palette (:tone (first g) tone) slot)))
                       groups)
        width @slot
        ground (long ground)]
    (fn [u]
      (let [^IStep step (graph u)
            fs (object-array (map #(% u) compiled))]
        (fn ^long [^double x ^double y]
          (let [c (double-array width)]
            (aset c 0 x) (aset c 1 y) (aset c iu u)
            (.run step c)
            (loop [i 0 col ground]
              (if (= i (alength fs)) col
                  (recur (inc i) (long (.invokePrim ^clojure.lang.IFn$OLL (aget fs i) c col)))))))))))
