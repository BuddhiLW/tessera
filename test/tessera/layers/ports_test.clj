(ns tessera.layers.ports-test
  "The second wave of field ports: slot ABI, emitters, and pixel parity
   against the original closure loops, with the metric and tolerance of
   tessera.layers-test (mean channel error < 1 level, max <= 1 level, at
   u = 0, .3, .7, 96x54 ss1 and 192x108 ss2)."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.emit :as emit]
            [desargues.wallpaper.field :as field]
            [desargues.wallpaper.pfx :as pfx]
            [desargues.wallpaper.render :as render]
            [desargues.wallpaper.wgsl :as wgsl]
            [hive-dsl.result :as result]
            [tessera.layers :as layers]
            [tessera.layers.ports :as ports]
            [tessera.loops :as loops])
  (:import (java.awt.image BufferedImage)))

(def phases [0.0 0.3 0.7])

(def expected-param-order
  ;; Binding ABI: loop_rate is implicit slot 0. Only append new params.
  {:twisted_bilayer '[wave_number twist pulse_spacing]
   :double_slit '[sharpness bar_length]
   :quantum_tapestry '[time_span knee]
   :quantum_carpet '[time_span knee]
   :box_modes '[tone_k contour_density]
   :drum_modes '[tile_scale node_sharpness]
   :breathers '[level_density line_sharpness]
   :fold_9 '[fermi_level contour_step sharpness lattice_scale offset_x offset_y]
   :fold_10 '[fermi_level contour_step sharpness lattice_scale offset_x offset_y]
   :fold_11 '[fermi_level contour_step sharpness lattice_scale offset_x offset_y]
   :fold_12 '[fermi_level contour_step sharpness lattice_scale offset_x offset_y]
   :brillouin_square '[edge_width pulse_zones]
   :brillouin_oblique '[edge_width pulse_zones]
   :brillouin_rect '[edge_width pulse_zones]})

(def expected-effect-names
  ;; Catalog titles are fosfora preset keys: never rename.
  {:twisted_bilayer "Twisted Bilayer" :double_slit "Double Slit"
   :quantum_tapestry "Quantum Tapestry" :quantum_carpet "Quantum Carpet"
   :box_modes "Box Modes" :drum_modes "Drum Modes" :breathers "Sine-Gordon Breathers"
   :fold_9 "Nine-Fold Contours" :fold_10 "Ten-Fold Contours"
   :fold_11 "Eleven-Fold Contours" :fold_12 "Twelve-Fold Contours"
   :brillouin_square "Brillouin Square" :brillouin_oblique "Brillouin Oblique"
   :brillouin_rect "Brillouin Rectangular"})

(defn- raster [shader view [w h] ss]
  (let [img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
    (render/field-raster :jvm img shader {:view view :ss ss})
    img))

(defn- channel-diff
  "Mean absolute channel error and worst channel error, in 8-bit levels."
  [^BufferedImage a ^BufferedImage b]
  (let [diffs (for [y (range (.getHeight a)) x (range (.getWidth a)) shift [16 8 0]]
                (Math/abs (long (- (bit-and 255 (bit-shift-right (.getRGB a x y) shift))
                                  (bit-and 255 (bit-shift-right (.getRGB b x y) shift))))))]
    {:mean (/ (double (reduce + diffs)) (count diffs)) :max (reduce max diffs)}))

(defn reference-field
  "The original closure shader of the loop layer `l` reproduces, at u."
  [l u]
  ((:field (loops/all (ports/source-loops (:id l)))) u))

(defn parity
  ([l u] (parity l u [96 54] 1))
  ([l u size ss]
   (channel-diff (raster (layers/bind l u) (:view l) size ss)
                 (raster (reference-field l u) (:view l) size ss))))

(defn parity-report
  "{id [{:u :mean :max}]} at 96x54 ss1, for a REPL look."
  []
  (into (sorted-map)
        (for [l ports/layers]
          [(:id l) (mapv #(assoc (parity l %) :u %) phases)])))

(deftest registered-after-the-own-layers
  ;; Ports are the registry's tail; tessera's own layers come first, the
  ;; original ten in their published order (new own layers append after them).
  (let [ids (mapv :id layers/layers)
        n-own (- (count ids) (count ports/layers))]
    (is (= (mapv :id ports/layers) (subvec ids n-own)))
    (is (= [:graphene_fermi :kagome_fermi :square_fermi :penrose_fermi :heptagonal_fermi
            :octagonal_fermi :quasicrystal :lieb_fermi :brillouin_zones :kaleido_quasicrystal]
           (subvec ids 0 10)))
    (is (<= 10 n-own))
    (is (apply distinct? ids))
    (is (= (set (keys expected-param-order)) (set (map :id ports/layers))))
    (is (= (set (keys ports/source-loops)) (set (map :id ports/layers))))
    (is (every? loops/all (vals ports/source-loops)))))

(deftest published-parameter-order
  (doseq [l ports/layers]
    (testing (name (:id l))
      (is (vector? (:params l)))
      (is (= (expected-param-order (:id l)) (mapv first (:params l))))
      (is (= (into ['u] (expected-param-order (:id l))) (get-in l [:form :params])))
      (is (re-matches #"[a-z0-9]+(?:_[a-z0-9]+)*" (name (:id l))))
      (doseq [[p {:keys [default min max label]}] (:params l)]
        (is (re-matches #"[a-z][a-z0-9_]*" (name p)))
        (is (string? label))
        (is (<= min default max))))))

(deftest every-loop-is-ported-or-explained
  (let [ported (into (set (vals ports/source-loops))
                     (map #(keyword (str/replace (name %) "_" "-")))
                     (keys layers/effects))
        explained (set (map #(keyword (str/replace (name %) "_" "-")) (keys layers/skipped)))]
    (doseq [id (keys loops/all)]
      (testing (name id)
        (is (or (ported id) (explained id)))))
    (testing "a ported loop is not also listed as skipped"
      (is (empty? (filter explained ported))))))

(deftest every-form-compiles-on-jvm-glsl-and-wgsl
  (doseq [l ports/layers]
    (testing (name (:id l))
      (is (fn? (field/jvm-shader (layers/bind l 0.0))))
      (is (result/ok? (field/source :glsl (:form l) {})))
      (is (result/ok? (wgsl/ir (:form l))))
      (is (result/ok? (emit/compiled l))))))

(defn- trama-header [text]
  (json/read-str (second (re-find #"(?s)^/\*! trama\s+(.*?)\*/" text))))

(deftest trama-structure-and-slot-order
  (doseq [l ports/layers]
    (testing (name (:id l))
      (let [files (emit/layer-files (assoc l :target :trama))
            _ (is (result/ok? files))
            text (get (:ok files) (str (name (:id l)) ".wgsl"))
            header (trama-header text)]
        (is (= #{"name" "id" "kind" "inputs" "params" "rates" "integers"} (set (keys header))))
        (is (= ["source" 0 (name (:id l))] (mapv header ["kind" "inputs" "id"])))
        (is (= (into ["loop_rate"] (map name (expected-param-order (:id l))))
               (mapv #(get % "name") (get header "params"))))
        (is (<= (count (get header "params")) 16))
        (is (= ["loop_rate"] (get header "rates")))
        (is (str/includes? text (str "// desargues: blend=" (name (:blend l)))))
        (is (str/includes? text "fract(param(0u) /"))
        (is (not (str/includes? text "u.time")))
        (is (str/includes? text "vec4f(clamp(desargues_c, vec3f(0.0), vec3f(1.0)), 1.0)"))))))

(deftest pfx-names-and-slot-order
  (is (= (set (keys expected-effect-names)) (set (keys ports/effects))))
  (testing "display names are unique across all layers and not fosfora built-ins"
    (let [names (map #(get-in (layers/pfx-layer %) [:pfx :name]) layers/layers)]
      (is (every? string? names))
      (is (apply distinct? names))
      (is (not-any? pfx/builtin-name? names))))
  (doseq [l ports/layers]
    (testing (name (:id l))
      (let [id (name (:id l))
            files (emit/layer-files (layers/pfx-layer l))
            _ (is (result/ok? files))
            pfx (json/read-str (get (:ok files) (str id ".pfx")))
            wgsl-text (get (:ok files) (str id ".wgsl"))
            n (inc (count (expected-param-order (:id l))))]
        (is (= [(str id ".pfx") (str id ".wgsl")] (keys (:ok files))))
        (is (= (expected-effect-names (:id l)) (get pfx "name")))
        (is (= ["tessera" (str id ".wgsl") "pattern" ["loop_rate"]]
               (mapv pfx ["author" "shader" "category" "rates"])))
        (is (seq (get pfx "description")))
        (is (= (get (trama-header (get (:ok (emit/layer-files (assoc l :target :trama))) (str id ".wgsl")))
                    "params")
               (get pfx "inputs")))
        (testing "the integral of loop_rate is read from the extra slot after the params"
          (is (str/includes? wgsl-text (str "fract(param(" n "u) /")))
          (is (not (str/includes? wgsl-text "param(0u)")))
          (is (<= (inc n) 16)))
        (is (not (str/includes? wgsl-text "/*! trama")))))))

(deftest wgsl-expression-tree-agrees-with-jvm
  ;; CPU interpretation of exactly the emitted WGSL IR; not a GPU claim.
  (doseq [l ports/layers
          :let [tree (:ok (wgsl/ir (:form l)))]]
    (doseq [u phases x [-3.17 0.0 2.23] y [-1.12 0.17 1.83]
            :let [bf (layers/bind l u) shader (field/jvm-shader bf)]]
      (is (= (.invokePrim ^clojure.lang.IFn$DDL shader (double x) (double y))
             (apply field/pack (wgsl/interpret tree x y (:field/args bf))))
          (str (:id l) " u=" u " at " x "," y)))))

(deftest field-raster-parity
  (doseq [l ports/layers u phases]
    (testing (str (:id l) " u=" u)
      (let [{:keys [mean max]} (parity l u)]
        (is (< mean 1.0))
        (is (<= max 1))))))

(deftest supersampling-preserves-parity
  (doseq [l ports/layers u phases]
    (testing (str (:id l) " u=" u " 192x108 ss=2")
      (let [{:keys [mean max]} (parity l u [192 108] 2)]
        (is (< mean 1.0))
        (is (<= max 1))))))

(deftest the-loop-closes
  ;; u = 1 must show the u = 0 frame (cos(2 pi k) differs from cos 0 only in
  ;; the last bit, so compare at the 8-bit output).
  (doseq [l ports/layers]
    (testing (name (:id l))
      (let [{:keys [mean max]} (channel-diff (raster (layers/bind l 0.0) (:view l) [96 54] 1)
                                             (raster (layers/bind l 1.0) (:view l) [96 54] 1))]
        (is (< mean 0.05))
        (is (<= max 1))))))
