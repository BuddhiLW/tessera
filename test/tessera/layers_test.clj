(ns tessera.layers-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.emit :as emit]
            [desargues.wallpaper.field :as field]
            [desargues.wallpaper.render :as render]
            [hive-dsl.result :as result]
            [tessera.layers :as layers]
            [tessera.palette :as palette]
            [tessera.reciprocal :as reciprocal]
            [tessera.lattice :as lattice]
            [tessera.loops :as loops]
            [tessera.vj :as vj]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [desargues.wallpaper.wgsl :as wgsl])
  (:import (java.awt.image BufferedImage)))

(def phases [0.0 0.3 0.7])
(def parity-size [96 54])

(def expected-param-order
  ;; Binding ABI: loop_rate is implicit slot 0. Only append new params.
  {:graphene_fermi '[fermi_level contour_step sharpness lattice_scale]
   :kagome_fermi '[fermi_level contour_step sharpness lattice_scale]
   :square_fermi '[fermi_level contour_step sharpness lattice_scale]
   :penrose_fermi '[fermi_level contour_step sharpness lattice_scale]
   :heptagonal_fermi '[fermi_level contour_step sharpness lattice_scale]
   :octagonal_fermi '[fermi_level contour_step sharpness lattice_scale]
   :quasicrystal '[wave_number contrast phase_offset]
   :lieb_fermi '[lattice_scale contour_step sharpness probe_amplitude flat_band_pulse]
   :brillouin_zones '[edge_width pulse_zones]
   :kaleido_quasicrystal '[fold_order wave_number lens_scale contour_density]})

(defn reference-field
  "Offline pre-postprocess reference. Graphene deliberately corrects a to
   4pi/3; compare the shipped 4.2 separately, not as an implementation error."
  [id u]
  (case id
    :graphene_fermi (reciprocal/fermi-shader u {:palette (palette/scheme :yellow-purple-magenta)
                                              :a (/ (* 4.0 Math/PI) 3.0)})
    :kagome_fermi ((:field reciprocal/kagome-fermi) u)
    :square_fermi ((:field reciprocal/square-fermi) u)
    :penrose_fermi ((:field reciprocal/penrose-fermi) u)
    :heptagonal_fermi ((:field reciprocal/heptagonal-fermi) u)
    :octagonal_fermi ((:field reciprocal/octagonal-fermi) u)
    :quasicrystal ((:field loops/quasicrystal) u)
    :lieb_fermi (lattice/lieb-shader u)
    :brillouin_zones ((:field reciprocal/brillouin-zones) u)
    :kaleido_quasicrystal (vj/kaleido-shader u)))

(defn raster
  [shader view size ss]
  (let [[w h] size
        img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
    (render/field-raster :jvm img shader {:view view :ss ss})
    img))

(defn channel-diff
  "Mean absolute channel error and worst channel error, in 8-bit levels."
  [^BufferedImage a ^BufferedImage b]
  (let [diffs (for [y (range (.getHeight a)) x (range (.getWidth a))
                    shift [16 8 0]]
                (Math/abs (long (- (bit-and 255 (bit-shift-right (.getRGB a x y) shift))
                                  (bit-and 255 (bit-shift-right (.getRGB b x y) shift))))))]
    {:mean (/ (double (reduce + diffs)) (count diffs)) :max (reduce max diffs)}))

(defn parity
  ([layer u] (parity layer u parity-size 1))
  ([layer u size ss]
   (channel-diff (raster (layers/bind layer u) (:view layer) size ss)
                 (raster (reference-field (:id layer) u) (:view layer) size ss))))

(defn parity-report []
  (mapv (fn [l]
          {:id (:id l) :phases (mapv (fn [u] (assoc (parity l u) :u u)) phases)})
        layers/layers))

(deftest published-parameter-order
  (is (= (set (keys expected-param-order)) (set (map :id layers/layers))))
  (doseq [l layers/layers]
    (testing (name (:id l))
      (is (vector? (:params l)))
      (is (= (expected-param-order (:id l)) (mapv first (:params l))))
      (is (= (into ['u] (expected-param-order (:id l))) (get-in l [:form :params])))
      (is (<= (inc (count (:params l))) 16))
      (is (re-matches #"[a-z0-9]+(?:_[a-z0-9]+)*" (name (:id l))))
      (doseq [[p {:keys [default min max]}] (:params l)]
        (is (re-matches #"[a-z][a-z0-9_]*" (name p)))
        (is (<= min default max))))))

(deftest every-form-compiles-on-jvm-and-glsl
  (doseq [l layers/layers]
    (testing (name (:id l))
      (is (fn? (field/jvm-shader (layers/bind l 0.0))))
      (is (result/ok? (field/source :glsl (:form l) {})))
      (is (result/ok? (emit/compiled l))))))

(deftest isf-structure-and-input-order
  (doseq [l layers/layers]
    (testing (name (:id l))
      (let [compiled (emit/compiled l)
            text (when (result/ok? compiled) (emit/isf (:ok compiled)))
            header-text (when text (second (re-find #"(?s)^/\*(.*?)\*/" text)))
            header (when header-text (json/read-str header-text))]
        (is (= "2" (get header "ISFVSN")))
        (is (= (into ["u" "rate"] (map name (expected-param-order (:id l))))
               (mapv #(get % "NAME") (get header "INPUTS"))))
        (is (= (name (:id l)) (get-in header ["DESARGUES" "id"])))
        (is (str/includes? text "void main()"))
        (is (str/includes? text "vec4(clamp(desargues_c, 0.0, 1.0), 1.0)"))))))

(deftest trama-structure-and-slot-order
  (doseq [l layers/layers]
    (testing (name (:id l))
      (let [files (emit/layer-files (assoc l :target :trama))
            _ (is (result/ok? files))
            text (get (:ok files) (str (name (:id l)) ".wgsl"))
            header (json/read-str (second (re-find #"(?s)^/\*! trama\s+(.*?)\*/" text)))]
        (is (= #{"name" "id" "kind" "inputs" "params" "rates" "integers"} (set (keys header))))
        (is (= "source" (get header "kind")))
        (is (= 0 (get header "inputs")))
        (is (= (name (:id l)) (get header "id")))
        (is (= (into ["loop_rate"] (map name (expected-param-order (:id l))))
               (mapv #(get % "name") (get header "params"))))
        (is (<= (count (get header "params")) 16))
        (is (= ["loop_rate"] (get header "rates")))
        (is (str/includes? text (str "// desargues: blend=" (name (:blend l)))))
        (is (str/includes? text "fract(param(0u) /"))
        (is (not (str/includes? text "u.time")))
        (is (str/includes? text "vec4f(clamp(desargues_c, vec3f(0.0), vec3f(1.0)), 1.0)"))))))

(def expected-effect-names
  ;; Catalog titles are fosfora preset keys: never rename.
  {:graphene_fermi "Graphene Fermi" :kagome_fermi "Kagome Fermi" :square_fermi "Square Fermi"
   :penrose_fermi "Penrose Fermi" :heptagonal_fermi "Heptagonal Fermi" :octagonal_fermi "Octagonal Fermi"
   :quasicrystal "Quasicrystal" :lieb_fermi "Lieb Fermi" :brillouin_zones "Brillouin Zones"
   :kaleido_quasicrystal "Kaleido Quasicrystal"})

(deftest pfx-names-and-slot-order
  (is (= (map :id layers/layers) (keys layers/effects)))
  (is (= (count expected-effect-names) (count (distinct (vals expected-effect-names)))))
  (doseq [l layers/layers]
    (testing (name (:id l))
      (let [id (name (:id l))
            files (emit/layer-files (layers/pfx-layer l))
            _ (is (result/ok? files))
            pfx (json/read-str (get (:ok files) (str id ".pfx")))
            wgsl-text (get (:ok files) (str id ".wgsl"))
            n (inc (count (expected-param-order (:id l))))]
        (is (= [(str id ".pfx") (str id ".wgsl")] (keys (:ok files))))
        (is (= ["name" "author" "description" "shader" "category" "inputs" "rates"]
               (vec (keys (json/read-str (get (:ok files) (str id ".pfx")) :key-fn identity)))))
        (is (= (expected-effect-names (:id l)) (get pfx "name")))
        (is (= ["tessera" (str id ".wgsl") "pattern" ["loop_rate"]] (mapv pfx ["author" "shader" "category" "rates"])))
        (is (seq (get pfx "description")))
        (is (= (into ["loop_rate"] (map name (expected-param-order (:id l))))
               (mapv #(get % "name") (get pfx "inputs"))))
        (testing "inputs are exactly the trama params"
          (let [trama (get (:ok (emit/layer-files (assoc l :target :trama))) (str id ".wgsl"))]
            (is (= (get (json/read-str (second (re-find #"(?s)^/\*! trama\s+(.*?)\*/" trama))) "params")
                   (get pfx "inputs")))))
        (testing "the integral of loop_rate is read from the extra slot after the params"
          (is (str/includes? wgsl-text (str "fract(param(" n "u) /")))
          (is (not (str/includes? wgsl-text "param(0u)")))
          (is (<= (inc n) 16)))
        (is (not (str/includes? wgsl-text "/*! trama")))))))

(deftest wgsl-expression-tree-agrees-with-jvm
  ;; CPU interpretation of exactly the emitted WGSL IR; not a GPU claim.
  (doseq [l layers/layers
          :let [compiled (wgsl/ir (:form l))
                tree (:ok compiled)]]
    (is (result/ok? compiled))
    (doseq [u phases x [-3.17 0.0 2.23] y [-1.12 0.17 1.83]
            :let [bf (layers/bind l u) shader (field/jvm-shader bf)]]
      (is (= (.invokePrim ^clojure.lang.IFn$DDL shader (double x) (double y))
             (apply field/pack (wgsl/interpret tree x y (:field/args bf))))))))

(deftest field-raster-parity
  (doseq [l layers/layers u phases]
    (testing (str (:id l) " u=" u)
      (let [{:keys [mean max]} (parity l u)]
        (is (< mean 1.0))
        (is (<= max 1))))))

(deftest supersampling-preserves-parity
  (doseq [l layers/layers u phases]
    (testing (str (:id l) " u=" u " 192x108 ss=2")
      (let [{:keys [mean max]} (parity l u [192 108] 2)]
        (is (< mean 1.0))
        (is (<= max 1))))))

(defn graphene-shipped-difference
  "Separate audit of the intentional a=4pi/3 correction versus shipped 4.2."
  []
  (mapv (fn [u]
          (assoc (channel-diff
                  (raster (layers/bind layers/graphene-fermi u) [8.0 4.5] parity-size 1)
                  (raster ((:field reciprocal/graphene-fermi) u) [8.0 4.5] parity-size 1))
                 :u u))
        phases))
