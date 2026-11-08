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
            [clojure.string :as str])
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
      (is (result/ok? (emit/compiled (layers/isf-layer l)))))))

(deftest isf-structure-and-input-order
  (doseq [l layers/layers]
    (testing (name (:id l))
      (let [compiled (emit/compiled (layers/isf-layer l))
            text (when (result/ok? compiled) (emit/isf (:ok compiled)))
            header-text (when text (second (re-find #"(?s)^/\*(.*?)\*/" text)))
            header (when header-text (json/read-str header-text))]
        (is (= "2" (get header "ISFVSN")))
        (is (= (into ["u" "rate"] (map name (expected-param-order (:id l))))
               (mapv #(get % "NAME") (get header "INPUTS"))))
        (is (= (name (:id l)) (get-in header ["DESARGUES" "id"])))
        (is (str/includes? text "void main()"))
        (is (str/includes? text "vec4(clamp(desargues_c, 0.0, 1.0), 1.0)"))))))

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
