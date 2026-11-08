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
   :kaleido_quasicrystal '[fold_order wave_number lens_scale contour_density]
   :graphene_zones '[fermi_level contour_step sharpness lattice_scale
                     edge_width kagome_flash dirac_glow zone_cycle]})

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
    :kaleido_quasicrystal (vj/kaleido-shader u)
    ;; No closure predates this layer: the offline loop renders the layer's
    ;; own form, so this checks the loop wiring; the math is checked below.
    :graphene_zones ((:field (:graphene-zones loops/all)) u)))

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
   :kaleido_quasicrystal "Kaleido Quasicrystal" :graphene_zones "Graphene Zones"})

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

;; ── graphene-zones: geometry of the overlay ──────────────────────────────

(def ^:private gz-probe
  "x, y -> [|f| zone sqrt3_d kagome_d bragg_d] of the graphene-zones form at its
   default params, by interpreting the emitted expression tree in doubles."
  (delay
    (let [defaults (into {'u 0.0} (map (fn [[p s]] [p (:default s)])) (:params layers/graphene-zones))
          probe (fn [channels]
                  (let [form (layers/graphene-zones-form channels)
                        args (:field/args (apply field/bind form (map defaults (:params form))))
                        tree (:ok (wgsl/ir form))]
                    (fn [x y] (wgsl/interpret tree (double x) (double y) args))))
          a (probe '[e zn sqrt3_d])
          b (probe '[kagome_d bragg_d 0.0])]
      (fn [x y] (into (vec (a x y)) (take 2 (b x y)))))))

(def ^:private s3 (Math/sqrt 3.0))
(def ^:private a-exact (/ (* 4.0 Math/PI) 3.0))
(def ^:private pixel (/ 8.0 960.0))

(def ^:private k-points
  "Hexagram tips: the |G| = 1 points of the unit hexagonal lattice."
  (for [k (range 6) :let [t (* k (/ Math/PI 3.0))]] [(Math/cos t) (Math/sin t)]))

(def ^:private m-points
  "Midpoints of the sqrt3 edges: halfway between adjacent tips."
  (map (fn [[ax ay] [bx by]] [(* 0.5 (+ ax bx)) (* 0.5 (+ ay by))])
       k-points (concat (rest k-points) [(first k-points)])))

(defn- argmin-miss
  "Distance, in pixels, from [x0 y0] to the pixel centre minimizing
   (score x y) on a pixel grid of (2r+1)^2 centres around [x0 y0], the grid
   shifted off [x0 y0] by a sub-pixel amount."
  [score [x0 y0] r]
  (let [sx 0.37 sy 0.21
        [i j] (apply min-key (fn [[i j]] (score (+ x0 (* (+ sx i) pixel)) (+ y0 (* (+ sy j) pixel))))
                     (for [i (range (- r) (inc r)) j (range (- r) (inc r))] [i j]))]
    (Math/sqrt (+ (Math/pow (+ sx i) 2) (Math/pow (+ sy j) 2)))))

(deftest graphene-zones-lattice-constant-is-exact
  (is (= a-exact (some (fn [[p s]] (when (= 'lattice_scale p) (:default s))) (:params layers/graphene-zones))))
  (testing "the band is reciprocal/graphene-band at a = 4pi/3"
    (doseq [x [-3.1 -0.4 0.0 0.77 2.9] y [-2.0 -0.3 0.5 1.9]]
      (is (< (Math/abs (- (first (@gz-probe x y)) (reciprocal/graphene-band a-exact x y))) 1.0e-12)))))

(deftest graphene-zones-dirac-points-on-hexagram-tips
  (doseq [[x y :as k] k-points]
    (testing (str "K " k)
      (is (< (first (@gz-probe x y)) 1.0e-7) "|f(K)| = 0")
      (is (< (nth (@gz-probe x y) 2) 1.0e-12) "K is a corner of graphene's BZ")
      (is (<= (argmin-miss (fn [x y] (first (@gz-probe x y))) k 4) 1.0)
          "the zero of |f| on the pixel grid is within a pixel of the tip"))))

(deftest graphene-zones-saddles-on-sqrt3-edge-midpoints
  (doseq [[x y :as m] m-points
          :let [[e _ sqrt3-d kagome-d] (@gz-probe x y)]]
    (testing (str "M " m)
      (is (< (Math/abs (- (Math/sqrt (+ (* x x) (* y y))) (/ s3 2.0))) 1.0e-12))
      (is (< (Math/abs (- e 1.0)) 1.0e-12) "|f(M)| = 1")
      (is (< sqrt3-d 1.0e-12) "M lies on a sqrt3 plane")
      (is (< kagome-d 1.0e-12) "the kagome net passes through M")
      (is (<= (argmin-miss (fn [x y] (let [[_ _ d kd] (@gz-probe x y)] (+ d kd))) m 4) 1.0)
          "the kagome net crosses the sqrt3 plane within a pixel of M"))))

(deftest graphene-zones-kagome-net-is-the-van-hove-level
  (doseq [x [-2.7 -0.9 0.3 1.6 3.3] y [-1.7 -0.2 0.6 2.1]
          :let [p1 (* a-exact x) p2 (* a-exact (+ (* 0.5 x) (* 0.5 s3 y)))
                [e] (@gz-probe x y)]]
    (is (< (Math/abs (- (- (* e e) 1.0)
                        (* 8.0 (Math/cos (* 0.5 p1)) (Math/cos (* 0.5 p2)) (Math/cos (* 0.5 (- p1 p2))))))
           1.0e-12)))
  (testing "points on a kagome line (k.a1 = pi) sit at |f| = 1 with kagome_d = 0"
    (doseq [y [-2.0 -0.7 0.0 0.9 2.2]
            :let [[e _ _ kd] (@gz-probe 0.75 y)]]
      (is (< (Math/abs (- e 1.0)) 1.0e-12))
      (is (< kd 1.0e-12))))
  (testing "the kagome lines lie halfway between the |G| = 1 and |G| = 2 planes"
    (is (< (nth (@gz-probe 0.75 0.0) 3) 1.0e-12))
    (is (< (Math/abs (- (nth (@gz-probe 0.5 0.0) 4) 0.0)) 1.0e-12) "|G| = 1 plane at x = 1/2")
    (is (< (Math/abs (- (nth (@gz-probe 1.0 0.0) 4) 0.0)) 1.0e-12) "|G| = 2 plane at x = 1")
    (is (< (Math/abs (- (nth (@gz-probe 0.5 0.0) 3) 0.25)) 1.0e-12))
    (is (< (Math/abs (- (nth (@gz-probe 1.0 0.0) 3) 0.25)) 1.0e-12))))

(deftest graphene-zones-graphene-bz-is-zones-one-to-three
  (let [inside (for [i (range -60 61) j (range -60 61)
                     :let [x (* i 0.0165) y (* j 0.0165)]
                     :when (every? (fn [[gx gy]] (< (+ (* x gx) (* y gy)) 1.4999))
                                   (for [k (range 6) :let [t (+ (/ Math/PI 6.0) (* k (/ Math/PI 3.0)))]]
                                     [(* s3 (Math/cos t)) (* s3 (Math/sin t))]))]
                 (long (second (@gz-probe x y))))
        counts (frequencies inside)
        n (count inside)]
    (is (= #{1 2 3} (set (keys counts))))
    (doseq [z [1 2 3]]
      (is (< (Math/abs (- (/ (double (counts z)) n) (/ 1.0 3.0))) 0.02) (str "zone " z " is a third")))))

(deftest graphene-zones-loop-is-seamless
  (let [l layers/graphene-zones]
    (is (<= (+ 2 (count (:params l))) 16) "params + loop_rate + rate integral fit 16 slots")
    (let [{:keys [mean max]} (channel-diff (raster (layers/bind l 0.0) (:view l) [160 90] 1)
                                           (raster (layers/bind l 1.0) (:view l) [160 90] 1))]
      (is (< mean 0.05))
      (is (<= max 1)))))
