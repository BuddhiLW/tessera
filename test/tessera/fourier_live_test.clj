(ns tessera.fourier-live-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.emit :as emit]
            [desargues.wallpaper.field :as field]
            [desargues.wallpaper.wgsl :as wgsl]
            [tessera.figures :as figures]
            [tessera.fourier :as f]
            [tessera.layers :as layers]
            [tessera.layers.ports.fourier :as live]
            [tessera.loops :as loops]
            [tessera.media :as media]
            [tessera.vj-test :as metrics]))

(deftest bounded-analytic-spectrum-and-slot-abi
  (is (= #{2 -1} (set (map :frequency live/terms))))
  (is (<= (count live/terms) live/max-terms 24))
  (is (= live/fourier-live (layers/by-id :fourier_live)))
  (is (= '[trail_width ring_width tail_length ring_gain] (mapv first (:params live/fourier-live))))
  (is (= '[u trail_width ring_width tail_length ring_gain] (get-in live/fourier-live [:form :params])))
  (let [ast (get-in live/fourier-live [:form :body]) nodes (tree-seq coll? seq ast)]
    (is (< (count nodes) 14000))
    (is (not-any? '#{loop recur for map reduce nth get texture} nodes)))
  (doseq [i (range 120) :let [u (/ i 120.0) a (f/point live/terms u) b (subvec (figures/trefoil u) 0 2)]]
    (is (< (Math/hypot (- (a 0) (b 0)) (- (a 1) (b 1))) 1.0e-11))))

(deftest analytic-distance-estimator-tracks-the-curve
  ;; This tests the analytic solver, not the emitted form against itself.
  (doseq [i (range 240) :let [[x y] (f/point live/terms (/ i 240.0))]]
    (is (< (apply min (map :distance (live/closest-candidates x y))) 1.0e-8)))
  (let [reference (mapv #(f/point live/terms (/ % 8192.0)) (range 8192))]
    (doseq [i (range 9) j (range 9)
            :let [x (- (/ i 2.0) 2.0) y (- (/ j 2.0) 2.0)
                  estimate (apply min (map :distance (live/closest-candidates x y)))
                  sampled (reduce min Double/POSITIVE_INFINITY
                                  (map (fn [[px py]] (Math/hypot (- x px) (- y py))) reference))]]
      ;; 0.005 scene units is < one native 1080p pixel (8/1080).
      (is (< (abs (- estimate sampled)) 0.005)))))

(defn reference-rgb
  "Numeric reference separate from field AST generation: analytic trail
   distances and ages, exact ring / segment distances, and drawing head."
  [x y u {:keys [trail-width ring-width tail-length ring-gain]
           :or {trail-width 0.025 ring-width 0.009 tail-length 0.85 ring-gain 0.25}}]
  (let [gaussian (fn [d w] (Math/exp (- (Math/pow (/ d w) 2.0))))
        trail (reduce max 0.0
                      (for [{:keys [angle distance]} (live/closest-candidates x y)
                            :let [v (- u (/ angle (* 2.0 Math/PI))) age (- v (Math/floor v))
                                  fade (Math/pow (max 0.0 (- 1.0 (/ age tail-length))) 2.0)]]
                        (* fade (gaussian (max 0.0 (- distance trail-width)) trail-width))))
        ends (f/chain live/terms u)
        rings (reduce min Double/POSITIVE_INFINITY
                      (map (fn [a term] (- (abs (- (Math/hypot (- x (a 0)) (- y (a 1))) (:magnitude term))) ring-width))
                           ends live/terms))
        rods (reduce min Double/POSITIVE_INFINITY
                     (map (fn [[ax ay] [bx by]]
                            (let [dx (- bx ax) dy (- by ay)
                                  h (max 0.0 (min 1.0 (/ (+ (* (- x ax) dx) (* (- y ay) dy))
                                                         (max 1.0e-9 (+ (* dx dx) (* dy dy))))))]
                              (- (Math/hypot (- x ax (* h dx)) (- y ay (* h dy))) 0.009)))
                          ends (rest ends)))
        ring (gaussian (max 0.0 rings) ring-width) rod (gaussian (max 0.0 rods) 0.014)
        [hx hy] (peek ends) head (gaussian (Math/hypot (- x hx) (- y hy)) 0.06)]
    [(+ (* 0.10 trail) (* 0.65 ring-gain ring) (* 0.6 rod) head)
     (+ (* 0.88 trail) (* 0.15 ring-gain ring) (* 0.32 rod) (* 0.80 head))
     (+ trail (* 0.85 ring-gain ring) (* 0.05 rod) (* 0.45 head))]))

(deftest field-agrees-with-independent-distance-reference
  (doseq [u [0.0 0.3 0.7 1.0]
          :let [shader (field/jvm-shader (layers/bind live/fourier-live u))]
          i (range -5 6) j (range -5 6)
          :let [x (* 0.53 i) y (* 0.41 j)
                expected (apply field/pack (reference-rgb x y u {}))
                actual (shader x y)] shift [0 8 16]]
    (is (<= (abs (- (bit-and 255 (bit-shift-right expected shift))
                   (bit-and 255 (bit-shift-right actual shift)))) 1))))

(deftest jvm-glsl-wgsl-and-fosfora-emitters
  (let [layer live/fourier-live tree (:ok (wgsl/ir (:form layer)))]
    (is (map? tree))
    (is (:ok (field/source :glsl (:form layer) {})))
    (doseq [u [0.0 0.3 0.7] [x y] [[0.0 0.0] [1.0 2.0] [-2.01 0.54]]
            :let [bound (layers/bind layer u) shader (field/jvm-shader bound)]]
      (is (= (shader x y) (apply field/pack (wgsl/interpret tree x y (:field/args bound))))))
    (let [pfx-files (:ok (emit/layer-files (layers/pfx-layer layer)))
          trama-files (:ok (emit/layer-files (assoc layer :target :trama)))
          pfx (json/read-str (get pfx-files "fourier_live.pfx"))
          trama (get trama-files "fourier_live.wgsl")
          header (json/read-str (second (re-find #"(?s)^/\*! trama\s+(.*?)\*/" trama)))]
      (is (= "Fourier Epicycles" (get pfx "name")))
      (is (= ["loop_rate" "trail_width" "ring_width" "tail_length" "ring_gain"]
             (mapv #(get % "name") (get header "params"))))
      (is (= (get header "params") (get pfx "inputs")))
      (is (< (count trama) 55000))
      (is (not (str/includes? trama "u.time")))
      (is (not (str/includes? trama "textureSample"))))))

(deftest live-loop-closes-the-raster-seam
  (let [spec (:fourier-live loops/all)
        ;; 60 distributed samples cover the high-curvature part of this
        ;; analytic curve at the real 600-frame period, with SS=2 + post.
        {:keys [seam typical-max]} (metrics/seam-stats spec [160 90] (map #(/ (+ % 0.5) 60.0) (range 60)))
        a ((:field spec) 0.0) b ((:field spec) 1.0)]
    (is (<= seam typical-max))
    (doseq [x [-2.4 -0.3 0.0 1.2 2.8] y [-2.1 0.0 1.9]] (is (= (a x y) (b x y))))))

(deftest fourier-media-and-registry-wiring
  (doseq [id [:fourier-trefoil :fourier-space :fourier-formula :fourier-live]
          :let [spec (loops/all id) item (media/item spec #{})]]
    (testing (name id)
      (is (= id (:id spec)))
      (is (= "Curves" (:family item)))
      (is (seq (:title item)))
      (is (some #{"fourier"} (:tags item))))))
