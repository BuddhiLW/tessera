(ns tessera.vj2-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.vj2 :as v2]
            [tessera.vj-test :as metrics]))

(deftest placement-is-reproducible-and-poisson-disc
  (let [sites (v2/poisson-sites 73129 100 0.46)]
    (is (= 100 (count sites)))
    (is (= sites v2/fabric-sites))
    (is (not= sites (v2/poisson-sites 63129 100 0.46)))
    (doseq [i (range (count sites)) j (range i)
            :let [[x y] (nth sites i) [a b] (nth sites j)]]
      (is (>= (Math/hypot (- x a) (- y b)) 0.46)))))

(deftest integer-beats-and-one-period-advection
  (doseq [u [0.0 0.17 0.3 0.71]]
    (is (< (Math/abs (- (v2/travel-phase (+ u 1.0)) (v2/travel-phase u) 1.0)) 1.0e-12))
    (let [^doubles a (v2/ornament-data u) ^doubles b (v2/ornament-data (+ u 1.0))]
      (is (< (apply max (map #(Math/abs (- %1 %2)) a b)) 1.0e-12))))
  (doseq [u [0.0 0.17 0.3 0.71]
          :let [a (v2/fabric-shader u) b (v2/fabric-shader (+ u 1.0))]
          [x y] [[0.73 0.29] [-2.31 1.01] [3.74 -1.88] [0.2 0.1] [-1.1 -0.65]]]
    (is (= (a x y) (b x y)))))

(deftest hyperspace-is-a-self-similar-hex-lattice-not-respawning-particles
  (let [seeds (v2/hyperspace-seeds)]
    (is (<= 15 (count seeds) 30))
    (doseq [{:keys [x y q angle]} seeds]
      (is (<= 0.0 q 1.0))
      (is (< 0.0 angle (/ Math/PI 12.0)))
      (let [j (/ y (* 0.14 0.866025403784)) i (- (/ x 0.14) (* 0.5 j))]
        (is (< (Math/abs (- j (Math/rint j))) 1.0e-10))
        (is (< (Math/abs (- i (Math/rint i))) 1.0e-10)))))
  (doseq [[x y] [[0.6 1.1] [-0.8 0.31] [1.7 -0.42]] u [0.0 0.3 0.71]]
    (let [[q a] (v2/hyperspace-coordinates x y u)
          [q1 a1] (v2/hyperspace-coordinates (* 2.0 x) (* 2.0 y) (+ u 1.0))]
      (is (< (Math/abs (- q q1)) 1.0e-12))
      (is (< (Math/abs (- a a1)) 1.0e-12))))
  (doseq [u [0.0 0.17 0.3 0.71]
          :let [a (v2/hyperspace-shader u) b (v2/hyperspace-shader (+ u 1.0))]
          [x y] [[0.73 0.29] [-2.31 1.01] [3.74 -1.88] [0.2 0.1]]]
    (is (= (a x y) (b x y)))
    (is (= (a x y) (a x (- y))))))

(deftest flower-advects-by-one-exact-dyadic-step
  (doseq [[x y] [[0.6 1.1] [-0.8 0.31] [1.7 -0.42]] u [0.0 0.3 0.71]]
    (let [[q a] (v2/flower-coordinates x y u)
          [q1 a1] (v2/flower-coordinates (* 2.0 x) (* 2.0 y) (+ u 1.0))]
      (is (< (Math/abs (- q q1)) 1.0e-12))
      (is (< (Math/abs (- a a1)) 1.0e-12))))
  (doseq [u [0.0 0.17 0.3 0.71]
          :let [a (v2/flower-shader u) b (v2/flower-shader (+ u 1.0))]
          [x y] [[0.73 0.29] [-2.31 1.01] [3.74 -1.88] [0.2 0.1] [0.34 0.015] [0.09 0.04]]]
    (is (= (a x y) (b x y))))
  ;; The eighteen small dashed circles and RGB rosette must be visible,
  ;; rather than the central rings merely being named in a title.
  (let [f (v2/flower-shader 0.3)
        peak (fn [c] (apply max (map #(bit-and 255 (bit-shift-right c %)) [0 8 16])))
        ring-values (for [i (range 720)
                          :let [a (* 2.0 Math/PI (/ i 720.0))]]
                      (peak (f (* 0.34 (Math/cos a)) (* 0.34 (Math/sin a)))))
        rosette-values (for [j (range 30) i (range 30)]
                         (peak (f (- (* 0.01 i) 0.15) (- (* 0.01 j) 0.15))))]
    (is (> (apply max ring-values) 100))
    (is (> (apply max rosette-values) 100))))

(deftest gallery-specs-and-strict-native-frame-seams
  (doseq [[id spec] v2/loops]
    (testing (name id)
      (is (= id (:id spec)))
      (is (= [1920 1080] (:size spec)))
      (is (= 30 (:fps spec)))
      (is (= 2 (:ss spec)))
      (is (not (contains? spec :scene)))
      (let [{:keys [seam typical-max]} (metrics/seam-stats spec)
            {:keys [mean std near-black lit-saturation white-clip peak-channel]}
            (metrics/image-stats (r/render-frame spec 0.3 {:size [240 135]}))]
        ;; No tolerance: compare N-1 -> 0 against actual consecutive frames.
        (is (<= seam typical-max))
        (is (> mean 5.0))
        (is (> std 10.0))
        (is (> near-black 0.10))
        (is (> lit-saturation 0.70))
        (is (zero? white-clip))
        (is (< peak-channel 250))))))

(deftest fabric-is-all-over-with-black-between-clusters
  (let [shader (v2/fabric-shader 0.3) blocks (v2/fabric-blocks 0.3)
        values (for [j (range 90) i (range 160)]
                 (blocks (+ -4.0 (* 8.0 (/ (+ i 0.5) 160.0)))
                         (- 2.25 (* 4.5 (/ (+ j 0.5) 90.0)))))
        coverage (/ (count (filter pos? values)) (double (count values)))]
    ;; Coordinator's revised contract: discrete platforms occupy 45..55%,
    ;; not a uniform wallpaper. The remaining space is genuinely black.
    (is (<= 0.45 coverage 0.55))
    (doseq [row (range 4) col (range 8)]
      (let [samples (for [j (range 12) i (range 12)]
                      (shader (+ -4.0 col (/ (+ i 0.5) 12.0))
                              (+ -2.25 (* row 1.125) (* 1.125 (/ (+ j 0.5) 12.0)))))
            lit (count (filter #(> (apply max (map (fn [s] (bit-and 255 (bit-shift-right % s))) [0 8 16])) 32) samples))]
        (is (> lit 20))))))
