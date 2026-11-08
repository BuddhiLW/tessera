(ns tessera.delays-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.delays :as d]
            [tessera.vj-test :as vt]))

(deftest pendulum-period-is-the-agm-elliptic-integral
  ;; small-amplitude limit 2pi, and T(a) = 4 K(sin a/2) at a = pi/2: 7.4163
  (is (< (Math/abs (- (d/pendulum-period 1.0e-6) (* 2.0 Math/PI))) 1.0e-9))
  (is (< (Math/abs (- (d/pendulum-period (/ Math/PI 2.0)) 7.416298709205487)) 1.0e-9))
  (doseq [a [0.5 1.7 2.6]]
    (let [orbit (d/pendulum-orbit a 512)]
      ;; released from rest at +a, reaches -a at half period, closes at one period
      (is (< (Math/abs (- (aget orbit 0) a)) 1.0e-12))
      (is (< (Math/abs (+ (aget orbit 256) a)) 1.0e-8))
      (is (< (Math/abs (- (d/pendulum-angle a 0.37) (d/pendulum-angle a 1.37))) 1.0e-12)))))

(deftest every-delay-loop-closes-exactly-in-its-phase
  (doseq [[id spec] d/loops]
    (testing (name id)
      (doseq [u [0.0 0.17 0.71]]
        (let [a (vt/pixels (r/render-frame spec u {:size [160 90]}))
              b (vt/pixels (r/render-frame spec (+ u 1.0) {:size [160 90]}))]
          (is (< (vt/mean-diff a b) 0.05)))))))

(deftest every-delay-loop-is-black-ground-saturated-and-seamless
  (doseq [[id spec] d/loops]
    (testing (name id)
      (let [{:keys [seam typical-max]} (vt/seam-stats spec)
            {:keys [mean std near-black lit-saturation white-clip]}
            (vt/image-stats (r/render-frame spec 0.3 {:size [240 135]}))]
        (is (= [1920 1080] (:size spec)))
        (is (= 30 (:fps spec)))
        ;; Strict criterion: no slack multiplier or additive tolerance.
        (is (<= seam typical-max))
        (is (> mean 4.0))
        (is (> std 8.0))
        (is (> near-black 0.15))
        (is (> lit-saturation 0.6))
        (is (zero? white-clip))))))
