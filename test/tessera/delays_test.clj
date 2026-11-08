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

(deftest periodic-noise-flow-is-closed-and-incompressible
  (doseq [[x y u] [[0.3 -1.1 0.12] [-2.7 0.8 0.55] [3.1 1.9 0.9]]]
    (let [a (d/noise-flow x y u) b (d/noise-flow x y (+ u 1.0))
          e 1.0e-5
          ;; central-difference divergence of v = (d_y psi, -d_x psi)
          div (/ (+ (- (aget (d/noise-flow (+ x e) y u) 0) (aget (d/noise-flow (- x e) y u) 0))
                    (- (aget (d/noise-flow x (+ y e) u) 1) (aget (d/noise-flow x (- y e) u) 1)))
                 (* 2.0 e))
          ;; vorticity = -laplacian psi, by central differences of psi
          psi (fn [x y] (aget (d/noise-flow x y u) 3))
          h 1.0e-3
          lap (/ (+ (psi (+ x h) y) (psi (- x h) y) (psi x (+ y h)) (psi x (- y h)) (* -4.0 (psi x y)))
                 (* h h))]
      (dotimes [i 4] (is (< (Math/abs (- (aget a i) (aget b i))) 1.0e-9)))
      (is (< (Math/abs div) 1.0e-6))
      (is (< (Math/abs (+ lap (aget a 2))) 1.0e-3)))))

(deftest gerstner-parcels-run-one-delayed-circle
  ;; with the ripple far away, a parcel's orbit is periodic and every
  ;; train shifts it by a delay proportional to k.x
  (doseq [[x y] [[2.5 -1.7] [-3.2 1.9] [0.4 0.1]]
          u [0.0 0.21 0.67]]
    (let [p (d/gerstner x y u) q (d/gerstner x y (+ u 1.0))]
      (is (< (apply max (map #(Math/abs (- %1 %2)) p q)) 1.0e-9))))
  (let [z (for [x (range -4.0 4.0 0.1) y (range -2.25 2.25 0.1)]
            (nth (d/gerstner x y 0.37) 2))]
    (is (< (apply max (map #(Math/abs (double %)) z)) 0.4))))

(deftest cyclic-palette-closes-and-stays-saturated
  (doseq [t [0.0 0.13 0.5 0.81]]
    (let [a (d/cyclic-colour d/swing-stops t) b (d/cyclic-colour d/swing-stops (+ t 1.0))]
      (is (< (apply max (map #(Math/abs (- %1 %2)) a b)) 1.0e-12))))
  (doseq [t (range 0.0 1.0 0.01)
          :let [c (d/cyclic-colour d/swing-stops t) hi (apply max c) lo (apply min c)]]
    (is (> (/ (- hi lo) hi) 0.6))
    ;; never green-dominant: the cycle stays cyan / violet / gold
    (is (not (and (> (second c) (first c)) (> (second c) (nth c 2)))))))

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
            ;; Splat loops: the rasteriser's 0.7 px minimum sigma makes thin
            ;; strokes ~3x fatter at 240x135 than at the 640x360 preview, so
            ;; image statistics are measured at the preview size.
            (vt/image-stats (r/render-frame spec 0.3 {:size [640 360]}))]
        (is (= [1920 1080] (:size spec)))
        (is (= 30 (:fps spec)))
        ;; Strict criterion: no slack multiplier or additive tolerance.
        (is (<= seam typical-max))
        (is (> mean 4.0))
        (is (> std 8.0))
        (is (> near-black 0.15))
        (is (> lit-saturation 0.6))
        (is (zero? white-clip))))))
