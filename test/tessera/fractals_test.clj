(ns tessera.fractals-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.fractals :as f]
            [tessera.vj-test :as metrics]))

(defn- rot-y [[x y z] a]
  [(- (* x (Math/cos a)) (* z (Math/sin a))) y (+ (* x (Math/sin a)) (* z (Math/cos a)))])

(deftest the-bulb-has-seven-fold-symmetry-about-its-axis
  ;; Why a seventh of a turn closes the mandelbulb loop.
  (doseq [p [[0.5 0.3 0.4] [0.2 -0.6 0.7] [0.9 0.1 -0.2] [-0.3 0.45 0.62]]]
    (let [a (apply f/bulb-de p) b (apply f/bulb-de (rot-y p (/ (* 2.0 Math/PI) 7.0)))]
      (is (< (Math/abs (- a b)) (* 1.0e-9 (max 1.0 (Math/abs a))))))))

(deftest fields-repeat-after-one-period
  (doseq [[id spec] f/loops
          :let [a ((:field spec) 0.0) b ((:field spec) 1.0)]]
    (testing (name id)
      (doseq [[x y] [[0.31 0.12] [-1.7 0.9] [2.4 -1.3] [0.0 0.0] [-0.6 -2.1]]]
        (is (= (a x y) (b x y)))))))

(deftest every-fractal-loop-closes-its-seam-and-keeps-a-black-ground
  (doseq [[id spec] f/loops]
    (testing (name id)
      (is (= id (:id spec)))
      (is (= [1920 1080] (:size spec)))
      (is (= 30 (:fps spec)))
      ;; Frame review rule: the wrap N-1 -> 0 is no larger than an ordinary
      ;; step, measured at the native frame count.
      (let [{:keys [seam typical-max]} (metrics/seam-stats spec [96 54])
            {:keys [near-black white-clip]} (metrics/image-stats (r/render-frame spec 0.3 {:size [160 90]}))]
        (is (<= seam typical-max))
        (is (> near-black 0.2))
        (is (< white-clip 0.005))))))
