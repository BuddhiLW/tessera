(ns tessera.figures-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.figures :as fig]
            [tessera.vj-test :as metrics]))

(deftest comets-wrap-around-the-curve
  (is (= (fig/comets 0.0 3 0.5 0.1) (fig/comets 1.0 3 0.5 0.1)))
  (is (= 1.0 (fig/comets 0.25 1 0.25 0.1)))
  (is (zero? (fig/comets 0.25 1 0.5 0.1))))

(deftest the-harmonograph-returns-after-one-loop
  (doseq [t [0.0 17.3 120.0 359.0]]
    (let [[x0 y0] (fig/harmonograph-point 0.0 t) [x1 y1] (fig/harmonograph-point 1.0 t)]
      (is (< (Math/abs (- x0 x1)) 1.0e-9))
      (is (< (Math/abs (- y0 y1)) 1.0e-9)))))

(deftest every-figure-loop-closes-its-seam-on-a-black-ground
  (doseq [[id spec] fig/loops]
    (testing (name id)
      (is (= id (:id spec)))
      (is (= [1920 1080] (:size spec)))
      (is (= 30 (:fps spec)))
      ;; Frame review rule: the wrap N-1 -> 0 is no larger than an ordinary
      ;; step, measured at the native frame count.
      (let [{:keys [seam typical-max]} (metrics/seam-stats spec [96 54])
            {:keys [near-black white-clip]} (metrics/image-stats (r/render-frame spec 0.3 {:size [160 90]}))]
        (is (<= seam typical-max))
        (is (> near-black 0.3))
        (is (< white-clip 0.01))))))
