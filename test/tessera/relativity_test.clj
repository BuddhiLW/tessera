(ns tessera.relativity-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as render]
            [tessera.relativity :as rel])
  (:import (java.awt.image BufferedImage)))

(defn- pixels ^ints [^BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(defn- mean-diff [^ints a ^ints b]
  (let [ch (fn [p s] (bit-and (bit-shift-right p s) 0xFF))]
    (/ (double (reduce + (for [i (range (alength a)) s [0 8 16]]
                          (Math/abs (- (ch (aget a i) s) (ch (aget b i) s))))))
       (* 3 (alength a)))))

(defn- frame [spec n i]
  (pixels (render/render-frame spec (/ (double i) n) {:size [240 135]})))

(deftest every-relativity-loop-has-an-invisible-seam
  ;; At the loop's native frame rate the wrap must be no bigger than a
  ;; typical adjacent-frame step. Test both sides, not just u=0 vs u=1.
  (doseq [[id spec] rel/loops]
    (testing (name id)
      (let [n (long (* (:seconds spec) (:fps spec)))
            frames (mapv (partial frame spec n) [0 1 (dec n) (- n 2)])
            [f0 f1 last prev] frames
            seam (mean-diff last f0)
            step (max (mean-diff f0 f1) (mean-diff prev last))]
        (is (<= seam step) (str "seam=" seam ", adjacent step=" step))))))

(deftest boosts-preserve-the-minkowski-interval-and-light-cones
  (doseq [p [[0.4 1.0] [1.0 1.0] [-1.0 1.0] [-0.8 0.3]]
          beta [-0.8 0.0 0.8]]
    (let [[x ct] (apply rel/boost [p beta])]
      (is (< (Math/abs (- (* ct ct) (* x x)
                          (- (* (second p) (second p)) (* (first p) (first p)))))
             1.0e-12))
      (when (= (Math/abs (first p)) (Math/abs (second p)))
        (is (< (Math/abs (- (Math/abs x) (Math/abs ct))) 1.0e-12))))))

(deftest aberration-is-identity-at-rest-and-finite
  (is (= [0.0 0.0] (rel/aberration-position [0.0 0.0] 0.9)))
  (is (= [1.0 0.0] (rel/aberration-position [1.0 0.0] 0.0)))
  (doseq [p [[0.1 0.1] [2.0 -1.0]] beta [-0.9 0.0 0.9]]
    (is (every? #(Double/isFinite (double %)) (rel/aberration-position p beta)))))

(deftest twin-tick-light-is-continuous-at-the-seam
  (let [opaque (fn [u] (mapv #(select-keys % [:from :to :opacity])
                             (filter #(= :line (:node %))
                                     (drop 480 (:shapes (rel/twin-frame u))))))
        a (opaque 0.0) b (opaque 1.0)]
    (is (= (count a) (count b)))
    (doseq [[p q] (map vector a b)]
      (is (= (dissoc p :opacity) (dissoc q :opacity)))
      (is (< (Math/abs (- (:opacity p) (:opacity q))) 1.0e-12)))))

(deftest aberration-forward-map-agrees-with-field-inverse
  (doseq [p [[0.3 -0.5] [1.7 0.8]] beta [-0.8 0.0 0.8]
          :let [[sx sy] p
                source-length (Math/sqrt (+ 1.0 (* sx sx) (* sy sy)))]
          :when (pos? (- 1.0 (* beta source-length)))]
    (let [[x y] (rel/aberration-position p beta)
          gamma (/ 1.0 (Math/sqrt (- 1.0 (* beta beta))))
          ray-length (Math/sqrt (+ 1.0 (* x x) (* y y)))]
      (is (< (Math/abs (- sx (/ x (* gamma (+ 1.0 (* beta ray-length)))))) 1.0e-12))
      (is (< (Math/abs (- sy (/ y (* gamma (+ 1.0 (* beta ray-length)))))) 1.0e-12)))))

(deftest twin-clocks-tick-by-proper-time-not-coordinate-time
  (is (every? #(<= 0.0 % 1.0) rel/twin-clock-phases))
  (is (apply < rel/twin-clock-phases))
  (is (< (Math/abs (rel/traveler-x 0.0)) 1.0e-12))
  (is (< (Math/abs (- (rel/traveler-x 1.0))) 1.0e-12))
  (is (< rel/twin-proper-total 1.0)))
