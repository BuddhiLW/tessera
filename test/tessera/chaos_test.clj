(ns tessera.chaos-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.chaos :as chaos])
  (:import (java.awt.image BufferedImage)))

(defn- pixels ^ints [^BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(defn- mean-diff [^ints a ^ints b]
  (let [ch (fn [p s] (bit-and (bit-shift-right p s) 0xFF))]
    (/ (double (reduce + (for [i (range (alength a)) s [0 8 16]]
                           (Math/abs (- (ch (aget a i) s) (ch (aget b i) s))))))
       (* 3 (alength a)))))

(deftest every-loop-wraps-without-a-visible-seam
  (doseq [[k spec] chaos/loops]
    (testing (name k)
      (let [n 60
            f (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size [160 90]})))
            typical (apply max (map #(mean-diff (f %) (f (inc %))) [5 25 45]))
            seam (mean-diff (f (dec n)) (f 0))]
        (is (<= seam (+ 1.0 (* 1.5 typical))))))))

(deftest cat-map-is-exactly-periodic
  (is (= 24 (chaos/cat-period chaos/cat-n)))
  (testing "24 cat steps (48 half-steps) return every lattice point; 12 reflect it"
    (doseq [[x y] [[0 0] [1 0] [17 99] [321 321] [72 5]]]
      (is (= [x y] (chaos/cat-state chaos/cat-n 48 x y)))
      (is (= [(mod (- x) chaos/cat-n) (mod (- y) chaos/cat-n)] (chaos/cat-state chaos/cat-n 24 x y)))))
  (testing "the khatam picture is centrally symmetric, so the half-loop image is whole"
    (doseq [[x y] [[0.1 0.2] [0.37 0.81] [0.5 0.05]]]
      (is (= (chaos/khatam x y) (chaos/khatam (- 1.0 x) (- 1.0 y))))))
  (testing "positions at u and u + 1 agree (to the last bits of frac)"
    (doseq [u [0.0 0.07 0.31 0.77] [x y] [[3 4] [100 20]]]
      (is (every? #(< (Math/abs (double %)) 1e-9)
                  (map - (chaos/cat-position chaos/cat-n 24 x y u)
                       (chaos/cat-position chaos/cat-n 24 x y (+ u 1.0))))))))

(deftest billiard-rays-close
  (let [[a b] chaos/billiard-table]
    (testing "every family's rays return to their start after q bounces"
      (doseq [fam chaos/billiard-families
              :let [lam (chaos/family-caustic a b fam)]
              th [0.3 1.7 2.9]
              :when (< (chaos/return-error a b lam (:q fam) th) 1.0)]
        (is (< (chaos/return-error a b lam (:q fam) th) 1e-8))))
    (testing "the hyperbolic family lies between b^2 and a^2"
      (let [lam (chaos/family-caustic a b (last chaos/billiard-families))]
        (is (< (* b b) lam (* a a)))))))

(deftest standard-map-classifies-orbits
  (let [kind (fn [x p] (:kind (chaos/rotation (chaos/standard-orbit chaos/standard-k x p 16384))))]
    (is (= :island (kind Math/PI 0.4)))
    (is (= :torus (kind 0.0 2.8)))))
