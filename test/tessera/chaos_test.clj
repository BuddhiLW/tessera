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
  (is (= 12 (chaos/cat-period chaos/cat-n)))
  (testing "twelve cat steps (24 half-steps) return every lattice point"
    (doseq [[x y] [[0 0] [1 0] [17 99] [143 143] [72 5]]]
      (is (= [x y] (chaos/cat-state chaos/cat-n 24 x y)))))
  (testing "positions at u and u + 1 agree exactly"
    (doseq [u [0.0 0.07 0.31 0.77] [x y] [[3 4] [100 20]]]
      (is (= (chaos/cat-position chaos/cat-n 12 x y u)
             (chaos/cat-position chaos/cat-n 12 x y (+ u 1.0)))))))
