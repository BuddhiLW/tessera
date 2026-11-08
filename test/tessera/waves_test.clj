(ns tessera.waves-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.waves :as waves])
  (:import (java.awt.image BufferedImage)))

(defn- pixels ^ints [^BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(defn- mean-diff [^ints a ^ints b]
  (let [ch (fn [p s] (bit-and (bit-shift-right p s) 0xFF))]
    (/ (double (reduce + (for [i (range (alength a)) s [0 8 16]]
                           (Math/abs (- (ch (aget a i) s) (ch (aget b i) s))))))
       (* 3 (alength a)))))

(deftest every-loop-wraps-without-a-visible-seam
  (doseq [[k spec] waves/loops]
    (testing (name k)
      (let [n 60
            f (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size [160 90]})))
            typical (apply max (map #(mean-diff (f %) (f (inc %))) [5 25 45]))
            seam (mean-diff (f (dec n)) (f 0))]
        (is (<= seam (+ 1.0 (* 1.5 typical))))))))

(deftest fields-are-exactly-periodic
  (doseq [[k spec] waves/loops
          [x y] [[-7.0 -3.0] [-2.3 0.0] [0.0 2.7] [3.1 -0.2]]]
    (testing (name k)
      (let [a ((:field spec) 0.37) b ((:field spec) 1.37)]
        (is (= (a x y) (b x y)))))))

(deftest photon-splatting-conserves-light
  (let [^doubles density (waves/caustic-buffer 0.31 96 96 10.0)]
    (is (< (Math/abs (- 1.0 (/ (areduce density i sum 0.0 (+ sum (aget density i)))
                               (alength density)))) 1.0e-10)))
  (is (every? #(< % 1.0e-12)
              (map (fn [a b] (Math/abs (- a b)))
                   (waves/water-gradient 0.31 1.2 -0.7)
                   (waves/water-gradient 0.31 9.2 -0.7)))))

(deftest exact-breather-and-water-periodicity
  (doseq [n waves/breather-rows
          u [0.0 0.071 0.31 0.77]
          x [-6.0 -1.2 0.0 3.4]]
    (is (< (Math/abs (- (waves/breather-value u n x)
                        (waves/breather-value (inc u) n x))) 1.0e-12)))
  (is (every? #(< % 1.0e-12)
              (map (fn [a b] (Math/abs (- (double a) (double b))))
                   (mapcat identity (waves/water-phases 0.31))
                   (mapcat identity (waves/water-phases 1.31))))))
