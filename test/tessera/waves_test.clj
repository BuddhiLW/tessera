(ns tessera.waves-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.field :as fld]
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

(deftest fields-are-valid-and-exactly-periodic
  (doseq [[k spec] waves/loops]
    (testing (name k)
      (let [bf ((:field spec) 0.37)]
        (is (fld/bound? bf))
        (is (seq (:bindings (fld/lower (:field/form bf)))))
        (doseq [x [-7.0 -2.3 0.0 3.1] y [-3.0 0.0 2.7]]
          (let [a (fld/jvm-shader ((:field spec) 0.37))
                b (fld/jvm-shader ((:field spec) 1.37))]
            (is (= (a x y) (b x y)))))))))

(deftest caustic-jacobian-matches-numerical-ray-map
  (doseq [[x y] [[1.23 -0.87] [-2.4 1.1] [0.31 2.6]]]
    (let [{:keys [det point]} (waves/water-ray 0.31 x y)
          [px py] point
          epsilon 1.0e-5
          [qx qy] (:point (waves/water-ray 0.31 (+ x epsilon) y))
          [rx ry] (:point (waves/water-ray 0.31 x (+ y epsilon)))
          jxx (/ (- qx px) epsilon) jyx (/ (- qy py) epsilon)
          jxy (/ (- rx px) epsilon) jyy (/ (- ry py) epsilon)
          numerical (- (* jxx jyy) (* jxy jyx))]
      (is (< (Math/abs (- det numerical)) 1.0e-5)
          (str "analytic " det ", finite difference " numerical)))))

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
