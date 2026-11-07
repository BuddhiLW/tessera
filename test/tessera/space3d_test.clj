(ns tessera.space3d-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [desargues.wallpaper.space :as sp]
            [tessera.space3d :as s])
  (:import (java.awt.image BufferedImage)))

(defn- pixels ^ints [^BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(defn- mean-diff [^ints a ^ints b]
  (let [ch (fn [p shift] (bit-and (bit-shift-right p shift) 0xFF))]
    (/ (double (reduce + (for [i (range (alength a)) shift [0 8 16]]
                           (Math/abs (- (ch (aget a i) shift)
                                        (ch (aget b i) shift))))))
       (* 3 (alength a)))))

(deftest every-space-loop-wraps-without-a-visible-seam
  (doseq [[k spec] s/loops]
    (testing (name k)
      (let [n 60
            f (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size [160 90]})))
            typical (apply max (map #(mean-diff (f %) (f (inc %))) [5 25 45]))
            seam (mean-diff (f (dec n)) (f 0))]
        (is (<= seam (+ 1.0 (* 1.5 typical))))))))

(deftest world-curves-close-and-breathing-repeats
  (let [err (fn [a b] (apply max (map #(Math/abs (- %1 %2)) a b)))
        phases [0.0 0.17 0.43 0.81]]
    (doseq [f s/hopf-fibres u phases t phases]
      (is (< (err (s/hopf-point f t u) (s/hopf-point f t (+ u 1.0))) 1e-10))
      (is (< (err (s/hopf-point f t u) (s/hopf-point f (+ t 1.0) u)) 1e-10)))
    (doseq [[point curves] [[s/knot-point s/knot-garden]
                           [s/lissajous-point s/lissajous-lattice]]
            c curves t phases]
      (is (< (err (point c t) (point c (+ t 1.0))) 1e-10)))))

(deftest hopf-framing-clears-edges-and-near-plane
  (let [projected (vec (for [u [0.0 0.125 0.25 0.375 0.5 0.625 0.75 0.875]
                            fibre s/hopf-fibres j (range 96)]
                        (sp/project s/hopf-camera
                                    (s/hopf-point fibre (/ (double j) 96.0) u))))
        xs (map first projected) ys (map second projected)]
    (is (every? some? projected))
    (is (every? (fn [[x y d]] (and (< -6.8 x 6.8) (< -3.8 y 3.8)
                                    (> d (:near s/hopf-camera)))) projected))
    (is (> (- (apply max ys) (apply min ys)) 6.5))
    (is (> (- (apply max xs) (apply min xs)) 10.0))
    (is (< (Math/abs (+ (apply max xs) (apply min xs))) 0.2))
    (is (< (Math/abs (+ (apply max ys) (apply min ys))) 0.35))))

(deftest fixed-gardens-fit-and-only-light-moves
  (doseq [[point curves] [[s/knot-point s/knot-garden]
                         [s/lissajous-point s/lissajous-lattice]]]
    (is (every? (fn [[x y d]] (and (< -6.95 x 6.95) (< -3.6 y 3.6) (> d 0.1)))
                (for [c curves j (range 96)]
                  (sp/project s/field-camera (point c (/ (double j) 96.0)))))))
  (doseq [spec [s/torus-knots s/lissajous-3d]]
    (is (= ((:frame spec) 0.0) ((:frame spec) 0.37) ((:frame spec) 0.81)))
    (is (not= ((:splat spec) 0.0) ((:splat spec) 0.37))))
  (is (some #(= :dof (:type %)) ((:post s/torus-knots) 0.0))))

(deftest lissajous-cells-are-separated-and-colour-families-are-periodic
  (is (= 12 (count s/lissajous-lattice)
         (count (set (map :freq s/lissajous-lattice)))))
  (doseq [{:keys [row col] :as c} s/lissajous-lattice]
    (let [cx (* 3.45 (- col 1.5)) cy (* 2.55 (- row 1.0))]
      (is (every? (fn [[x y _]] (and (< (Math/abs (- x cx)) 1.4)
                                     (< (Math/abs (- y cy)) 1.1)))
                  (for [j (range 128)]
                    (sp/project s/field-camera (s/lissajous-point c (/ j 128.0))))))))
  (doseq [p (vals s/flow-palettes) phase [0.0 0.17 0.61]]
    (is (= (s/pigment p phase) (s/pigment p (+ phase 1.0)))))
  (is (= 6 (count s/knot-garden)))
  (is (= #{:teal-ice :gold-ember :violet-rose} (set (map :palette s/knot-garden)))))
