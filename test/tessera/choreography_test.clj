(ns tessera.choreography-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.choreography :as c])
  (:import (java.awt.image BufferedImage)))

(defn- pixels ^ints [^BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))
(defn- mean-diff [^ints a ^ints b]
  (let [ch (fn [p s] (bit-and (bit-shift-right p s) 0xFF))]
    (/ (double (reduce + (for [i (range (alength a)) s [0 8 16]]
                           (Math/abs (- (ch (aget a i) s) (ch (aget b i) s))))))
       (* 3 (alength a)))))

(deftest every-loop-wraps-without-a-visible-seam
  (doseq [[k spec] c/loops]
    (testing (name k)
      (let [n 60
            f (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size [160 90]})))
            typical (apply max (map #(mean-diff (f %) (f (inc %))) [5 25 45]))
            seam (mean-diff (f (dec n)) (f 0))]
        (is (<= seam (+ 1.0 (* 1.5 typical))))))))

(deftest closed-orbit-retains-hamiltonian
  (let [states @c/figure-orbit es (map c/energy states)]
    (is (= (first states) (peek states)))
    (is (< (- (apply max es) (apply min es)) 1.0e-5))
    (doseq [u [0.0 0.123 0.499 0.999]]
      (is (every? #(< (Math/abs (double %)) 1.0e-12)
                  (map - (c/sample-orbit states u) (c/sample-orbit states (+ u 1.0))))))))

(deftest distinct-atlas-orbits-close-before-cosmetic-correction
  (is (= [:butterfly-i :moth-i :yin-yang-1a] (mapv :id c/atlas-seeds)))
  (doseq [{:keys [id states closure-error parameters guess]} @c/atlas-orbits]
    (testing (name id)
      (is (< closure-error 1.0e-6))
      (is (= (first states) (peek states)))
      (is (< (c/residual-norm (mapv - parameters guess)) 0.01))
      (let [es (map c/energy states)]
        ;; Butterfly's near encounters amplify a nanometre closure correction.
        ;; This is still <2.5e-5 relative to |H|, including the linear correction.
        (is (< (- (apply max es) (apply min es)) 5.0e-5)))
      (doseq [u [0.0 0.129 0.999]]
        (is (< (c/residual-norm (mapv - (c/sample-orbit states u)
                                       (c/sample-orbit states (+ u 1.0)))) 1.0e-10))))))

(deftest interpolation-survives-roundoff-at-negative-zero
  (doseq [u [-1.0e-20 -0.25 0.0 1.0 2.5]]
    (is (= 12 (count (c/sample-orbit @c/figure-orbit u))))))

(deftest yoshida-implementation-and-newton-solver
  (let [h 0.003
        expected (c/yoshida c/initial-state h)
        actual (c/integrate c/initial-state h 1 false)]
    (is (< (c/residual-norm (mapv - expected actual)) 1.0e-14)))
  (is (thrown? clojure.lang.ExceptionInfo (c/pendulum-orbit 1.0 1)))
  (is (thrown? clojure.lang.ExceptionInfo (c/pendulum-orbit -1.0 1))))

(deftest pendulum-level-sets-and-integer-angle-wrap
  (doseq [e [-0.9 0.0 0.98 1.025 2.5] direction [-1 1]
          :let [orbit (c/pendulum-orbit e direction)]
          u [0.0 0.13 0.27 0.51 0.99]]
    (let [p (c/pendulum-point orbit u) p1 (c/pendulum-point orbit (+ u 3.0))]
      (is (< (Math/abs (- e (c/pendulum-energy p))) 2.0e-5))
      (is (every? #(< (Math/abs (double %)) 1.0e-10) (map - p p1))))))

(deftest pendulum-angle-follows-hamiltons-equations
  (doseq [e [-0.8 0.0 0.98 1.025 2.5] direction [-1 1]
          :let [orbit (c/pendulum-orbit e direction)
                u 0.137 h 1.0e-5
                [q p] (c/pendulum-point orbit u)
                [q1 p1] (c/pendulum-point orbit (+ u h))
                dt (* h (:period orbit))]]
    (is (< (Math/abs (- p (/ (- q1 q) dt))) 0.01))
    (is (< (Math/abs (+ (Math/sin q) (/ (- p1 p) dt))) 0.01))))
