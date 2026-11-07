(ns tessera.mechanics-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.mechanics :as m])
  (:import (java.awt.image BufferedImage)))

(defn- pixels ^ints [^BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(defn- mean-diff [^ints a ^ints b]
  (let [ch (fn [p s] (bit-and (bit-shift-right p s) 0xFF))]
    (/ (double (reduce + (for [i (range (alength a)) s [0 8 16]]
                           (Math/abs (- (ch (aget a i) s) (ch (aget b i) s))))))
       (* 3 (alength a)))))

(deftest every-loop-wraps-without-a-visible-seam
  (doseq [[k spec] m/loops]
    (testing (name k)
      (let [n 60
            f (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size [160 90]})))
            typical (apply max (map #(mean-diff (f %) (f (inc %))) [5 25 45]))
            seam (mean-diff (f (dec n)) (f 0))]
        (is (<= seam (+ 1.0 (* 1.5 typical))))))))

(deftest pendulums-swing-their-target-counts
  (let [{:keys [n base seconds th0 g]} m/wave-params]
    (is (= (range base (+ base n)) (map :swings @m/pendulums)))
    (doseq [{:keys [k swings l swing]} @m/pendulums]
      (testing (str "pendulum " k)
        ;; the exact period times the swing count is the loop
        (is (< (Math/abs (- (* swings (m/pendulum-period g l th0)) seconds)) 1e-9))
        ;; the integrated motion agrees: back at th0 at rest after one period,
        ;; at -th0 at rest after half of one
        (let [[th w] (m/swing-at swing 0.5)
              ths ^doubles (:th swing)
              end (aget ths (dec (alength ths)))]
          (is (< (Math/abs (+ th th0)) 1e-6))
          (is (< (Math/abs w) 1e-6))
          (is (< (Math/abs (- end th0)) 1e-6)))))))

(deftest small-angle-period-would-not-close
  ;; at th0 the small-angle period is off by enough to break the loop
  (let [{:keys [th0 g]} m/wave-params
        {:keys [swings l]} (last @m/pendulums)
        small (* 2.0 Math/PI (Math/sqrt (/ l g)))]
    (is (> (Math/abs (- (* swings small) (:seconds m/wave-params))) 0.5))))

(deftest keplers-equation
  (doseq [e [0.0 0.2 0.45 0.9] M [0.1 1.0 3.0 5.5]]
    (let [E (m/eccentric-anomaly M e)]
      (is (< (Math/abs (- (- E (* e (Math/sin E))) M)) 1e-12)))))

(deftest bodies-return-after-one-loop
  (doseq [o m/orbits]
    (is (every? #(< (Math/abs %) 1e-9) (map - (m/body-at o 0.0) (m/body-at o 1.0))))))

(deftest oscillators-solve-their-lagrange-equations
  (doseq [[ws] m/bodies]
    (is (every? #(= "0" (str %)) (seq (m/oscillator-equations ws)))))
  (doseq [b m/bodies]
    (is (every? #(< (Math/abs %) 1e-9) (map - (m/oscillator-at b 0.0) (m/oscillator-at b 1.0))))))
