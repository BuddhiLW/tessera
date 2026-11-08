(ns tessera.lattice-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [tessera.lattice :as lattice])
  (:import (java.awt.image BufferedImage)))

(defn pixels ^ints [^BufferedImage image]
  (.getRGB image 0 0 (.getWidth image) (.getHeight image) nil 0 (.getWidth image)))

(defn mean-diff
  "Mean absolute RGB-channel difference on the 0..255 scale."
  [^ints a ^ints b]
  (/ (reduce + (for [i (range (alength a)) s [0 8 16]]
                 (Math/abs (long (- (bit-and (bit-shift-right (aget a i) s) 255)
                                    (bit-and (bit-shift-right (aget b i) s) 255))))))
     (double (* 3 (alength a)))))

(defn luminance-stats
  "Rec.709 luminance in 0..255; near-black means luminance <8."
  [^ints image]
  (let [values (mapv (fn [c]
                       (+ (* 0.2126 (bit-and (bit-shift-right c 16) 255))
                          (* 0.7152 (bit-and (bit-shift-right c 8) 255))
                          (* 0.0722 (bit-and c 255)))) image)
        n (double (count values)) mean (/ (reduce + values) n)]
    {:mean mean
     :std (Math/sqrt (/ (reduce + (map #(let [d (- % mean)] (* d d)) values)) n))
     :near-black (/ (count (filter #(< % 8.0) values)) n)}))

(defn seam-stats
  "Same wrap-vs-consecutive criterion as tessera.loops-test, all post included."
  ([spec] (seam-stats spec [160 90] 60))
  ([spec size n]
   (let [frame (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size size})))
         steps (mapv #(mean-diff (frame %) (frame (inc %))) [5 25 45])]
     {:seam (mean-diff (frame (dec n)) (frame 0))
      :typical-max (apply max steps)
      :typical-mean (/ (reduce + steps) (count steps))})))

(deftest lieb-is-a-square-flat-band-system
  (is (< (Math/abs (- (lattice/lieb-energy 0.0 0.0) (Math/sqrt 8.0))) 1.0e-12))
  (is (< (lattice/lieb-energy Math/PI Math/PI) 1.0e-12))
  (is (= 2.0 (lattice/lieb-energy 0.0 Math/PI)))
  (doseq [[x y] [[0.24 1.42] [-1.8 0.55] [2.1 -0.3]]]
    (is (< (Math/abs (- (lattice/lieb-energy x y) (lattice/lieb-energy y x))) 1.0e-12))
    (let [cx (Math/cos (* 0.5 x)) cy (Math/cos (* 0.5 y))]
      ;; H(k)*(0,-cy,cx) = 0, hence a truly flat band, not a raised square band.
      (is (< (Math/abs (+ (* 2.0 cx (- cy)) (* 2.0 cy cx))) 1.0e-12)))))

(deftest bloch-probability-is-conserved-and-revives
  (let [seed (lattice/bloch-seed 1.0 1.2)]
    (doseq [u [0.0 0.13 0.3 0.57 0.81] cycles [1 2]]
      (let [a (vec (lattice/bloch-density seed u cycles 4.8 24))
            b (vec (lattice/bloch-density seed (+ u 1.0) cycles 4.8 24))]
        (is (every? #(and (Double/isFinite %) (not (neg? %))) a))
        (is (< (Math/abs (- 1.0 (reduce + a))) 1.0e-9))
        (is (< (apply max (map #(Math/abs (- %1 %2)) a b)) 1.0e-12))))))

(deftest bloch-center-obeys-the-acceleration-theorem
  (let [k0 1.1 seed (lattice/bloch-seed 1.0 k0)
        phi ^doubles (:phi seed) qs ^doubles (:q seed) m (alength qs)
        avg (fn [f] (/ (reduce + (for [i (range m)]
                                   (* (+ (Math/pow (aget phi (* 2 i)) 2.0)
                                         (Math/pow (aget phi (inc (* 2 i))) 2.0))
                                      (f (aget qs i))))) m))]
    (doseq [u [0.12 0.3 0.6 0.83]]
      (let [theta (* lp/tau u)
            density (lattice/bloch-density seed u 1 4.8 24)
            center (reduce + (map * (range -24 25) density))
            predicted (* 4.8 (avg #(- (Math/cos (- % theta)) (Math/cos %))))]
        (is (< (Math/abs (- center predicted)) 1.0e-8))))))

(deftest harper-has-known-zero-and-half-flux-spectra
  (let [zero (sort (lattice/harper-edges 0 1))
        half (sort (lattice/harper-edges 1 2))]
    (is (= [-4.0 4.0] zero))
    (is (< (Math/abs (+ (first half) (Math/sqrt 8.0))) 1.0e-12))
    (is (< (Math/abs (second half)) 1.0e-12))
    (is (< (Math/abs (nth half 2)) 1.0e-12))
    (is (< (Math/abs (- (last half) (Math/sqrt 8.0))) 1.0e-12)))
  (is (thrown? clojure.lang.ExceptionInfo (lattice/harper-edges 1 0)))
  (doseq [[p q] [[1 3] [2 5] [3 7] [4 9]]]
    (let [a (sort (lattice/harper-edges p q))
          b (sort (lattice/harper-edges (- q p) q))]
      (is (= (* 2 q) (count a)))
      (is (every? #(<= -4.000001 % 4.000001) a))
      (is (< (apply max (map #(Math/abs (- %1 %2)) a b)) 1.0e-10)))))

(deftest every-lattice-loop-wraps-without-a-visible-seam
  (doseq [[id spec] lattice/loops]
    (testing (name id)
      (let [{:keys [seam typical-max]} (seam-stats spec)]
        (is (<= seam (+ 1.0 (* 1.5 typical-max))))
        (is (= id (:id spec)))
        (is (fn? (:field spec)))
        (is (not (contains? spec :scene)))))))
