(ns tessera.vj-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.vj :as vj])
  (:import (java.awt.image BufferedImage)))

(defn pixels ^ints [^BufferedImage image]
  (.getRGB image 0 0 (.getWidth image) (.getHeight image) nil 0 (.getWidth image)))

(defn mean-diff
  "Mean absolute RGB channel difference, 0..255."
  [^ints a ^ints b]
  (/ (reduce + (for [i (range (alength a)) shift [0 8 16]]
                 (Math/abs (long (- (bit-and 255 (bit-shift-right (aget a i) shift))
                                    (bit-and 255 (bit-shift-right (aget b i) shift)))))))
     (double (* 3 (alength a)))))

(defn image-stats
  "Rec.709 luminance 0..255, near-black Y<8, lit max(RGB)>32.
   White clip means all channels >=250, not a saturated single channel."
  [^BufferedImage image]
  (let [values (mapv (fn [c]
                       (let [red (bit-and 255 (bit-shift-right c 16))
                             green (bit-and 255 (bit-shift-right c 8))
                             blue (bit-and 255 c)
                             hi (max red green blue) lo (min red green blue)]
                         {:y (+ (* 0.2126 red) (* 0.7152 green) (* 0.0722 blue))
                          :v hi :s (if (pos? hi) (/ (- hi lo) (double hi)) 0.0)
                          :white? (>= lo 250)})) (pixels image))
        n (double (count values))
        mean (/ (reduce + (map :y values)) n)
        lit (filter #(> (:v %) 32) values)]
    {:mean mean
     :std (Math/sqrt (/ (reduce + (map #(let [d (- (:y %) mean)] (* d d)) values)) n))
     :near-black (/ (count (filter #(< (:y %) 8.0) values)) n)
     :lit-saturation (if (seq lit) (/ (reduce + (map :s lit)) (count lit)) 0.0)
     :white-clip (/ (count (filter :white? values)) n)
     :peak-channel (apply max (map :v values))}))

(defn seam-stats
  "Wrap versus distributed consecutive samples at the actual spec frame count."
  ([spec] (seam-stats spec [160 90]))
  ([spec size]
   (let [n (long (* (:fps spec) (:seconds spec)))
         frame (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size size})))
         steps (mapv #(let [i (long (* % n))]
                        (mean-diff (frame i) (frame (inc i))))
                     [0.08 0.21 0.36 0.53 0.71 0.89])]
     {:frames n
      :seam (mean-diff (frame (dec n)) (frame 0))
      :typical-max (apply max steps)
      :typical-mean (/ (reduce + steps) (count steps))})))

(deftest stepped-cross-has-exact-geometry
  (is (neg? (vj/stepped-cross 0.0 0.0)))
  (is (zero? (vj/stepped-cross 0.39 0.0)))
  (is (zero? (vj/stepped-cross 0.27 0.27)))
  (is (pos? (vj/stepped-cross 0.4 0.4)))
  (doseq [[x y] [[0.31 0.11] [0.22 -0.29] [-0.42 0.08]]]
    (is (= (vj/stepped-cross x y) (vj/stepped-cross (- y) x)))))

(deftest cylindrical-rays-and-closed-texture-periods
  (doseq [[x y] [[1.3 0.5] [0.4 -0.7] [-2.1 1.2]]]
    (let [[s t z] (vj/tunnel-coordinates x y 0.3)
          [s1 t1 z1] (vj/tunnel-coordinates x y 1.3)]
      (is (< (Math/abs (- (* z (Math/hypot x y)) 3.0)) 1.0e-12))
      (is (< (Math/abs (- s s1)) 1.0e-12))
      (is (< (Math/abs (- t t1 1.0)) 1.0e-12))
      (is (= z z1)))))

(deftest every-vj-loop-is-black-ground-saturated-and-seamless
  (doseq [[id spec] vj/loops]
    (testing (name id)
      (let [{:keys [seam typical-max]} (seam-stats spec)
            {:keys [mean std near-black lit-saturation white-clip]}
            (image-stats (r/render-frame spec 0.3 {:size [240 135]}))]
        (is (= id (:id spec)))
        (is (fn? (:field spec)))
        (is (<= seam (+ 0.5 (* 1.5 typical-max))))
        (is (> mean 5.0))
        (is (> std 10.0))
        (is (> near-black 0.15))
        (is (> lit-saturation 0.75))
        (is (zero? white-clip))))))
