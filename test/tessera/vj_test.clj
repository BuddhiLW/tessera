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
  "Wrap versus distributed consecutive samples at the actual spec frame count.
   `samples` evenly spaced ordinary steps estimate the typical maximum."
  ([spec] (seam-stats spec [160 90]))
  ([spec size] (seam-stats spec size [0.08 0.21 0.36 0.53 0.71 0.89]))
  ([spec size fractions]
   (let [n (long (* (:fps spec) (:seconds spec)))
         frame (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size size})))
         steps (mapv #(let [i (long (* % n))]
                        (mean-diff (frame i) (frame (inc i))))
                     fractions)]
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

(deftest dihedral-fold-is-the-d8-quotient
  (doseq [[x y] [[0.6 1.1] [-0.8 0.31] [1.7 -0.42]]]
    (let [[a b] (vj/dihedral-fold 8 x y)
          theta (/ Math/PI 4.0)
          xr (- (* x (Math/cos theta)) (* y (Math/sin theta)))
          yr (+ (* x (Math/sin theta)) (* y (Math/cos theta)))
          folded-turn (vj/dihedral-fold 8 xr yr)
          folded-reflection (vj/dihedral-fold 8 x (- y))]
      (is (< (Math/abs (- (Math/hypot a b) (Math/hypot x y))) 1.0e-12))
      (is (<= 0.0 (Math/atan2 b a) (/ Math/PI 8.0)))
      (doseq [[p q] [[folded-turn [a b]] [folded-reflection [a b]]]]
        (is (< (apply max (map #(Math/abs (- %1 %2)) p q)) 1.0e-12)))))
  (is (thrown? clojure.lang.ExceptionInfo (vj/dihedral-fold 1 0.2 0.1)))
  (let [f (vj/kaleido-shader 0.3)]
    (doseq [[x y] [[0.6 1.1] [-0.8 0.31] [1.7 -0.42]]]
      (is (= (f x y) (f x (- y)))))))

(deftest sprite-size-driver-is-the-honeycomb-band
  (is (= 3.0 (vj/graphene-amplitude 0.0 0.0)))
  (is (< (vj/graphene-amplitude (/ (* 2.0 Math/PI) 3.0) (/ (* -2.0 Math/PI) 3.0)) 1.0e-12))
  (doseq [[a b] [[0.31 1.27] [-0.8 2.1] [2.8 -0.4]]]
    (let [e (vj/graphene-amplitude a b)
          squared (+ 3.0 (* 2.0 (Math/cos a)) (* 2.0 (Math/cos b)) (* 2.0 (Math/cos (- a b))))]
      (is (<= 0.0 e 3.0))
      (is (< (Math/abs (- (* e e) squared)) 1.0e-12))
      (is (< (Math/abs (- e (vj/graphene-amplitude (+ a (* 2.0 Math/PI)) b))) 1.0e-12))))
  (let [a (vj/sprite-shader 0.0) b (vj/sprite-shader 1.0)]
    (doseq [[x y] [[0.73 0.29] [-2.31 1.01] [3.74 -1.88]]]
      (is (= (a x y) (b x y))))))

(deftest gallery-specs-preserve-the-approved-render-contract
  (is (= [:textile-tunnel :kaleido-quasicrystal :sprite-lattice] (vec (keys vj/loops))))
  (doseq [[id spec] vj/loops]
    (testing (name id)
      (is (= [1920 1080] (:size spec)))
      (is (= 30 (:fps spec)))
      (is (>= (:ss spec) 3))
      (is (not (contains? spec :scene)))
      (doseq [u [0.0 0.17 0.3 0.71]
              :let [a ((:field spec) u) b ((:field spec) (+ u 1.0))]
              [x y] [[0.73 0.29] [-2.31 1.01] [3.74 -1.88]]]
        (let [c (a x y)]
          (is (= c (b x y)))
          (is (< (apply max (map #(bit-and 255 (bit-shift-right c %)) [0 8 16])) 250)))))))

(deftest every-vj-loop-is-black-ground-saturated-and-seamless
  (doseq [[id spec] vj/loops]
    (testing (name id)
      (let [{:keys [seam typical-max]} (seam-stats spec)
            {:keys [mean std near-black lit-saturation white-clip]}
            (image-stats (r/render-frame spec 0.3 {:size [240 135]}))]
        (is (= id (:id spec)))
        (is (fn? (:field spec)))
        ;; Strict task criterion: no slack multiplier or additive tolerance.
        (is (<= seam typical-max))
        (is (> mean 5.0))
        (is (> std 10.0))
        (is (> near-black 0.15))
        (is (> lit-saturation 0.75))
        (is (zero? white-clip))))))
