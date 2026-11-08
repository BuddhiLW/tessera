(ns tessera.compose.modulate
  "Modulators: mathematical fields and loop-periodic timing that drive a
   layer's size, colour, delay or visibility.

   Field nodes write a scalar slot per pixel (plane-wave quasicrystal,
   graphene band, periodic noise). Timing is written as expressions over
   :u (the :phase node) and is periodic by construction: integer beats
   wave(k u), bleuje delays f(u - d(x)) with an integer-period f, and noise
   sampled on a circle in u. The helpers below return EDN expressions, so
   a scene that uses them is still data."
  (:require [desargues.wallpaper.loop :as lp]
            [tessera.compose.core :as c])
  (:import (tessera.compose.core IStep)))

(set! *warn-on-reflection* true)

(defmethod c/node :plane-waves
  ;; Quasicrystal: scale * sum_j cos(k (p . e_j) + tau speed u), e_j at
  ;; angles tau j/n (:dirs :full) or pi j/n (:half). speed is an integer.
  [{:keys [n k scale speed dirs in out] :or {scale 1.0 speed 1 dirs :full in [:px :py] out :field}} slot]
  (let [n (long n) k (double k) scale (double scale) speed (double speed)
        turn (if (= dirs :half) Math/PI lp/tau)
        e (double-array (mapcat (fn [j] [(Math/cos (* turn (/ (double j) n)))
                                         (Math/sin (* turn (/ (double j) n)))]) (range n)))
        [sx sy] in ix (long (slot sx)) iy (long (slot sy)) io (long (slot out))]
    (fn [u]
      (let [phase (* lp/tau (* speed (double u)))]
        (reify IStep
          (run [_ c]
            (let [px (aget c ix) py (aget c iy)
                  sum (loop [j 0 sum 0.0]
                        (if (< j n)
                          (recur (inc j)
                                 (+ sum (Math/cos (+ (* k (+ (* px (aget e (* 2 j)))
                                                             (* py (aget e (inc (* 2 j))))))
                                                     phase))))
                          sum))]
              (aset c io (* scale (double sum))))))))))

(defn graphene-amplitude
  "Honeycomb nearest-neighbour structure factor |1 + e^{i k1} + e^{i k2}|,
   in [0, 3], with a Dirac zero at (2pi/3, -2pi/3)."
  ^double [^double k1 ^double k2]
  (let [re (+ 1.0 (Math/cos k1) (Math/cos k2))
        im (+ (Math/sin k1) (Math/sin k2))]
    (Math/hypot re im)))

(defmethod c/node :graphene
  ;; |f(k)| of the tight-binding band at k1 = a.p + tau u, k2 = b.p - tau u.
  [{:keys [a b in out speed] :or {in [:x :y] out :band speed 1}} slot]
  (let [[a1 a2] (map double a) [b1 b2] (map double b) speed (double speed)
        [sx sy] in ix (long (slot sx)) iy (long (slot sy)) io (long (slot out))]
    (fn [u]
      (let [phase (* lp/tau (* speed (double u)))]
        (reify IStep
          (run [_ c]
            (let [x (aget c ix) y (aget c iy)]
              (aset c io (graphene-amplitude (+ (* a1 x) (* a2 y) phase)
                                             (- (+ (* b1 x) (* b2 y)) phase))))))))))

(defn- hash01
  "Deterministic lattice hash in [0, 1)."
  ^double [^long i ^long j ^long seed]
  (let [h (unchecked-multiply (bit-xor (unchecked-multiply i 374761393)
                                       (unchecked-multiply j 668265263)
                                       (unchecked-multiply seed 2147483647)) 1274126177)
        h (bit-xor h (unsigned-bit-shift-right h 13))]
    (/ (double (bit-and h 0xFFFFFF)) 16777216.0)))

(defn value-noise
  "Smooth value noise on the plane, period-free, from `seed`."
  ^double [^double x ^double y ^long seed]
  (let [i (long (Math/floor x)) j (long (Math/floor y))
        fx (- x i) fy (- y j)
        sx (* fx fx (- 3.0 (* 2.0 fx))) sy (* fy fy (- 3.0 (* 2.0 fy)))
        a (hash01 i j seed) b (hash01 (inc i) j seed)
        c (hash01 i (inc j) seed) d (hash01 (inc i) (inc j) seed)]
    (+ (* (- 1.0 sy) (+ a (* sx (- b a)))) (* sy (+ c (* sx (- d c)))))))

(defmethod c/node :loop-noise
  ;; Periodic noise: the noise plane sampled at p + R (cos tau u, sin tau u),
  ;; a circle in u, so u + 1 returns the same field exactly.
  [{:keys [freq radius seed in out] :or {freq 1.0 radius 0.6 seed 7 in [:x :y] out :noise}} slot]
  (let [f (double freq) rr (double radius) seed (long seed)
        [sx sy] in ix (long (slot sx)) iy (long (slot sy)) io (long (slot out))]
    (fn [u]
      (let [ox (* rr (Math/cos (* lp/tau (double u)))) oy (* rr (Math/sin (* lp/tau (double u))))]
        (reify IStep
          (run [_ c]
            (aset c io (value-noise (+ (* f (aget c ix)) ox) (+ (* f (aget c iy)) oy) seed))))))))

(defn beat
  "Expression: base + amp wave(k u), k an integer beat count per loop."
  [base amp k]
  [:+ base [:* amp [:wave [:* (double k) :u]]]])

(defn delayed
  "Bleuje delay expression: wave(k (u - d)) with d a per-pixel delay
   expression. Periodic in u for integer k, whatever d is."
  [k d]
  [:wave [:* (double k) [:- :u d]]])

(defn under
  "Mask expression 1 - w s: a layer shown only where `s` is not."
  ([s] [:- 1.0 s])
  ([w s] [:- 1.0 [:* w s]]))

(defn fade-in
  "Expression min(1, k r) (1 - falloff r): centre fade-in, edge falloff."
  [r k falloff]
  [:* [:min 1.0 [:* k r]] [:- 1.0 [:* falloff r]]])
