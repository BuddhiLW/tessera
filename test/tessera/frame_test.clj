(ns tessera.frame-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.chaos :as chaos]
            [tessera.frame :as frame]
            [tessera.loops :as loops]
            [tessera.main :as main])
  (:import (java.awt.image BufferedImage)))

(defn- pixels [^BufferedImage img x0 w]
  (.getRGB img (int x0) 0 (int w) (.getHeight img) nil 0 (int w)))

(deftest shown-view-widens-only-the-long-axis
  (let [v [(/ 128.0 9.0) 8.0]
        [w h] (frame/shown-view v [2560 1080])]
    (is (= v (frame/shown-view v [1920 1080])) "16:9 shows the authored window")
    (is (= 8.0 h) "same vertical span")
    (is (< 18.96 w 18.97) "2560/1080 x 8 units wide")))

(deftest a-field-loop-shows-more-world-at-ultrawide
  ;; same pixel height, 4/3 the width: the renderer keeps pixels per unit, so
  ;; the 16:9 frame is exactly the centre of the ultrawide one, and the
  ;; ultrawide frame spans more world horizontally over the same vertical span
  (let [spec (loops/all :quasicrystal)
        hd (r/render-frame spec 0.3 {:size [192 108]})
        uw (r/render-frame (frame/at-size spec [256 108]) 0.3 {:size [256 108]})
        [hw hh] (frame/shown-view (frame/view-of spec) [192 108])
        [uww uwh] (frame/shown-view (frame/view-of spec) [256 108])]
    (is (= hh uwh))
    (is (> uww hw))
    (is (java.util.Arrays/equals (pixels hd 0 192) (pixels uw 32 192))
        "the 16:9 picture is the ultrawide picture's centre, unstretched")
    (is (not (java.util.Arrays/equals (pixels uw 0 32) (pixels uw 224 32)))
        "the side strips are new world, not padding")))

(deftest sixteen-nine-is-the-authored-spec
  (doseq [[id spec] loops/all]
    (let [at (frame/at-size spec [1920 1080])]
      (is (= spec at) (str id " unchanged at 1920x1080"))
      (doseq [k [:field :frame :splat :light :post :scene]]
        (is (identical? (k spec) (k at)) (str id " " k " is the same fn"))))))

(deftest reframes-keep-identity-and-timing
  (let [reframed (filter (comp :reframe val) loops/all)]
    (is (<= 16 (count reframed)) "the loops laid out for 16:9 carry a reframe")
    (doseq [[id spec] reframed]
      (let [uw (frame/at-size spec frame/ultrawide)]
        (is (= [id (:seconds spec) (:fps spec)] [(:id uw) (:seconds uw) (:fps uw)]) (str id))
        (is (= frame/ultrawide (:size uw)) (str id))
        (is (some uw [:field :frame :splat :scene]) (str id " still draws"))))))

(deftest cat-map-tiles-across-the-ultrawide-width
  (let [spec (frame/at-size (loops/all :cat-map) frame/ultrawide)
        xs (map first ((:splat spec) 0.0))
        native (map first (chaos/cat-rows 0.0 {}))]
    (is (< (reduce max native) 7.3))
    (is (> (reduce max xs) 9.0) "beads reach the ultrawide edge")))

(defn- mean-diff ^double [^ints a ^ints b]
  (let [n (alength a)]
    (loop [i 0 acc 0]
      (if (= i n)
        (/ (double acc) (* 3 n))
        (let [p (aget a i) q (aget b i)
              ch (fn ^long [^long s] (Math/abs (- (bit-and (bit-shift-right p s) 0xFF)
                                                  (bit-and (bit-shift-right q s) 0xFF))))]
          (recur (inc i) (+ acc (ch 0) (ch 8) (ch 16))))))))

(defn- wrap-ratio
  "Wrap step (last frame -> frame 0) over the median of the ordinary steps
   beside it (n-3 -> n-2, n-2 -> n-1, 0 -> 1), at the native frame count."
  [spec [w h]]
  (let [s (frame/at-size spec [w h])
        n (* (long (:seconds s)) (long (:fps s)))
        f (memoize (fn [i] (pixels (r/render-frame s (/ (double (mod i n)) n) {:size [w h]}) 0 w)))
        step (fn [i] (mean-diff (f i) (f (inc i))))
        ordinary (nth (sort (map step [-3 -2 0])) 1)]
    (/ (step -1) (max 0.02 ordinary))))

(deftest reframed-loops-wrap-at-ultrawide
  ;; A reframe must keep the loop seamless: at ultrawide the wrap step is
  ;; no bigger, relative to the ordinary steps, than at the authored 16:9
  ;; (some authored loops carry a small jump of their own; the reframe may
  ;; not add one).
  (doseq [[id spec] loops/all :when (:reframe spec)]
    (let [native (wrap-ratio spec [144 81])
          wide (wrap-ratio spec [192 81])]
      (is (<= wide (* 1.3 (max 1.0 native)))
          (str id " wraps at ultrawide: wrap/step " wide " vs 16:9 " native)))))

(deftest seam-is-no-bigger-than-a-step
  ;; Frame-review rule: a loop whose wrap step exceeds its ordinary steps
  ;; fails. neon-pulse staggered its rosettes by i + j, which the diagonal
  ;; drift does not preserve; breathers flashed a flooded zero level on the
  ;; flat u = 0 frame.
  (doseq [id [:neon-pulse :breathers]]
    (let [ratio (wrap-ratio (loops/all id) [144 81])]
      (is (<= ratio 1.0) (str id " wrap/step " ratio)))))

(deftest uw-flag
  (is (= {:uw true :size [2560 1080] :only [:box-modes]}
         (main/parse-args ["--uw" "--only" "box-modes"]))))
