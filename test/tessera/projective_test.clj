(ns tessera.projective-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.emit :as emit]
            [desargues.wallpaper.field :as field]
            [desargues.wallpaper.render :as render]
            [desargues.wallpaper.wgsl :as wgsl]
            [hive-dsl.result :as result]
            [tessera.layers :as layers]
            [tessera.layers.ports :as ports]
            [tessera.loops :as loops]
            [tessera.media :as media]
            [tessera.projective :as p])
  (:import (java.awt.image BufferedImage)))

(defn- raster [l u]
  (let [img (BufferedImage. 96 54 BufferedImage/TYPE_INT_RGB)]
    (render/field-raster :jvm img (field/jvm-shader (p/bind l u)) {:view p/view :ss 1})
    (.getRGB img 0 0 96 54 nil 0 96)))

(defn- mean-diff [^ints a ^ints b]
  (/ (double (reduce + (for [i (range (alength a)) s [0 8 16]]
                         (Math/abs (- (bit-and (bit-shift-right (aget a i) s) 255)
                                      (bit-and (bit-shift-right (aget b i) s) 255))))))
     (* 3 (alength a))))

(deftest every-projective-loop-is-seamless
  (doseq [l p/layers]
    (testing (name (:id l))
      (let [n 128 frames (mapv #(raster l (/ (double %) n)) (range n))
            typical (apply max (map mean-diff (rest frames) (drop 2 frames)))
            seam (mean-diff (last frames) (first frames))]
        (is (<= seam (+ 0.05 typical)) (str "seam " seam " step " typical))))))

(deftest projective-fields-compile-on-every-target
  (doseq [l p/layers]
    (testing (name (:id l))
      (is (fn? (field/jvm-shader (p/bind l 0.3))))
      (is (result/ok? (field/source :glsl (:form l) {})))
      (is (result/ok? (wgsl/ir (:form l))))
      (is (result/ok? (emit/compiled l))))))

(deftest registered-ported-and-curated
  (doseq [l p/layers :let [loop-id (get ports/source-loops (:id l))]]
    (is (contains? loops/all loop-id))
    (is (some #{l} layers/layers))
    (is (= "Geometry" (first (media/families loop-id))))
    (is (string? (media/titles loop-id)))
    (is (map? (ports/effects (:id l))))))

(deftest desargues-axis-is-collinear
  (doseq [u [0.0 0.17 0.4 0.77]]
    (let [{:keys [triangles axis]} (p/desargues-geometry u)
          [ta tb] triangles
          sides (fn [t] (mapv p/join t (cycle (rest t))))
          qs (mapv p/meet (sides (mapv #(conj % 1.0) ta))
                   (sides (mapv #(conj % 1.0) tb)))]
      (doseq [[x y] qs]
        (is (< (Math/abs (+ (* (first axis) x) (* (second axis) y) (nth axis 2))) 1.0e-6))))))

(deftest pascal-axis-is-collinear
  (doseq [u [0.0 0.17 0.4 0.77]]
    (let [{:keys [pascal]} (p/pascal-geometry u)
          axis (p/join (conj (pascal 0) 1.0) (conj (pascal 1) 1.0))]
      (doseq [[x y] pascal]
        (is (< (Math/abs (+ (* (first axis) x) (* (second axis) y) (nth axis 2))) 1.0e-6))))))

(deftest pencil-keeps-its-four-base-points
  (doseq [u [0.0 0.13 0.25 0.48 0.75 0.92]]
    (let [[A B C D E F] (p/pencil-coefficients u)]
      (doseq [[x y w] p/pencil-bases]
        (is (< (Math/abs (+ (* A x x) (* B x y) (* C y y) (* D x w) (* E y w) (* F w w))) 1.0e-6))))))
