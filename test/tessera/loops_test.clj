(ns tessera.loops-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.loops :as loops]
            [tessera.main :as main]
            [tessera.palette :as pal])
  (:import (java.awt.image BufferedImage)))

(defn- pixels ^ints [^BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(defn- mean-diff [^ints a ^ints b]
  (let [ch (fn [p s] (bit-and (bit-shift-right p s) 0xFF))]
    (/ (double (reduce + (for [i (range (alength a)) s [0 8 16]]
                           (Math/abs (- (ch (aget a i) s) (ch (aget b i) s))))))
       (* 3 (alength a)))))

(deftest every-loop-wraps-without-a-visible-seam
  (doseq [[k spec] loops/all]
    (testing (name k)
      (let [n 60
            f (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size [160 90]})))
            typical (apply max (map #(mean-diff (f %) (f (inc %))) [5 25 45]))
            seam (mean-diff (f (dec n)) (f 0))]
        (is (<= seam (+ 1.0 (* 1.5 typical))))))))

(deftest cycled-palettes-repeat-after-one-period
  (doseq [p [pal/lapis-gold pal/ember]
          t [0.0 0.13 0.5 0.77]]
    (is (= (pal/at p t) (pal/at p (+ t 1.0))))))

(deftest command-line
  (is (= {:only [:girih-light :julia-orbit] :size [1920 1080] :format :webm :seconds 6.0}
         (main/parse-args ["--only" "girih-light,julia-orbit" "--size" "1920x1080"
                           "--format" "webm" "--seconds" "6"])))
  (is (thrown? clojure.lang.ExceptionInfo (main/parse-args ["--bogus"]))))
