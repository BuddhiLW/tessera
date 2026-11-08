(ns tessera.compose-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.compose.core :as c]
            [tessera.compose.layout :as layout]
            [tessera.compose.scene :as scene]
            [tessera.compose.scenes :as scenes]
            [tessera.vj :as vj]
            [tessera.vj2 :as vj2]
            [tessera.vj-test :as metrics]))

(def parity-tolerance
  "Mean absolute RGB channel difference (0..255) a composed scene may have
   from the approved monolith frame. Measured: 0.0 (bit-exact) for both."
  0.5)

(defn- frame-diff [spec monolith-field u]
  (metrics/mean-diff
   (metrics/pixels (r/render-frame spec u {:size [320 180]}))
   (metrics/pixels (r/render-frame (assoc spec :field monolith-field) u {:size [320 180]}))))

(deftest composed-scenes-match-the-approved-monoliths
  (doseq [[scene monolith] [[scenes/kaleido-quasicrystal vj/kaleido-shader]
                            [scenes/fabric vj2/fabric-shader]]
          :let [spec (scene/->spec scene)]
          u [0.0 0.3 0.7]]
    (testing (str (name (:id scene)) " u=" u)
      (let [d (frame-diff spec monolith u)]
        (is (<= d parity-tolerance))
        (is (zero? d) "bit-exact parity")))))

(deftest scenes-are-valid-edn-data
  (doseq [s [scenes/kaleido-quasicrystal scenes/fabric]]
    (is (nil? (scene/explain s)))
    (is (= s (read-string (pr-str s))))))

(deftest invalid-scenes-are-refused
  (is (some? (scene/explain {:id :x :seconds 1 :fps 30 :layers []})))
  (is (thrown? clojure.lang.ExceptionInfo
               (scene/->spec {:id :x :seconds 1 :fps 30 :layers [{:ink "red"}]}))))

(deftest expressions-fold-like-clojure
  (let [slot (c/allocator) e (c/expr [:+ [:* 2.0 :x] [:glow :y 0.5] [:wave :x]] slot)
        ctx (double-array [0.3 0.1])]
    (is (= (+ (* 2.0 0.3) (c/glow 0.1 0.5) (* 0.5 (+ 1.0 (Math/cos (* 2.0 Math/PI 0.3)))))
           (.at ^tessera.compose.core.IFactor e ctx)))))

(deftest poisson-layout-is-the-approved-placement
  (is (= vj2/fabric-sites (layout/poisson-disc 73129 100 0.46))))
