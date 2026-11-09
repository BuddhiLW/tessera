(ns tessera.fourier-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.player.svg-path :as svg]
            [desargues.wallpaper.loop :as lp]
            [tessera.figures :as figures]
            [tessera.fourier :as f]
            [desargues.wallpaper.render :as render]
            [tessera.vj-test :as metrics]
            [desargues.tex.typeset :as typeset]))

(defn error [a b] (reduce max 0.0 (map #(abs (- %1 %2)) a b)))

(deftest equal-arc-length-with-no-duplicate-endpoint
  (let [square [[0 0] [2 0] [2 0] [2 2] [0 2] [0 0]]]
    (is (= [[0.0 0.0] [1.0 0.0] [2.0 0.0] [2.0 1.0]
            [2.0 2.0] [1.0 2.0] [0.0 2.0] [0.0 1.0]]
           (f/resample-points square 8))))
  (let [circle #(vector (Math/cos (* lp/tau %)) (Math/sin (* lp/tau %)))
        pts (f/resample-curve circle 64)]
    (is (= 64 (count pts)))
    (doseq [i (range 64)] (is (< (error (pts i) (circle (/ i 64.0))) 1.0e-9))))
  (doseq [pts [[] [[0 0] [0 0]] [[0 0] [Double/NaN 1]] [[0 0] [1 2 3]]]]
    (is (thrown? clojure.lang.ExceptionInfo (f/resample-points pts 16))))
  (is (thrown? clojure.lang.ExceptionInfo (f/resample-points [[0 0] [1 1]] 1))))

(deftest disconnected-subpaths-close-before-the-bridges
  (let [segs (svg/segments "M0 0 L2 0 L2 2 M5 0 l1 0 l0 1z")
        pts (f/segments->points segs)]
    (is (= [[0.0 0.0] [2.0 0.0] [2.0 2.0] [0.0 0.0]
            [5.0 0.0] [6.0 0.0] [6.0 1.0] [5.0 0.0]] pts))
    (is (= 128 (count (f/resample-segments segs 128)))))
  (doseq [d ["M0 0 Q1 2 2 0Z" "M0 0 C0 3 3 3 3 0Z" "M1 0A1 1 0 1 1 -1 0A1 1 0 1 1 1 0Z"]]
    (let [pts (f/segments->points (svg/segments d) {:flatness 0.001})]
      (is (> (count pts) 8))
      (is (= (first pts) (peek pts))))))

(deftest svg-definitions-use-placement-and-transforms
  (let [doc "<svg xmlns:xlink='http://www.w3.org/1999/xlink'><defs><path id='g' d='M0 0L1 0L0 1Z'/></defs><g transform='translate(10,20) scale(2)'><use x='3' y='4' xlink:href='#g'/><rect x='0' y='0' width='2' height='1'/></g></svg>"
        segs (f/svg->segments doc)]
    (is (= [:move 16.0 28.0] (first segs)))
    (is (= [:line 18.0 28.0] (second segs)))
    (is (= 9 (count segs)))
    (is (= [:move 10.0 20.0] (nth segs 4))))
  (is (thrown? clojure.lang.ExceptionInfo
               (f/svg->segments "<svg><defs><g id='g'><use href='#g'/></g></defs><use href='#g'/></svg>"))))

(deftest complex-dft-signed-frequencies-and-normalization
  (doseq [m [15 16 31 32]]
    (let [curve (fn [u] (let [a (* lp/tau u)]
                         [(+ 1.25 (* 2.0 (Math/cos (* -3 a))) (* 0.5 (Math/cos (* 2 a))))
                          (+ -0.75 (* 2.0 (Math/sin (* -3 a))) (* 0.5 (Math/sin (* 2 a))))]))
          pts (mapv #(curve (/ % (double m))) (range m))
          terms (f/dft pts) top (f/top-terms terms 3)]
      (is (= #{-3 0 2} (set (map :frequency top))))
      (is (< (abs (- 2.0 (:magnitude (first top)))) 1.0e-12))
      (doseq [i (range m)]
        (is (< (error (pts i) (f/point terms (/ i (double m)))) 1.0e-11))))
    (let [pts (mapv #(vector (double %) (Math/sin %)) (range m)) terms (f/dft pts)]
      (is (= m (count (set (map :frequency terms)))))
      (doseq [i (range m)]
        (is (< (error (pts i) (f/point terms (/ i (double m)))) 1.0e-10)))))
  (is (= #{0 1 -2 -1} (set (map :frequency (f/dft [[1 0] [-1 0] [1 0] [-1 0]]))))))

(deftest real-vector-dft-reconstructs-the-space-curve
  (doseq [m [31 32]]
    (let [pts (f/resample-curve figures/trefoil m) terms (f/dft pts)]
      (doseq [i (range m)]
        (is (< (error (pts i) (f/point terms (/ i (double m)))) 1.0e-10))))
    (let [pts (mapv #(vector (double %) (Math/sin %) (if (even? %) 1.0 -1.0)) (range m))
          terms (f/dft pts)]
      (doseq [i (range m)]
        (is (< (error (pts i) (f/point terms (/ i (double m)))) 1.0e-10))))))

(deftest bounded-selection-and-periodicity
  (let [terms (f/dft (f/resample-curve figures/trefoil 128)) top (f/top-terms terms 24)]
    (is (= 24 (count top)))
    (is (apply >= (map :magnitude top)))
    (is (= [] (f/top-terms terms 0)))
    (is (= (count terms) (count (f/top-terms terms 1000))))
    (is (= (inc (count top)) (count (f/chain top 0.3))))
    (doseq [u [0.0 0.17 0.73 -0.25]]
      (is (< (error (f/point top u) (f/point top (+ u 1.0))) 1.0e-12))))
  (is (thrown? clojure.lang.ExceptionInfo (f/top-terms [] -1)))
  (is (thrown? clojure.lang.ExceptionInfo (f/dft []))))

(deftest formula-boundary-skips-unavailable-binaries
  (let [adapter (typeset/latex-typesetter "absent-latex" "absent-dvisvgm")]
    (is (:skipped (f/typeset-formula adapter f/gauss-law))))
  (with-redefs [typeset/available? (constantly true)
                typeset/typeset (fn [_ _ _] {:svg "<svg><path d='M0 0L1 0L0 1Z'/></svg>"})]
    (is (= 4 (count (:segments (f/typeset-formula :fake "x"))))))
  (is (= 96 (count @f/formula-terms)))
  (is (= 24 (count f/trefoil-terms)))
  (is (= 24 (count f/space-terms))))

(deftest every-offline-fourier-loop-closes-its-seam
  (is (= #{:fourier-trefoil :fourier-space :fourier-formula} (set (keys f/loops))))
  (doseq [[id spec] f/loops]
    (testing (name id)
      (is (= id (:id spec)))
      (is (= [1920 1080] (:size spec)))
      (is (= 30 (:fps spec)))
      ;; The seam is measured at the actual frame count, not a shortened
      ;; preview period. Distributed ordinary steps sample its maximum.
      (let [{:keys [seam typical-max]} (metrics/seam-stats spec [160 90] (map #(/ (+ % 0.5) 24.0) (range 24)))
            {:keys [near-black white-clip peak-channel]} (metrics/image-stats (render/render-frame spec 0.3 {:size [160 90]}))]
        (is (<= seam typical-max))
        (is (> near-black 0.3))
        (is (< white-clip 0.01))
        (is (> peak-channel 128))))))
