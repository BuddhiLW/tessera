(ns tessera.flows-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.flows :as f])
  (:import (java.awt.image BufferedImage)))

(defn pixels ^ints [^BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(defn mean-diff [^ints a ^ints b]
  (let [ch (fn [p s] (bit-and (bit-shift-right p s) 0xFF))]
    (/ (double (reduce + (for [i (range (alength a)) s [0 8 16]]
                          (Math/abs (- (ch (aget a i) s) (ch (aget b i) s))))))
       (* 3 (alength a)))))

(defn wrap-metrics
  "Same wrap-vs-ordinary-motion comparison as tessera.loops-test."
  [spec]
  (let [n 60
        frame (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size [160 90]})))
        typical (apply max (map #(mean-diff (frame %) (frame (inc %))) [5 25 45]))
        seam (mean-diff (frame (dec n)) (frame 0))]
    {:ordinary typical :seam seam :limit (+ 1.0 (* 1.5 typical))}))

(defn max-error [a b]
  (reduce max 0.0 (map #(Math/abs (double (- %1 %2))) (flatten a) (flatten b))))

(deftest every-flow-wraps-without-a-visible-seam
  (doseq [[k spec] f/loops]
    (testing (name k)
      (let [{:keys [seam limit]} (wrap-metrics spec)] (is (<= seam limit)))
      (let [frame #(pixels (r/render-frame spec % {:size [160 90]}))]
        (is (zero? (mean-diff (frame 0.0) (frame 1.0))))
        (is (< (mean-diff (frame -0.001) (frame 0.999)) 0.01))
        (is (< (mean-diff (frame 0.237) (frame 1.237)) 0.01))))))

(deftest analytic-curl-is-incompressible-and-periodic
  (doseq [t [0.0 0.23 0.73] q [[0.0 0.0] [1.7 -0.9] [-3.1 1.4]]]
    (let [[x y] q eps 1.0e-5
          dx (fn [fun] (/ (- (fun [(+ x eps) y]) (fun [(- x eps) y])) (* 2.0 eps)))
          dy (fn [fun] (/ (- (fun [x (+ y eps)]) (fun [x (- y eps)])) (* 2.0 eps)))
          vx #(first (f/curl-velocity t %)) vy #(second (f/curl-velocity t %))]
      (is (< (Math/abs (+ (dx vx) (dy vy))) 1.0e-8))
      (is (< (max-error (f/curl-velocity t q)
                       [(dy #(f/stream-function t %)) (- (dx #(f/stream-function t %)))]) 1.0e-8))
      (is (< (max-error (f/curl-velocity t q) (f/curl-velocity (+ t 1.0) q)) 1.0e-12)))))

(deftest replacement-hides-open-trajectory-resets
  (is (= 0.0 (f/envelope 0.0) (f/envelope 1.0)))
  (is (< (f/envelope 1.0e-7) 1.0e-16))
  (is (< (f/envelope (- 1.0 1.0e-7)) 1.0e-16))
  (let [particle {:offset 0.0 :rgb [1.0 0.5 0.2]
                  :path (double-array [0.0 0.0 3.0 2.0])}
        rows #(f/streak-rows [particle] % {:tile f/view})]
    (is (empty? (rows 0.0)))
    (is (empty? (rows 1.0)))
    (is (empty? (rows -1.0e-8)))
    (is (seq (rows 0.5)))
    (is (= [3.0 2.0] (f/path-point (:path particle) 1.0))))
  (doseq [particles [@f/curl-particles @f/street-particles]]
    (is (>= (count particles) 1500))
    (is (every? #(Double/isFinite (double %))
                (mapcat (fn [p] (seq (:path p))) particles))))
  ;; These paths deliberately do not close: no hidden linear seam repair.
  (is (> (max-error (f/path-point (:path (first @f/curl-particles)) 0.0)
                    (f/path-point (:path (first @f/curl-particles)) 1.0)) 0.1)))

(deftest periodic-green-function-and-hamiltonian-equations
  (let [length 4.8 gamma [4.0 -4.0 1.2 -1.2]
        q [[0.0 0.75] [2.3 -0.65] [1.1 1.8] [3.3 -1.9]]
        v (f/vortex-velocities length gamma q) eps 1.0e-6
        derivative (fn [i axis]
                     (/ (- (f/vortex-hamiltonian length gamma (update-in q [i axis] + eps))
                           (f/vortex-hamiltonian length gamma (update-in q [i axis] - eps)))
                        (* 2.0 eps)))]
    (doseq [i (range 4)]
      (is (< (max-error (v i) [(/ (derivative i 1) (gamma i))
                              (/ (- (derivative i 0)) (gamma i))]) 1.0e-8)))
    (is (< (max-error (f/periodic-kernel length [0.7 -0.3])
                     (f/periodic-kernel length [(+ length 0.7) -0.3])) 1.0e-13))
    (is (< (max-error (f/periodic-kernel length [0.7 -0.3])
                     (mapv - (f/periodic-kernel length [-0.7 0.3]))) 1.0e-13))
    (let [s 1.0e-5 [x y] [(* s 0.7) (* s -0.3)]
          r2 (+ (* x x) (* y y))]
      (is (< (max-error (mapv #(* s %) (f/periodic-kernel length [x y]))
                       [(/ (* s (- y)) (* 2.0 Math/PI r2))
                        (/ (* s x) (* 2.0 Math/PI r2))]) 1.0e-9)))
    (is (thrown? clojure.lang.ExceptionInfo (f/periodic-kernel length [0.0 0.0])))))

(deftest translating-street-is-an-exact-relative-equilibrium
  (let [{:keys [length strengths positions speed period]} f/street
        v (f/vortex-velocities length strengths positions)
        states (vec (take 1001 (iterate #(f/vortex-midpoint length strengths % (/ period 1000.0)) positions)))
        analytic (f/street-vortices period)
        e0 (f/vortex-hamiltonian length strengths positions)]
    (is (< (max-error v [[speed 0.0] [speed 0.0]]) 1.0e-13))
    (is (< (max-error (peek states) analytic) 1.0e-10))
    (is (< (Math/abs (- (* speed period) length)) 1.0e-13))
    (is (< (reduce max (map #(Math/abs (- (f/vortex-hamiltonian length strengths %) e0)) states)) 1.0e-10))
    (is (< (max-error positions (mapv (fn [[x y]] [(- x length) y]) analytic)) 1.0e-13))))

(deftest hamiltonian-conservation-away-from-equilibrium
  ;; A genuinely evolving four-vortex problem: the equilibrium-only check
  ;; would pass even for an integrator that froze all relative coordinates.
  (let [length (:length f/street) gamma [4.0 -4.0 1.2 -1.2]
        q [[0.0 0.75] [2.3 -0.65] [1.1 1.8] [3.3 -1.9]]
        states (vec (take 3001 (iterate #(f/vortex-midpoint length gamma % 0.002) q)))
        e0 (f/vortex-hamiltonian length gamma q)
        drift (reduce max (map #(Math/abs (- (f/vortex-hamiltonian length gamma %) e0)) states))]
    (is (> (Math/abs (- (get-in (peek states) [0 1]) (get-in q [0 1]))) 0.1))
    (is (< drift 1.0e-6))
    (is (< (max-error q (f/vortex-midpoint length gamma (f/vortex-midpoint length gamma q 0.002) -0.002)) 1.0e-11))))

(deftest street-dye-and-vortices-cover-the-whole-view
  (let [length (:length f/street)
        positions (vec (f/tile-points [0.0 0.0] [length nil] [0.0 0.0]))
        rows (f/street-splats 0.25)
        ;; Bins in scene coordinates catch accidentally one-sided image indices.
        bins (frequencies (map (fn [[x]] (long (Math/floor (* 3.0 (+ 0.5 (/ x (first f/view))))))) rows))]
    (is (= [(- length) 0.0 length] (mapv first positions)))
    (is (= (first f/view) (* 3.0 length)))
    (doseq [i (range 3)] (is (> (get bins i 0) 15000)))
    (is (> (count (distinct (map :band @f/street-particles))) 15))
    (is (< (apply min (map (comp second :origin) @f/street-particles)) -3.7))
    (is (> (apply max (map (comp second :origin) @f/street-particles)) 3.7))))
