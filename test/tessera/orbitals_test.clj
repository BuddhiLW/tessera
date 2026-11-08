(ns tessera.orbitals-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.orbitals :as orbital])
  (:import (java.awt.image BufferedImage)))

(defn- pixels ^ints [^BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(defn- mean-diff [^ints a ^ints b]
  (let [ch (fn [p s] (bit-and (bit-shift-right p s) 0xFF))]
    (/ (double (reduce + (for [i (range (alength a)) s [0 8 16]]
                           (Math/abs (- (ch (aget a i) s) (ch (aget b i) s))))))
       (* 3 (alength a)))))

(deftest every-loop-wraps-without-a-visible-seam
  (doseq [[k spec] orbital/loops]
    (testing (name k)
      (let [n 60
            f (fn [i] (pixels (r/render-frame spec (/ (double i) n) {:size [160 90]})))
            typical (apply max (map #(mean-diff (f %) (f (inc %))) [5 25 45]))
            seam (mean-diff (f (dec n)) (f 0))]
        (is (<= seam (+ 1.0 (* 1.5 typical))))))))

(defn- near? [a b]
  (< (Math/abs (- (double a) (double b))) 1.0e-10))

(defn- close?
  "Tolerance suitable for the finite-difference quadratures below."
  ([a b] (close? a b 1.0e-7))
  ([a b tolerance]
   (< (Math/abs (- (double a) (double b))) (double tolerance))))

(defn- integral
  "Midpoint quadrature of f on [a,b], with n equal subdivisions."
  [f a b n]
  (let [a (double a) b (double b) n (long n)
        h (/ (- b a) n)]
    (* h (reduce + 0.0
                 (map (fn [i] (f (+ a (* (+ i 0.5) h)))) (range n))))))

(deftest hydrogen-eigenstates-are-orthonormal
  (doseq [[n l m] orbital/hydrogen-states]
    (testing (str "R_" n l " and Y_" l m)
      (let [radius (Math/sqrt 7.69)
            value (orbital/hydrogen-eigenvalue n l m 1.2 0.0 2.5)]
        (is (near? value (* (orbital/hydrogen-radial n l radius)
                            (orbital/hydrogen-angular l (/ 2.5 radius))))))
      (is (close? 1.0 (integral (fn [r]
                                  (let [v (orbital/hydrogen-radial n l r)]
                                    (* r r v v)))
                                0.0 180.0 20000) 1.0e-6))
      (is (close? 1.0 (* 2.0 Math/PI
                         (integral (fn [z]
                                     (let [v (orbital/hydrogen-angular l z)]
                                       (* v v)))
                                   -1.0 1.0 16000)) 1.0e-6))))
  (is (near? 0.0 (integral (fn [r]
                             (* r r (orbital/hydrogen-radial 2 1 r)
                                (orbital/hydrogen-radial 3 1 r)))
                           0.0 100.0 20000)))
  (doseq [[l1 l2] [[0 1] [0 2] [1 2]]]
    (is (close? 0.0 (integral (fn [z]
                                (* (orbital/hydrogen-angular l1 z)
                                   (orbital/hydrogen-angular l2 z)))
                              -1.0 1.0 16000))))
  (is (near? 1.0 (reduce + (map #(* % %) orbital/hydrogen-coefficients)))))

(deftest hydrogen-superposition-is-normalized
  (doseq [u [0.0 0.31 0.77]]
    (testing (str "phase " u)
      (let [nr 640 nz 128
            dr (/ 70.0 nr) dz (/ 2.0 nz)
            volume (reduce + 0.0
                           (for [ir (range nr) iz (range nz)
                                 :let [radius (* (+ ir 0.5) dr)
                                       cosine (- 1.0 (* (+ iz 0.5) dz))
                                       sine (Math/sqrt (- 1.0 (* cosine cosine)))
                                       x (* radius sine) z (* radius cosine)
                                       [re im] (orbital/hydrogen-psi u x 0.0 z)]]
                             (* radius radius (+ (* re re) (* im im)))))]
        (is (< 1.0e-5
               (Math/abs (- 1.0 (* 2.0 Math/PI dr dz volume)))))))))

(deftest wavefunctions-and-membrane-are-periodic
  (doseq [u [0.0 0.071 0.25 0.63]
          [x y z] [[0.0 0.0 0.0] [1.3 0.0 -2.1] [-4.0 0.0 6.0] [8.0 0.0 1.0]]]
    (let [[re im] (orbital/hydrogen-psi u x y z)
          [re1 im1] (orbital/hydrogen-psi (inc u) x y z)]
      (is (near? re re1))
      (is (near? im im1))))
  (doseq [u [0.0 0.071 0.25 0.63]
          [x y] [[0.1 0.2] [0.5 0.5] [0.73 0.41] [0.92 0.87]]]
    (let [[re im] (orbital/box-psi u x y)
          [re1 im1] (orbital/box-psi (inc u) x y)]
      (is (near? re re1))
      (is (near? im im1))
      (is (near? (orbital/drum-amplitude u x y)
                 (orbital/drum-amplitude (inc u) x y))))))

(deftest square-well-modes-are-commensurate
  (is (= 5 (count (distinct (partition 2 orbital/box-mode-numbers)))))
  ;; Energies m^2+n^2 are integers, so every mode completes an integral
  ;; number of cycles in one loop: psi(u+1) = psi(u) exactly (to float).
  ;; Periodicity at points inside the 16 by 9 well.
  (doseq [u [0.0 0.071 0.25 0.63]
          [x y] [[1.6 1.8] [8.0 4.5] [11.7 3.7] [14.7 7.8]]]
    (let [[re im] (orbital/box-psi u x y)
          [re1 im1] (orbital/box-psi (inc u) x y)]
      (is (near? re re1))
      (is (near? im im1))))
  ;; The wavefunction vanishes on the walls of the well.
  (doseq [[x y] [[0.0 3.0] [16.0 3.0] [4.0 0.0] [4.0 9.0]]]
    (let [[re im] (orbital/box-psi 0.31 x y)]
      (is (near? 0.0 re))
      (is (near? 0.0 im)))))

(deftest drum-tiles-by-odd-reflection
  (let [fold (fn [q] (let [q (mod q 2.0)] (if (<= q 1.0) q (- 2.0 q))))
        f (fn [q] (let [x (fold q)]
                    (reduce + (map (fn [m] (* (Math/sin (* Math/PI m x))
                                              (Math/cos (* m 0.13))))
                                   [3 4 6 8 9 12 16]))))]
    (doseq [q [0.0 0.2 0.7 1.0 1.4 2.3]]
      (is (near? (f q) (f (- q))))
      (is (near? (f q) (f (+ q 2.0)))))))

