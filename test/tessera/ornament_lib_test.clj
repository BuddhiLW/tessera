(ns tessera.ornament-lib-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [tessera.ornament.lib :as o]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))
(defn- xf= [m n] (every? true? (map close? m n)))

(defn- closed-under?
  "Every product of two elements of `group` is again in it."
  [group]
  (every? (fn [[a b]] (some #(xf= (o/xf* a b) %) group))
          (for [a group b group] [a b])))

(deftest point-groups-have-their-order-and-close
  (doseq [n [3 5 6 8 12]]
    (is (= n (count (o/cyclic n))))
    (is (= (* 2 n) (count (o/dihedral n))))
    (is (closed-under? (o/cyclic n)))
    (is (closed-under? (o/dihedral n)))))

(deftest every-mirror-is-an-involution
  (doseq [t [0.0 0.3 1.1 (/ Math/PI 2)]]
    (is (xf= o/identity-xf (o/xf* (o/mirror t) (o/mirror t))))))

(deftest frieze-cells-close-modulo-the-period
  (let [T 10.0]
    (doseq [g o/frieze-groups
            :let [cell (o/frieze-cell g T)
                  ;; reduce a map modulo translation by T along x
                  mod-T (fn [[a b c d e f]] [a b c d (mod (+ e 1e-12) T) f])]]
      (testing (name g)
        (is (every? (fn [[a b]] (some #(xf= (mod-T (o/xf* a b)) (mod-T %)) cell))
                    (for [a cell b cell] [a b])))))))

(deftest seven-distinct-friezes
  (let [rounded (fn [cell] (set (map (fn [m] (mapv #(Math/round (* 1e6 (double %))) m)) cell)))]
    (is (= 7 (count o/frieze-groups)))
    (is (= 7 (count (set (map #(rounded (o/frieze-cell % 1.0)) o/frieze-groups)))))))

(deftest seeded-output-is-deterministic
  (is (= (o/rosette-doc 7 {}) (o/rosette-doc 7 {})))
  (is (not= (o/rosette-doc 7 {}) (o/rosette-doc 8 {})))
  (is (= (o/frieze-doc 101 :p11g) (o/frieze-doc 101 :p11g))))

(deftest one-group-per-motif
  (let [{:keys [group motifs]} (o/rosette 7 [0.0 0.0] 100.0 {:group [:d 6]})
        doc (o/svg {:w 10 :h 10} motifs)]
    (is (= [:d 6] group))
    ;; a sun plus three rings, each the 12-element orbit of Dn
    (is (= (inc (* 3 12)) (count motifs)))
    (is (= (count motifs) (count (re-seq #"<g id=" doc))))
    (is (apply distinct? (map :id motifs)))))

(deftest paths-round-trip-through-the-desargues-parser
  (doseq [segs [(o/droplet 10.0 3.0) (o/sun 9 2.0 5.0 0.5) (o/band 40.0 2.0 3.0 2)]]
    (is (= (o/segs->d segs) (o/segs->d (o/d->segs (o/segs->d segs)))))))

(deftest primitives-are-closed
  (doseq [segs [(o/droplet 10.0 3.0) (o/pin 10.0 2.0) (o/bead [0.0 0.0] 3.0 1.0)
                (o/band 40.0 2.0 3.0 2) (o/sun 9 2.0 5.0 0.5)]]
    (is (= [:close] (peek segs)))
    (is (str/starts-with? (o/segs->d segs) "M "))))

(deftest heroes-are-deterministic-and-grouped
  (doseq [f [#(o/hero-mandala 5) #(o/hero-desargues 3) #(o/hero-khatam 8)]]
    (let [d (f) ids (map second (re-seq #"<g id=\"([^\"]+)\"" d))]
      (is (= d (f)))
      (is (> (count ids) 100))
      (is (apply distinct? ids) "every <g> id is unique, so an id mask splits motifs"))))

(deftest generate-reproduces-the-committed-svgs
  (let [tmp (str (java.nio.file.Files/createTempDirectory "orn" (make-array java.nio.file.attribute.FileAttribute 0)))
        files (o/generate! {:root tmp})]
    (is (= 18 (count files)))
    (doseq [f files
            :let [rel (subs (str f) (inc (count tmp)))
                  committed (java.io.File. rel)]
            :when (.exists committed)]
      (is (= (slurp committed) (slurp f)) rel))))
