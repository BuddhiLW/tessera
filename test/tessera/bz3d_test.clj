(ns tessera.bz3d-test
  "Geometry of the 3D Brillouin zones; nothing is rendered."
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.loop :as lp]
            [tessera.bz3d :as bz]))

(defn- zone-of [id] (bz/zone (get-in bz/lattices [id :reciprocal])))

(defn- dot [a b] (reduce + (map * a b)))

(defn- counts [z] [(count (:vertices z)) (count (bz/edges z)) (count (:faces z))])

(deftest vertex-edge-face-counts
  (is (= [24 36 14] (counts (zone-of :fcc))) "truncated octahedron")
  (is (= [14 24 12] (counts (zone-of :bcc))) "rhombic dodecahedron")
  (is (= [8 12 6] (counts (zone-of :sc))) "cube")
  (is (= [12 18 8] (counts (zone-of :hex))) "hexagonal prism"))

(deftest euler-characteristic
  (doseq [id (keys bz/lattices)
          :let [[v e f] (counts (zone-of id))]]
    (is (= 2 (+ (- v e) f)) (name id))))

(deftest face-shapes
  (is (= {6 8, 4 6} (frequencies (map (comp count :vertices) (:faces (zone-of :fcc))))))
  (is (= {4 12} (frequencies (map (comp count :vertices) (:faces (zone-of :bcc)))))))

(deftest faces-lie-on-bragg-planes
  (doseq [id (keys bz/lattices)
          :let [{:keys [vertices faces]} (zone-of id)]
          {:keys [g] vs :vertices} faces
          i vs]
    (is (< (Math/abs (- (dot g (vertices i)) (/ (dot g g) 2.0))) 1e-9)
        (str (name id) " face " g))))

(deftest every-vertex-in-the-zone
  (testing "no vertex is nearer another lattice point than the origin"
    (doseq [id (keys bz/lattices)
            :let [{:keys [vertices]} (zone-of id)
                  gs (bz/bragg-vectors (get-in bz/lattices [id :reciprocal]) 2)]
            k vertices]
      (is (every? #(<= (dot % k) (+ (/ (dot % %) 2.0) 1e-9)) gs) (name id)))))

(deftest faces-wind-outward
  (doseq [id (keys bz/lattices)
          :let [{:keys [vertices faces] :as z} (zone-of id)
                tri (bz/triangles z 1.0 [0 0 0])]
          [a b c] (:faces tri)
          :let [[pa pb pc] (map (:vertices tri) [a b c])
                u (mapv - pb pa) w (mapv - pc pa)
                n [(- (* (u 1) (w 2)) (* (u 2) (w 1)))
                   (- (* (u 2) (w 0)) (* (u 0) (w 2)))
                   (- (* (u 0) (w 1)) (* (u 1) (w 0)))]]]
    (is (pos? (dot n pa)) (name id))))

(deftest symmetry-points-on-the-zone
  (testing "every point but Gamma lies on the boundary of its zone"
    (doseq [id (keys bz/lattices)
            :let [{:keys [faces]} (zone-of id)]
            [label k] (get-in bz/lattices [id :points])
            :when (not= label :Gamma)]
      (is (some (fn [{:keys [g]}] (< (Math/abs (- (dot g k) (/ (dot g g) 2.0))) 1e-9)) faces)
          (str (name id) " " (name label))))))

(deftest only-the-lamp-moves
  (let [a (bz/bz-glass-scene 0.0) b (bz/bz-glass-scene 0.37) c (bz/bz-glass-scene 1.0)]
    (is (= (dissoc (:objects a) :lamp) (dissoc (:objects b) :lamp)))
    (is (= (:camera a) (:camera b)))
    (is (not= (get-in a [:objects :lamp]) (get-in b [:objects :lamp])))
    (is (lp/periodic? bz/orbit))
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-9)
                           (flatten (get-in a [:objects :lamp :vertices]))
                           (flatten (get-in c [:objects :lamp :vertices])))))))
