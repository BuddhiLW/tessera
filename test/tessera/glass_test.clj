(ns tessera.glass-test
  (:require [clojure.test :refer [deftest is testing]]
            [tessera.glass :as gl]
            [desargues.wallpaper.girih :as g]))

(deftest sun-is-exactly-periodic
  ;; sin(2 pi u) is only bit-close after a whole loop, never bit-equal; the
  ;; contract is that no seam is VISIBLE, so compare against float noise.
  (doseq [u [0.0 0.13 0.37 0.5 0.83]]
    (is (< (g/vlen (g/v- (gl/sun-dir u 0.42 2.30 0.44)
                         (gl/sun-dir (+ u 1.0) 0.42 2.30 0.44)))
           1e-12)
        (str "sun direction repeats after one loop at u=" u)))
  (testing "the sun actually moves"
    (is (< 1e-3 (g/vlen (g/v- (gl/sun-dir 0.0 0.42 2.30 0.44)
                              (gl/sun-dir 0.25 0.42 2.30 0.44)))))))

(deftest meshes-are-closed-and-wound-outward
  (let [star (:rosette (g/hankin (:vertices (first (:cell (g/tiling :4.8.8))))
                                 (g/khatam-angle) 0.0))
        m (gl/bevel-prism star 0.0 0.4 0.08)
        {:keys [vertices faces]} m]
    (testing "every face index addresses a vertex"
      (is (every? (fn [f] (and (= 3 (count f))
                               (every? #(and (<= 0 %) (< % (count vertices))) f)))
                  faces)))
    (testing "every edge is shared by exactly two triangles, in opposite
              directions, so the solid is watertight for volume absorption"
      (let [dirs (frequencies
                  (for [[a b c] faces
                        [i j] [[a b] [b c] [c a]]]
                    [i j]))]
        (is (every? (fn [[_ n]] (= 1 n)) dirs)
            "directed edges each appear once")
        (is (every? (fn [[[i j]]]
                      (= 1 (get dirs [j i] 0)))
                    dirs)
            "and the reverse appears exactly once too")))))

(deftest tiles-leave-joints
  (let [ts (gl/tiles 4.0 0.06)
        objs (gl/tile-objects ts 0.03 0.40 0.86)]
    (is (pos? (count (:vertices (:stars objs)))))
    (is (pos? (count (:vertices (:squares objs)))))
    (testing "shrinking about the centre really shrinks the polygon"
      (let [t (first ts)
            shrunk (mapv (fn [v] (g/v+ (:centre t) (g/v* 0.86 (g/v- v (:centre t)))))
                         (:points t))
            r (fn [pts] (apply max (map #(g/vlen (g/v- % (:centre t))) pts)))]
        (is (< (Math/abs (- (* 0.86 (r (:points t))) (r shrunk))) 1e-9))))))

(deftest volumes-reach-the-scene-file
  (let [props (gl/scene-props (gl/girih-glass-scene 0.0 {:spp 8}))
        m (into {} props)]
    (is (= "clear" (get m "scene.volumes.cobalt.type")))
    (is (= "clear" (get m "scene.volumes.honey.type")))
    (is (= [3.0 1.2 0.15] (get m "scene.volumes.cobalt.absorption")))
    (testing "the materials point at their volumes, not at a material of the
              same name"
      (is (= :cobalt (get m "scene.materials.lapis.volume.interior")))
      (is (= :honey (get m "scene.materials.amber.volume.interior"))))))

(deftest only-the-sun-moves
  (let [a (gl/girih-glass-scene 0.0 {:spp 8})
        b (gl/girih-glass-scene 0.25 {:spp 8})]
    (is (= (:objects a) (:objects b)) "the tiles are static")
    (is (= (:camera a) (:camera b)) "the camera never moves")
    (is (= 1 (count (:lights a))) "one light only, no flat ambient fill")
    (is (not= (get-in a [:lights :key :dir]) (get-in b [:lights :key :dir])))))

(deftest loop-spec
  (is (= :girih-glass (:id gl/girih-glass)))
  (is (= [:girih-glass] (keys gl/loops)))
  (is (fn? (:scene gl/girih-glass)))
  (is (map? ((:scene gl/girih-glass) 0.3))))
