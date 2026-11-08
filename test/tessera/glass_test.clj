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

(deftest pieces-are-stars-and-crosses
  (let [ts (gl/tiles 3.6)]
    (is (pos? (count ts)))
    (is (every? #{:star :accent} (map :colour ts)))
    (testing "stars have 16 vertices (8 points), crosses are shrunk squares"
      (doseq [p ts]
        (is (= (if (= :star (:colour p)) 16 4) (count (:points p))))))
    (testing "each piece is a simple polygon of distinct points"
      (doseq [p ts]
        (is (= (count (:points p)) (count (distinct (:points p))))
            (str "degenerate piece " (:colour p)))))
    (testing "the backing pane covers the whole panel, so there are no gaps"
      (let [objs (gl/panel-objects ts 0.05 0.045)]
        (is (= 2 (count (get-in objs [:backing :faces]))))
        (is (pos? (count (get-in objs [:star :faces]))))
        (is (pos? (count (get-in objs [:accent :faces]))))
        (is (pos? (count (get-in objs [:came :faces]))))))))

(deftest backdrop-faces-the-camera
  (let [sc (gl/girih-glass-scene 0.0 {:spp 8 :half 3.6 :strips 24})
        b (get-in sc [:objects :back0])
        [a c d] (take 3 (:vertices b))
        n (g/cross (mapv - c a) (mapv - d a))]
    (testing "the emissive strip's normal points at the camera (-y), or the
              glass would transmit a dark wall"
      (is (neg? (second n))))
    (testing "hidden from camera rays by default, so there is no grey wash"
      (is (true? (:camerainvisible b))))))

(deftest sweep-is-exactly-periodic-and-moves
  (let [a (gl/sweep-emissions 0.0 24 1.2 3.0 3.0)
        b (gl/sweep-emissions 1.0 24 1.2 3.0 3.0)
        c (gl/sweep-emissions 0.5 24 1.2 3.0 3.0)]
    (is (= a b) "the backdrop sweep closes exactly")
    (is (not= a c) "and it actually moves")
    (is (= 24 (count a)))))

(deftest only-the-light-moves
  (let [a (gl/girih-glass-scene 0.0 {:spp 8})
        b (gl/girih-glass-scene 0.25 {:spp 8})]
    (is (= (:stars (:objects a)) (:stars (:objects b))) "the glass is static")
    (is (= (:camera a) (:camera b)) "the camera never moves")
    (is (= 1 (count (:lights a))) "one sun only, no flat ambient fill")
    (is (not= (get-in a [:lights :key :dir]) (get-in b [:lights :key :dir])))
    (is (not= (get-in a [:materials :backmat0 :emission])
              (get-in b [:materials :backmat0 :emission]))
        "the backdrop sweep moves")))

(deftest panel-stands-upright
  (let [objs (gl/panel-objects (gl/tiles 3.6) 0.05 0.045)
        zs (map #(nth % 2) (get-in objs [:star :vertices]))]
    (testing "the panel spans height, not depth"
      (is (< -0.5 (apply min zs)))
      (is (< 5.0 (apply max zs) 8.0))
      (is (< (apply max (map second (get-in objs [:star :vertices]))) 4.5)))))

(deftest loop-spec
  (is (= :girih-glass (:id gl/girih-glass)))
  (is (= [:girih-glass] (keys gl/loops)))
  (is (fn? (:scene gl/girih-glass)))
  (is (map? ((:scene gl/girih-glass) 0.3))))
