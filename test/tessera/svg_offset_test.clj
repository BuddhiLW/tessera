(ns tessera.svg-offset-test
  (:require [clojure.test :refer [deftest is testing]]
            [desargues.wallpaper.render :as r]
            [tessera.svg-offset :as so]
            [tessera.vj-test :as metrics]))

(def square-and-circle
  "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>
     <g fill='#ff8800'><rect x='10' y='10' width='30' height='30'/>
                       <circle cx='70' cy='30' r='15'/></g>
     <g transform='translate(0 0)'><polygon points='50,60 80,90 20,90' fill='#00ffaa'/></g>
     <path d='M 0 0 L 5 0 L 5 5 Z' fill='none'/>
   </svg>")

(defn- close? [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest shapes-are-read-in-scene-units-y-up
  (let [ss (so/shapes square-and-circle)
        [sq circ tri] ss
        k (/ 7.2 100.0)]
    (is (= 3 (count ss)) "fill none is dropped")
    (is (= [0 0 1] (map :group ss)))
    (is (= [0xff8800 0xff8800 0x00ffaa] (map :fill ss)))
    (is (close? (:area sq) (* 900 k k) 1.0e-9))
    (is (close? (:area circ) (* Math/PI 225 k k) 0.02))
    (testing "y is flipped: the triangle drawn low sits below the centre"
      (is (neg? (second (:centroid tri))))
      (is (pos? (second (:centroid sq)))))))

(deftest transforms-compose
  (let [xf (so/parse-transform "translate(10 5) rotate(90) scale(2)")]
    (is (every? true? (map #(close? %1 %2 1.0e-12) (so/apply-xf xf 1 0) [10.0 7.0])))))

(deftest signed-distance-is-exact-for-a-square
  (let [s (first (so/shapes "<svg viewBox='-1 -1 2 2'><rect x='-1' y='-1' width='2' height='2'/></svg>"
                            {:height 2.0}))
        e (#'so/edge-array (:rings s))]
    (is (close? -1.0 (so/signed-distance e 0.0 0.0) 1.0e-12))
    (is (close? -0.5 (so/signed-distance e 0.5 0.1) 1.0e-12))
    (is (close? 1.0 (so/signed-distance e 2.0 0.0) 1.0e-12))
    (is (close? (Math/sqrt 2.0) (so/signed-distance e 2.0 2.0) 1.0e-12))))

(deftest bake-finds-inradii-and-order-keys
  (let [b (so/bake (so/shapes square-and-circle))
        k (/ 7.2 100.0)
        [rs rc rt] (vec (:inradius b))]
    (is (close? rs (* 15 k) (* 2 (:cell b))))
    (is (close? rc (* 15 k) (* 2 (:cell b))))
    ;; triangle: inradius = area / semiperimeter
    (is (close? rt (* k (/ 900.0 (/ (+ 60 (* 2 (Math/hypot 30 30))) 2.0))) (* 2 (:cell b))))
    (doseq [[_ ^doubles ks] (:keys b)]
      (is (every? #(<= 0.0 % 1.0) ks)))
    (is (= [0.0 0.0 1.0] (vec (:group (:keys b)))))))

(deftest the-test-art-is-ours-and-loads
  (let [ss (so/shapes (slurp so/test-svg))]
    (is (= 17 (count ss)))
    (is (= 3 (count (distinct (map :group ss)))))))

(deftest every-parameter-combination-repeats-after-one-period
  (let [b (so/baked so/test-svg)]
    (doseq [params [{} {:motion :bounce :speed 2} {:palette :greyscale :order :vertical}
                    {:palette :white :keyer {:speed 3 :repeat 2}} {:palette :original :order :random}
                    {:spin 1 :scale 0.8 :offset [0.5 -0.3] :background :white :shrink 0.3}]
            :let [a (so/shader b params 0.0) c (so/shader b params 1.0)]]
      (testing (pr-str params)
        (doseq [x (range -4.0 4.0 0.61) y (range -3.6 3.6 0.53)]
          (is (= (a x y) (c x y))))))))

(deftest offset-loops-close-their-seam-on-a-black-ground
  (doseq [[id spec] so/loops]
    (testing (name id)
      (is (= id (:id spec)))
      (is (= [1920 1080] (:size spec)))
      (let [{:keys [seam typical-max]} (metrics/seam-stats spec [96 54])
            {:keys [near-black white-clip lit-saturation]}
            (metrics/image-stats (r/render-frame spec 0.3 {:size [160 90]}))]
        (is (<= seam typical-max))
        (is (> near-black 0.3))
        (is (< white-clip 0.01))))))
