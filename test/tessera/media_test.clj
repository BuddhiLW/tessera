(ns tessera.media-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tessera.loops :as loops]
            [tessera.media :as media]))

(def manifest (media/catalog))

(def items (:items manifest))

(defn- paths [item]
  (concat (vals (:clips item)) (keep item [:thumb :preview])))

(deftest covers-every-loop
  (is (>= (count items) 64))
  (is (= (set (map name (keys loops/catalog))) (set (map :id items))))
  (testing "every loop is curated, not defaulted"
    (is (empty? (remove (set (keys media/families)) (keys loops/catalog))))
    (is (empty? (remove (set (keys media/titles)) (keys loops/catalog))))))

(deftest families-are-allowed
  (let [allowed (set media/family-set)]
    (doseq [{:keys [id family tags]} items]
      (is (allowed family) (str id " has family " family))
      (is (seq tags) (str id " has no tags"))
      (is (every? string? tags)))))

(deftest ids-are-unique
  (is (apply distinct? (map :id items))))

(deftest titles-are-short-and-present
  (doseq [{:keys [id title]} items]
    (is (and (string? title) (not (str/blank? title)) (<= (count title) 40))
        (str id ": " title))))

(deftest round-trips-as-json
  (let [back (json/read-str (json/write-str manifest) :key-fn keyword)]
    (is (= 1 (:version back)))
    (is (= (count items) (count (:items back))))
    (is (= (map #(update % :clips (partial into {})) items)
           (map #(update % :clips (partial into {})) (:items back))))))

(deftest paths-are-relative
  (doseq [item items p (paths item)]
    (is (not (.isAbsolute (io/file p))) p)
    (is (not (str/includes? p "..")) p)
    (is (str/starts-with? p (str (:id item) "/")) p)))

(deftest clips-follow-presence
  (let [reg (select-keys loops/catalog [:julia-orbit :choreographies])
        m (media/catalog reg {:julia-orbit #{:hd :thumb :preview} :choreographies #{}})
        [julia chor] (:items m)]
    (is (= {:hd "julia-orbit/clip-hd.mp4"} (:clips julia)))
    (is (= "julia-orbit/thumb.webp" (:thumb julia)))
    (is (= {} (:clips chor)) "an unrendered loop is still listed")
    (is (nil? (:thumb chor)))
    (is (= ["Orbits" "Three-Body Choreographies"] [(:family chor) (:title chor)]))))
