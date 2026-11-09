(ns tessera.transforms-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [desargues.wallpaper.field :as field]
            [tessera.transforms :as tx]))

(def map-factory
  (memoize
   (fn [form]
     (let [{:keys [params bindings rgb]} (field/lower form)
           ps (mapcat (fn [i] [(symbol (str "p" i)) (list 'nth 'args i)]) (range (count params)))]
       (eval (list 'fn '[args]
                   (list 'let (vec ps)
                         (list 'fn '[fx fy] (list 'let (vec (mapcat identity bindings)) rgb)))))))))

(defn map-fn
  "Evaluate the exact shared UV field without colour quantization."
  ([effect u] (map-fn effect u {}))
  ([effect u overrides]
   (let [values (merge (into {} (map (fn [[s p]] [s (:default p)]) (:params effect)))
                       {'u u 'aspect (/ 2560.0 1080.0) 'height 0.6} overrides)
         argv (mapv (comp double values) (get-in effect [:form :params]))]
     ((map-factory (:form effect)) argv))))

(defn sample-frame [effect u overrides]
  (let [f (map-fn effect u overrides)
        mirror (fn [v] (- 1.0 (Math/abs (- 1.0 (* 2.0 (- (/ v 2.0) (Math/floor (/ v 2.0))))))))]
    (vec (mapcat (fn [[x y]]
                   (let [[a b coverage] (f x y)
                         a (mirror a) b (mirror b)]
                     ;; Asymmetric, non-periodic RGBA test texture; no lucky symmetry.
                     (mapv #(* coverage %) [(* a b) (* a a) b 1.0])))
                 (for [y (range 9) x (range 16)]
                   [(/ (+ x 0.37) 16.0) (/ (+ y 0.61) 9.0)])))))

(defn seam-report
  "Native 60 fps for a 16-second loop, not an aliased 60-phase shortcut."
  ([effect] (seam-report effect {}))
  ([effect overrides]
   (let [n (* 60 (:period effect))
         frames (mapv #(sample-frame effect (/ (double %) n) overrides) (range n))
         distance (fn [a b] (/ (reduce + (map #(Math/abs (double (- %1 %2))) a b)) (count a)))
         steps (mapv distance frames (rest frames))]
     {:id (:id effect) :samples n
      :seam (distance (peek frames) (first frames))
      :step (reduce max steps)
      :closure (distance (first frames) (sample-frame effect 1.0 overrides))})))

(deftest input-effects-compile-and-close
  (doseq [e tx/suite]
    (testing (name (:id e))
      (let [text (tx/source-text e)
            header (json/read-str (second (re-find #"(?s)^/\*! trama\s*(.*?)\*/" text)))
            report (seam-report e)]
        (is (= "effect" (header "kind")))
        (is (= 1 (header "inputs")))
        (is (= ["loop_rate"] (header "rates")))
        (is (= (into ["loop_rate"] (map (comp name first) (:params e)))
               (mapv #(get % "name") (header "params"))))
        (is (not (str/includes? text "u.time")))
        (is (= 0 (:exit (tx/validate-wgsl! text))))
        (is (< (:closure report) 1.0e-10) (str report))
        (is (<= (:seam report) (:step report)) (str report))))))
