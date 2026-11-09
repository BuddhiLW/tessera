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
        mirror (fn [v] (- 1.0 (Math/abs (- 1.0 (* 2.0 (- (/ v 2.0) (Math/floor (/ v 2.0))))))))
        texture (fn [a b]
                  (let [a (mirror a) b (mirror b)]
                    [(* a b) (* a a) b 1.0]))
        strength (get overrides 'streak_length
                      (get-in (into {} (:params effect)) ['streak_length :default] 0.0))]
    (vec (mapcat (fn [[x y]]
                   (let [[a b coverage] (f x y)]
                     (if (:streak? effect)
                       (mapv #(/ % 12.0)
                             (reduce (partial mapv +)
                                     (for [i (range 12)
                                           :let [s (Math/exp (* (- strength) (/ i 11.0)))]]
                                       (texture (+ 0.5 (* (- a 0.5) s))
                                                (+ 0.5 (* (- b 0.5) s))))))
                       (mapv #(* coverage %) (texture a b)))))
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

(deftest rectangular-stagger-and-mirror
  (let [f (map-fn tx/tile 0.0 {'columns 4.0 'rows 3.0 'sway 0.0 'row_offset 0.5})]
    (is (= [0.4 0.30000000000000004 1.0] (f 0.1 0.1)))
    (is (< (Math/abs (- 0.9 (first (f 0.1 0.4)))) 1.0e-12))
    (is (= 1.0 (last (f 0.5 0.5))))
    (is (not= (sample-frame tx/tile 0.0 {}) (sample-frame tx/tile 0.25 {})))))

(deftest ripple-modes-and-origin
  (doseq [mode [0.0 1.0]]
    (let [f (map-fn tx/ripple 0.3 {'planar mode})
          identity-f (map-fn tx/ripple 0.3 {'planar mode 'amplitude 0.0})
          r (seam-report tx/ripple {'planar mode})]
      (is (every? #(Double/isFinite (double %)) (f 0.5 0.5)))
      (is (= [0.27 0.63 1.0] (identity-f 0.27 0.63)))
      (is (< (:closure r) 1.0e-10))
      (is (<= (:seam r) (:step r)) (str r)))))
