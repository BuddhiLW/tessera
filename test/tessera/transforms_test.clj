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
  (let [mirror (fn [v] (- 1.0 (Math/abs (- 1.0 (* 2.0 (- (/ v 2.0) (Math/floor (/ v 2.0))))))))
        texture (fn [a b]
                  (let [a (mirror a) b (mirror b)]
                    [(* a b) (* a a) b 1.0]))
        raw-factory (map-factory (:form effect))
        base (merge (into {} (map (fn [[s p]] [s (:default p)]) (:params effect)))
                    {'u u 'aspect (/ 2560.0 1080.0) 'height 0.6} overrides)
        base-args (mapv (comp double base) (get-in effect [:form :params]))
        f (raw-factory base-args)
        strength (get base 'streak_length 0.0)]
    (vec (mapcat (fn [[x y]]
                   (let [map-point (if (:height? effect)
                                     (raw-factory (assoc base-args (dec (count base-args))
                                                        (reduce + (map * (take 3 (texture x y))
                                                                       [0.2126 0.7152 0.0722])))) f)
                         [a b coverage] (map-point x y)]
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

(deftest kaleido-equirect-and-polar-folds
  (doseq [mode [0.0 1.0]]
    (let [r (seam-report tx/kaleido {'equirect mode})]
      (is (< (:closure r) 1.0e-10) (str r))
      (is (<= (:seam r) (:step r)) (str r))))
  (let [f (map-fn tx/kaleido 0.23 {'equirect 1.0 'folds 7.0})
        delta (fn [a b] (reduce max (map #(Math/abs (double (- %1 %2))) a b)))]
    (doseq [y [0.0 0.13 0.5 0.87 1.0]]
      (is (< (delta (f 0.0 y) (f 1.0 y)) 1.0e-12)))
    (doseq [y [0.0 1.0]]
      (is (< (delta (f 0.12 y) (f 0.78 y)) 1.0e-12)))
    (is (< (delta (f 0.37 0.2) (f 0.37 0.8)) 1.0e-12)))
  (let [f (map-fn tx/kaleido 0.0 {'aspect 1.0 'folds 6.0})
        point (fn [a] [(+ 0.5 (* 0.3 (Math/cos a))) (+ 0.5 (* 0.3 (Math/sin a)))])
        a (apply f (point 0.2)) b (apply f (point (+ 0.2 (/ (* 2.0 Math/PI) 6.0))))]
    (is (every? true? (map #(< (Math/abs (double (- %1 %2))) 1.0e-7) a b)))))

(deftest sprite-cell-phase-and-coverage
  (let [overrides {'columns 4.0 'rows 3.0 'rock 0.0}
        f (map-fn tx/sprite-grid 0.0 overrides)
        center (f 0.125 (/ 0.5 3.0))
        gap (f 0.25 (/ 0.5 3.0))
        a (f 0.15 (/ 0.5 3.0)) b (f 0.4 (/ 0.5 3.0))]
    (is (= [0.5 0.5 1.0] center))
    (is (= 0.0 (last gap)))
    (is (not= (first a) (first b)) "cell phase changes sprite scale, not upstream time")
    (is (every? #(<= 0.0 % 1.0) (map last [center gap a b])))))

(deftest luminance-drives-parallax
  (let [base {'wave_depth 0.0 'orbit 0.0 'angle 0.0 'height_bias 0.5}
        black (map-fn tx/depth 0.0 (assoc base 'height 0.0))
        white (map-fn tx/depth 0.0 (assoc base 'height 1.0))
        flat (map-fn tx/depth 0.0 (assoc base 'parallax 0.0))]
    (is (< (first (black 0.4 0.6)) 0.4 (first (white 0.4 0.6))))
    (is (= 0.6 (second (black 0.4 0.6))))
    (is (= [0.4 0.6 1.0] (flat 0.4 0.6)))
    (is (str/includes? (tx/source-text tx/depth) "dot(tas_sample(uv).rgb"))))
