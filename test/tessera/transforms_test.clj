(ns tessera.transforms-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [desargues.wallpaper.field :as field]
            [tessera.transforms :as tx]
            [desargues.wallpaper.wgsl :as wgsl]
            [tessera.layers :as layers]
            [desargues.wallpaper.emit :as emit]))

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

(def expected-abi
  {:tas_torus '[twist speed depth_scale rotation core]
   :tas_hyperspace '[z_speed warp density streak_length]
   :tas_tile '[columns rows row_offset sway cycles]
   :tas_ripple '[amplitude frequency cycles planar angle]
   :tas_kaleido '[folds zoom rotate rotation equirect]
   :tas_sprite_grid '[columns rows fill pulse phase_step cycles rock]
   :tas_depth '[parallax wave_depth frequency cycles angle orbit height_bias]})

(deftest published-effect-abi
  (is (= (set (keys expected-abi)) (set (map :id tx/suite))))
  (is (= (count tx/suite) (count (distinct (map :title tx/suite)))))
  (doseq [e tx/suite :let [m (tx/manifest e) text (tx/source-text e)]]
    (is (= (expected-abi (:id e)) (mapv first (:params e))))
    (is (= #{"name" "id" "kind" "inputs" "params" "rates" "integers"} (set (keys m))))
    (is (= (mapv name (:integers e)) (m "integers")))
    (is (<= (count (m "params")) 16))
    (doseq [i (range 1 (inc (count (:params e))))]
      (is (str/includes? text (str "clamp(param(" i "u),")))))
  (is (thrown? clojure.lang.ExceptionInfo
               (tx/check (assoc tx/torus :params (vec (reverse (:params tx/torus)))))))
  (is (thrown? clojure.lang.ExceptionInfo
               (tx/check (assoc-in tx/torus [:params 0 1 :default] 99.0)))))

(deftest wgsl-ir-parity-and-finite-extremes
  (doseq [e tx/suite
          overrides [{} (into {} (map (fn [[s p]] [s (:min p)]) (:params e)))
                     (into {} (map (fn [[s p]] [s (:max p)]) (:params e)))]
          u [0.0 0.31 0.99]
          :let [f (map-fn e u overrides)
                values (merge (into {} (map (fn [[s p]] [s (:default p)]) (:params e)))
                              {'u u 'aspect (/ 2560.0 1080.0) 'height 0.6} overrides)
                args (mapv values (get-in e [:form :params]))
                tree (:ok (wgsl/ir (:form e)))]
          [x y] [[0.0 0.0] [0.17 0.83] [0.5 0.5] [1.0 1.0]]]
    (let [expected (f x y)
          actual (wgsl/interpret tree x y args)
          f32 (wgsl/interpret tree x y args (comp double float))]
      (is (every? #(Double/isFinite (double %)) f32) (str (:id e) overrides u))
      (is (every? true? (map #(< (Math/abs (double (- %1 %2))) 1.0e-9) expected actual))))))

(deftest rounded-cycles-remain-periodic
  (doseq [e tx/suite
          :let [overrides (into {} (map (fn [p] [p 1.7]) (:integers e)))
                a (sample-frame e 0.0 overrides) b (sample-frame e 1.0 overrides)]]
    (is (every? true? (map #(< (Math/abs (double (- %1 %2))) 1.0e-10) a b))
        (str (:id e) " a modulated fractional cycle count must still close"))))

(deftest layer-emitter-integration
  ;; Verify orchestration without writing all expensive lattice fields.
  (let [calls (atom [])]
    (with-redefs [emit/emit-all! (fn [xs dir]
                                  (swap! calls conj [xs dir]) {:source {:ok []}})
                  tx/emit-trama! (fn [dir]
                                   (swap! calls conj [:transforms dir]) {:effect {:ok []}})]
      (is (= #{:source :effect} (set (keys (layers/emit-trama! "out/layers"))))))
    (is (= [:transforms "out/layers"] (first @calls)))
    (is (every? #(= :trama (:target %)) (first (second @calls))))
    (reset! calls [])
    (with-redefs [emit/emit-all! (fn [xs dir] (swap! calls conj [xs dir]) {})]
      (layers/emit-pfx! "out/effects"))
    (is (every? #(= :pfx (:target %)) (ffirst @calls)))
    (is (not-any? (set (map :id tx/suite)) (map :id (ffirst @calls))))))

(deftest emission-fails-before-any-file-write
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                     "tas-refused-" (make-array java.nio.file.attribute.FileAttribute 0)))
        calls (atom 0)]
    (try
      (with-redefs [tx/validate-wgsl! (fn [_]
                                      (when (= 2 (swap! calls inc))
                                        (throw (ex-info "Rejected validator fixture" {}))))]
        (is (thrown? clojure.lang.ExceptionInfo (tx/emit-trama! tx/suite (str dir))))
        (is (empty? (.listFiles dir))))
      (finally (.delete dir))))
  (is (thrown? clojure.lang.ExceptionInfo
               (tx/validate-wgsl! "@fragment fn broken( -> vec4f {}"))))
