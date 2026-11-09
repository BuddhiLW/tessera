(ns tessera.layers.ports.fourier
  "An unrolled analytic Fourier field, within the <=24 harmonic budget.
   The native-parameter trefoil projection has just TWO nonzero complex
   harmonics. Its locus is the same as the arc-length offline trefoil,
   with a nonuniform pen speed. No sampled points, line-segment trail,
   texture, mutable history, runtime loop or array enters the field AST.

   Rings/rods are exact SDFs. The analytic trail's tube SDF is estimated by
   twelve fixed seeds and four bounded Gauss-Newton steps on the Fourier
   curve itself. It is a closest-point distance estimator, not an exact
   global signed distance oracle (notably at crossings / far from the tube)."
  (:require [tessera.figures :as figures]
            [tessera.fourier :as fourier]
            [tessera.layers.ports.forms :as forms]))

(def max-terms 24)
(def seed-count 12)
(def refinement-steps 4)

(def terms
  "DFT of the native trefoil parameterization; drop only numerical noise."
  (mapv (fn [term]
          (reduce (fn [t k] (update t k #(mapv (fn [x] (/ (double (Math/round (* 1.0e12 x))) 1.0e12)) %)))
                  term [:cos :sin]))
        (fourier/top-terms (fourier/dft (mapv #(subvec (figures/trefoil (/ % 64.0)) 0 2) (range 64))) 2)))

(defn- sym [prefix suffix] (symbol (str prefix "_" suffix)))
(defn- add [xs] (forms/sum-of xs))

(defn- geometry
  "Unique scalar let bindings for C(a) and dC/da, with a in radians."
  [a prefix]
  (let [trig (mapv (fn [i {:keys [frequency]}]
                     [(sym prefix (str "c" i)) (list 'cos (list '* frequency a))
                      (sym prefix (str "s" i)) (list 'sin (list '* frequency a))])
                   (range) terms)
        coordinate (fn [axis derivative?]
                     (add (map (fn [term [c _ s _]]
                                 (let [ac (nth (:cos term) axis) bs (nth (:sin term) axis) k (:frequency term)]
                                   (if derivative?
                                     (list '* k (list '+ (list '* (- ac) s) (list '* bs c)))
                                     (list '+ (list '* ac c) (list '* bs s))))) terms trig)))
        x (sym prefix "x") y (sym prefix "y") dx (sym prefix "dx") dy (sym prefix "dy")]
    {:bindings (into (vec (mapcat identity trig))
                     [x (coordinate 0 false) y (coordinate 1 false)
                      dx (coordinate 0 true) dy (coordinate 1 true)])
     :x x :y y :dx dx :dy dy}))

(defn- gaussian [d width]
  (list 'let ['q (list '/ d width)] '(exp (- (* q q)))))

(defn- candidate
  "Unroll a bounded closest-point solve, then evaluate the analytic tube."
  [seed]
  (let [prefix (str "near" seed)
        {:keys [bindings angle]}
        (reduce (fn [{:keys [bindings angle]} i]
                  (let [p (str prefix "_" i)
                        g (geometry angle p) a (sym p "a")
                        {:keys [x y dx dy]} g]
                    {:bindings (into bindings
                                     (concat (:bindings g)
                                             [a (list '- angle
                                                      (list 'clamp
                                                            (list '/ (list '+ (list '* (list '- x 'x) dx)
                                                                                (list '* (list '- y 'y) dy))
                                                                  (list 'max 1.0e-9 (list '+ (list '* dx dx) (list '* dy dy))))
                                                            -0.4 0.4))]))
                     :angle a}))
                {:bindings [] :angle (* forms/tau (/ seed (double seed-count)))}
                (range refinement-steps))
        g (geometry angle (str prefix "_end"))
        d (sym prefix "distance") age (sym prefix "age") light (sym prefix "light")
        ex (list '- 'x (:x g)) ey (list '- 'y (:y g))]
    {:bindings (into bindings
                     (concat (:bindings g)
                             [d (list '- (list 'sqrt (list '+ (list '* ex ex) (list '* ey ey))) 'trail_width)
                              age (list 'frac (list '- 'u (list '/ angle 'tau)))
                              light (list '* (list 'pow (list 'max 0.0 (list '- 1.0 (list '/ age 'tail_length))) 2.0)
                                          (gaussian (list 'max 0.0 d) 'trail_width))]))
     :light light}))

(defn- circle-chain
  "The instantaneous chain as scalar bindings, exact circle/rod SDFs."
  []
  (reduce
   (fn [{:keys [bindings x y rings rods]} [i term]]
     (let [p (str "rod" i) a (sym p "a") c (sym p "c") s (sym p "s")
           nx (sym p "x") ny (sym p "y")
           vx (sym p "vx") vy (sym p "vy") h (sym p "h")
           rd (sym p "ring") sd (sym p "sdf")
           [ax ay] (:cos term) [bx by] (:sin term)
           ex (list '- 'x x) ey (list '- 'y y)
           qx (list '- ex (list '* h vx)) qy (list '- ey (list '* h vy))]
       {:bindings (into bindings
                        [a (list '* (:frequency term) 'phase) c (list 'cos a) s (list 'sin a)
                         nx (list '+ x (list '* ax c) (list '* bx s))
                         ny (list '+ y (list '* ay c) (list '* by s))
                         rd (list '- (list 'abs (list '- (list 'sqrt (list '+ (list '* ex ex) (list '* ey ey))) (:magnitude term))) 'ring_width)
                         vx (list '- nx x) vy (list '- ny y)
                         h (list 'clamp (list '/ (list '+ (list '* ex vx) (list '* ey vy))
                                              (list 'max 1.0e-9 (list '+ (list '* vx vx) (list '* vy vy)))) 0.0 1.0)
                         sd (list '- (list 'sqrt (list '+ (list '* qx qx) (list '* qy qy))) 0.009)])
        :x nx :y ny :rings (conj rings rd) :rods (conj rods sd)}))
   {:bindings [] :x 0.0 :y 0.0 :rings [] :rods []}
   (map-indexed vector terms)))

(defn field-form
  "Fixed-size field AST; 2 <= max-terms 24, all solvers unrolled at build time."
  []
  (assert (<= (count terms) max-terms))
  (let [candidates (mapv candidate (range seed-count))
        chain (circle-chain)
        minimum (fn [xs] (reduce #(list 'min %1 %2) xs))
        maximum (fn [xs] (reduce #(list 'max %1 %2) xs))
        hx (list '- 'x (:x chain)) hy (list '- 'y (:y chain))]
    {:params '[u trail_width ring_width tail_length ring_gain]
     :body
     (list 'let
           (vec (concat ['phase '(* tau u)]
                        (mapcat :bindings candidates)
                        (:bindings chain)
                        ['trail_light (maximum (map :light candidates))
                         'rings (gaussian (list 'max 0.0 (minimum (:rings chain))) 'ring_width)
                         'rods (gaussian (list 'max 0.0 (minimum (:rods chain))) 0.014)
                         'head (gaussian (list 'sqrt (list '+ (list '* hx hx) (list '* hy hy))) 0.06)]))
           '[(+ (* 0.10 trail_light) (* 0.65 ring_gain rings) (* 0.6 rods) head)
             (+ (* 0.88 trail_light) (* 0.15 ring_gain rings) (* 0.32 rods) (* 0.80 head))
             (+ trail_light (* 0.85 ring_gain rings) (* 0.05 rods) (* 0.45 head))])}))

(def fourier-live
  {:id :fourier_live
   :title "Analytic Fourier trefoil: SDF rings, rods and a fading pen trail"
   :period 20 :view fourier/view :blend :screen
   ;; Append-only ABI: loop_rate is supplied by the host at slot zero.
   :params [['trail_width {:default 0.025 :min 0.008 :max 0.12 :label "Trail tube radius"}]
            ['ring_width {:default 0.009 :min 0.003 :max 0.05 :label "Epicycle ring width"}]
            ['tail_length {:default 0.85 :min 0.1 :max 1.0 :label "Trail length (turns)"}]
            ['ring_gain {:default 0.25 :min 0.0 :max 1.0 :label "Epicycle ring light"}]]
   :form (field-form)})

(def effects
  {:fourier_live {:name "Fourier Epicycles" :description "Two integer harmonics of a trefoil: analytic SDF trail, epicycle rings and rods."}})

(defn closest-candidates
  "Independent numeric reference for the unrolled analytic distance solver.
   Returns [{:distance :angle}] for all seeds; distance is to the centreline."
  [x y]
  (let [eval-curve (fn [a]
                     (reduce (fn [g {:keys [frequency cos sin]}]
                               (let [c (Math/cos (* frequency a)) s (Math/sin (* frequency a))]
                                 (mapv + g [(+ (* (cos 0) c) (* (sin 0) s))
                                            (+ (* (cos 1) c) (* (sin 1) s))
                                            (* frequency (+ (* (- (cos 0)) s) (* (sin 0) c)))
                                            (* frequency (+ (* (- (cos 1)) s) (* (sin 1) c)))])))
                             [0.0 0.0 0.0 0.0] terms))]
    (mapv (fn [seed]
            (let [a (reduce (fn [a _]
                              (let [[px py dx dy] (eval-curve a)
                                    delta (/ (+ (* (- px x) dx) (* (- py y) dy)) (max 1.0e-9 (+ (* dx dx) (* dy dy))))]
                                (- a (max -0.4 (min 0.4 delta)))))
                            (* forms/tau (/ seed (double seed-count))) (range refinement-steps))
                  [px py] (eval-curve a)]
              {:angle a :distance (Math/hypot (- x px) (- y py))}))
          (range seed-count))))
