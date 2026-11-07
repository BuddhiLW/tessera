(ns tessera.choreography
  "Hamiltonian light: closed three-body orbits and pendulum action-angle flow.
   Fixed cameras; no globally rotating configurations."
  (:require [desargues.wallpaper.loop :as lp] [tessera.palette :as pal]))

(def period 6.32591398)
(def initial-state
  [0.97000436 -0.24308753 -0.97000436 0.24308753 0.0 0.0
   0.466203685 0.43236573 0.466203685 0.43236573 -0.93240737 -0.86473146])

(defn acceleration [s]
  (vec (mapcat
        (fn [i]
          (reduce (fn [[ax ay] j]
                    (let [dx (- (s (* 2 j)) (s (* 2 i)))
                          dy (- (s (inc (* 2 j))) (s (inc (* 2 i))))
                          r2 (+ (* dx dx) (* dy dy)) r3 (* r2 (Math/sqrt r2))]
                      [(+ ax (/ dx r3)) (+ ay (/ dy r3))]))
                  [0.0 0.0] (remove #{i} (range 3)))) (range 3))))

(defn leapfrog [s h]
  (let [a (acceleration s)
        v (mapv #(+ (s (+ 6 %)) (* 0.5 h (a %))) (range 6))
        q (mapv #(+ (s %) (* h (v %))) (range 6)) b (acceleration q)]
    (into q (mapv #(+ (v %) (* 0.5 h (b %))) (range 6)))))

(defn yoshida
  "Fourth-order symmetric composition of velocity-Verlet steps."
  [s h]
  (let [w (/ 1.0 (- 2.0 (Math/cbrt 2.0))) v (- 1.0 (* 2.0 w))]
    (-> s (leapfrog (* h w)) (leapfrog (* h v)) (leapfrog (* h w)))))

(defn energy [s]
  (- (* 0.5 (reduce + (map #(* % %) (subvec s 6))))
     (reduce + (for [i (range 3) j (range (inc i) 3)
                     :let [dx (- (s (* 2 i)) (s (* 2 j)))
                           dy (- (s (inc (* 2 i))) (s (inc (* 2 j))))]]
                 (/ 1.0 (Math/sqrt (+ (* dx dx) (* dy dy))))))))

(defn close-orbit
  "Distribute endpoint mismatch linearly across every state coordinate.
   Retain the identical first/last state explicitly for a bit-exact seam."
  [states]
  (let [n (dec (count states)) delta (mapv - (peek states) (first states))]
    (assoc (mapv (fn [i s] (mapv #(- %1 (* (/ (double i) n) %2)) s delta))
                 (range (inc n)) states)
           0 (first states) n (first states))))

(def figure-orbit
  (delay (close-orbit (vec (take 12001 (iterate #(yoshida % (/ period 12000.0)) initial-state))))))

(defn sample-orbit
  "Linear interpolation of the closed state table, with a negative-time-safe wrap.
   Clamp the index: frac(-epsilon) can round to 1.0 in floating point."
  [states u]
  (let [n (dec (count states))
        t (* n (lp/frac (double u)))
        i (min (dec n) (long (Math/floor t)))
        f (- t i)]
    (mapv #(+ (* (- 1.0 f) %1) (* f %2)) (states i) (states (inc i)))))

(defn- verlet!
  "Allocation-free velocity Verlet for the shooting solver. Arrays are local
   to an integration, never shared by renders or retained in orbit tables."
  [^doubles s ^double h]
  (dotimes [pass 2]
    (dotimes [i 3]
      (dotimes [j i]
        (let [ix (* 2 i) jx (* 2 j)
              dx (- (aget s jx) (aget s ix))
              dy (- (aget s (inc jx)) (aget s (inc ix)))
              r2 (+ (* dx dx) (* dy dy))
              f (/ (* 0.5 h) (* r2 (Math/sqrt r2)))
              fx (* f dx) fy (* f dy)]
          (aset-double s (+ 6 ix) (+ (aget s (+ 6 ix)) fx))
          (aset-double s (+ 7 ix) (+ (aget s (+ 7 ix)) fy))
          (aset-double s (+ 6 jx) (- (aget s (+ 6 jx)) fx))
          (aset-double s (+ 7 jx) (- (aget s (+ 7 jx)) fy)))))
    (when (zero? pass)
      (dotimes [i 6] (aset-double s i (+ (aget s i) (* h (aget s (+ 6 i))))))))
  s)

(defn collinear-state [[px py]]
  [-1.0 0.0 1.0 0.0 0.0 0.0 px py px py (* -2.0 px) (* -2.0 py)])

(defn integrate
  "Yoshida fourth order with G=m=1. keep? retains at most 20,001 uniformly
   spaced states for display; the endpoint map always uses every microstep."
  [state duration steps keep?]
  (let [s (double-array state) h (/ (double duration) steps)
        w (/ 1.0 (- 2.0 (Math/cbrt 2.0)))
        h1 (* h w) h0 (* h (- 1.0 (* 2.0 w)))
        stride (max 1 (quot steps 20000))]
    (when (and keep? (not (zero? (mod steps stride))))
      (throw (ex-info "Step count must be divisible by table stride" {:steps steps :stride stride})))
    (loop [i 0 out (if keep? (transient [(vec s)]) nil)]
      (if (= i steps)
        (if keep? (persistent! out) (vec s))
        (do (verlet! s h1) (verlet! s h0) (verlet! s h1)
            (recur (inc i) (if (and keep? (zero? (mod (inc i) stride)))
                            (conj! out (vec s)) out)))))))

(defn period-residual [[px py duration] steps]
  (let [s (collinear-state [px py])]
    (mapv - (integrate s duration steps false) s)))

(defn residual-norm [r] (Math/sqrt (reduce + (map #(* % %) r))))

(defn- solve-linear
  "Pivoted elimination on the three-parameter shooting normal system."
  [a b]
  (let [n (count b)]
    (loop [k 0 rows (mapv conj a b)]
      (if (= k n)
        (mapv peek rows)
        (let [pivot (apply max-key #(Math/abs (double (get-in rows [% k]))) (range k n))
              rows (assoc rows k (rows pivot) pivot (rows k))
              div (get-in rows [k k])
              row (mapv #(/ % div) (rows k))
              next (mapv (fn [i r]
                           (if (= i k) row (mapv #(- %1 (* (r k) %2)) r row)))
                         (range n) rows)]
          (recur (inc k) next))))))

(defn shoot
  "Damped Newton/least-squares correction of p1,p2,T against all twelve
   coordinates of the period map. Central differences and backtracking;
   refuses nonconvergence rather than hiding it by closure."
  [guess steps]
  (let [residual #(period-residual % steps)]
    (loop [x guess iteration 0 history []]
      (let [r (residual x) norm (residual-norm r) history (conj history norm)]
        (cond
          (< norm 1.0e-8) {:parameters x :residual norm :iterations iteration :history history}
          (>= iteration 24) (throw (ex-info "Shooting did not converge" {:parameters x :history history}))
          :else
          (let [columns (mapv (fn [j]
                                (let [eps (* 1.0e-8 (if (= j 2) (x j) 1.0))
                                      plus (residual (update x j + eps))
                                      minus (residual (update x j - eps))]
                                  (mapv #(/ (- %1 %2) (* 2.0 eps)) plus minus))) (range 3))
                dot-product (fn [a b] (reduce + (map * a b)))
                normal (mapv (fn [a] (mapv #(dot-product a %) columns)) columns)
                rhs (mapv #(- (dot-product % r)) columns)
                delta (solve-linear normal rhs)
                trust (min 1.0 (/ 0.005 (max 1.0e-20 (Math/abs (double (delta 0)))))
                           (/ 0.005 (max 1.0e-20 (Math/abs (double (delta 1)))))
                           (/ (* 0.005 (x 2)) (max 1.0e-20 (Math/abs (double (delta 2))))))
                trial (first (for [i (range 18)
                                   :let [factor (* trust (Math/pow 0.5 i))
                                         candidate (mapv #(+ %1 (* factor %2)) x delta)]
                                   :when (< (residual-norm (residual candidate)) norm)] candidate))]
            (if trial (recur trial (inc iteration) history)
                (throw (ex-info "Shooting stalled" {:parameters x :history history})))))))))

(defn verified-trajectory
  "Integrate a refined seed, measure the raw return residual and only then
   distribute the remaining mismatch. Invalid shooting results fail closed.
   Retained states include the endpoint; integration still takes microsteps."
  [{:keys [parameters steps] :as seed}]
  (let [[px py duration] parameters
        raw (integrate (collinear-state [px py]) duration steps true)
        residual (residual-norm (mapv - (peek raw) (first raw)))]
    (when (> residual 1.0e-6)
      (throw (ex-info "Periodic orbit requires further shooting refinement"
                      {:seed seed :closure-error residual})))
    (assoc seed :closure-error residual :states (close-orbit raw))))

(defn body-point [u body scale [cx cy]]
  (let [s (sample-orbit @figure-orbit u)]
    [(+ cx (* scale (s (* 2 body)))) (+ cy (* scale (s (inc (* 2 body)))))]))

(defn line [a b col width opacity]
  {:node :line :from a :to b :opacity opacity :stroke {:color col :width width}})
(defn dot [p col radius opacity]
  {:node :dot :at p :opacity opacity :opts {:radius radius} :fill {:color col :opacity 1.0}})

(defn orbit-shapes [u centre scale colors phase]
  (let [u (+ u phase)]
    (concat
     (for [j (range 240)]
       (line (body-point (/ j 240.0) 0 scale centre)
             (body-point (/ (inc j) 240.0) 0 scale centre) (first colors) 0.7 0.12))
     (mapcat
      (fn [b]
        (let [col (nth colors b) p (body-point u b scale centre)]
          (concat
           (for [j (range 100 -1 -1)
                 :let [f (- 1.0 (/ j 101.0)) t (- u (* j 0.0025))]]
             (line (body-point t b scale centre) (body-point (- t 0.0025) b scale centre)
                   col (+ 0.5 (* 3.2 f)) (* 0.85 f f)))
           [(dot p col (* scale 0.065) 0.18) (dot p col (* scale 0.032) 1.0)
            (dot p "#effaff" (* scale 0.013) 1.0)]))) (range 3)))))

(defn fit-trajectory
  "Fit every body of a trajectory into an atlas cell without rotation.
   Scene coordinates only: velocities and the conserved energy stay physical."
  [states]
  (let [coords (mapcat #(take 6 %) states)
        xs (take-nth 2 coords) ys (take-nth 2 (rest coords))
        xmin (apply min xs) xmax (apply max xs) ymin (apply min ys) ymax (apply max ys)
        scale (min (/ 5.4 (- xmax xmin)) (/ 2.95 (- ymax ymin)))]
    {:states states :scale scale :centre [(* 0.5 (+ xmax xmin)) (* 0.5 (+ ymax ymin))]}))

(def atlas-seeds
  "Šuvakov–Dmitrašinović, PRL 110, 114301 (2013). Newton-refined p1,p2,T.
   Refined using 1,600,000 microsteps, independently checked at twice that.
   :guess is the paper's rounded seed; no softening or configuration rotation."
  [{:id :butterfly-i :guess [0.30689 0.12551 6.2356]
    :parameters [0.30689342175343254 0.12550656783942127 6.234674853090286]
    :steps 1600000 :shooting-residual 3.024272633871047e-9}
   {:id :moth-i :guess [0.46444 0.39606 14.8939]
    :parameters [0.46444517280597986 0.396060014653685 14.894305174521262]
    :steps 1600000 :shooting-residual 5.160615651228338e-10}
   {:id :yin-yang-1a :guess [0.51394 0.30474 17.3284]
    :parameters [0.5139385285048248 0.3047359184826184 17.32883348627866]
    :steps 1600000 :shooting-residual 1.9814129668016275e-10}])

(def atlas-orbits
  (delay (mapv verified-trajectory atlas-seeds)))

(def atlas-fits
  (delay (into [(fit-trajectory @figure-orbit)] (map #(fit-trajectory (:states %)) @atlas-orbits))))

(defn trajectory-shapes
  "Three actual body trajectories in a stationary atlas cell. Fitting and
   background orbit outlines are precomputed; only light and particles move."
  [{:keys [states scale centre]} u location colors]
  (let [point (fn [t b]
                (let [s (sample-orbit states t)]
                  [(+ (first location) (* scale (- (s (* 2 b)) (first centre))))
                   (+ (second location) (* scale (- (s (inc (* 2 b))) (second centre))))]))]
    (concat
     (for [b (range 3) j (range 700)]
       (line (point (/ j 700.0) b) (point (/ (inc j) 700.0) b)
             (colors b) 0.65 0.16))
     (mapcat
      (fn [b]
        (let [col (colors b) p (point u b)]
          (concat
           (for [j (range 180 -1 -1)
                 :let [f (- 1.0 (/ j 181.0)) t (- u (* j 0.001))]]
             (line (point t b) (point (- t 0.001) b) col (+ 0.6 (* 2.2 f)) (* 0.85 f f)))
           [(dot p col 0.075 0.2) (dot p col 0.035 1.0) (dot p "#f5fcff" 0.014 1.0)])))
      (range 3)))))

(defn glow [_]
  [{:type :bloom :radius 12 :strength 1.45} {:type :vignette :strength 0.45}])
(def figure-eight
  {:id :figure-eight :title "Three suns, one figure eight — Chenciner–Montgomery"
   :seconds 16 :fps 30 :size [1920 1080] :view [14.2222222222 8.0]
   :frame (fn [u] {:background "#02040d"
                   :shapes (vec (orbit-shapes u [0 0] 5.4 ["#24ddff" "#ff358d" "#ffc65a"] 0))})
   :post glow})

(def choreographies
  {:id :choreographies :title "Three-body atlas — figure eight, Butterfly I, Moth I, Yin-yang 1a"
   :seconds 24 :fps 30 :size [1920 1080] :view [14.2222222222 8.0]
   :frame
   (fn [u]
     {:background "#02040d"
      :shapes
      (vec
       (mapcat
        (fn [i]
          (let [location (nth [[-3.35 1.85] [3.35 1.85] [-3.35 -1.85] [3.35 -1.85]] i)
                colors (nth [["#28cfff" "#8970ff" "#ff4ea7"]
                             ["#ff9f45" "#ffd87e" "#ff4c7a"]
                             ["#37f5bf" "#35baff" "#e8ffff"]
                             ["#bc71ff" "#fa5bc1" "#ffcc75"]] i)]
            (trajectory-shapes (nth @atlas-fits i) (+ u (* 0.173 i)) location colors)))
        (range 4)))})
   :post glow})

(defn pendulum-energy [[q p]] (- (* 0.5 p p) (Math/cos q)))
(defn pendulum-orbit
  "Uniform-time/action-angle table from midpoint quadrature dt=dq/p.
   Libration uses q=A sin(theta), regularising both turning points.
   Rotation uses q=theta-pi; direction selects the sign of its momentum.
   The separatrix (E=1, infinite period) is intentionally not sampled."
  [e direction]
  (when-not (and (> e -1.0) (not= e 1.0) (#{-1 1} direction))
    (throw (ex-info "Require E>-1, E!=1 and direction +1 or -1"
                    {:energy e :direction direction})))
  (let [n 4096 lib? (< e 1.0) a (when lib? (Math/acos (- e)))
        dtheta (/ lp/tau n)
        times (vec
               (reductions
                + 0.0
                (for [i (range n)
                      :let [theta (* dtheta (+ i 0.5))
                            q (if lib? (* a (Math/sin theta)) (- theta Math/PI))
                            speed (Math/sqrt (max 1.0e-16 (* 2.0 (+ e (Math/cos q)))))]]
                  (* dtheta (if lib? (/ (* a (Math/abs (Math/cos theta))) speed)
                                (/ 1.0 speed))))))]
    {:energy e :direction direction :libration? lib? :amplitude a
     :times times :period (peek times)}))

(defn pendulum-point
  "Invert the accumulated time to angle, then evaluate q,p on H=E.
   One phase unit is one period, including clockwise rotating trajectories."
  [{:keys [times period libration? direction amplitude energy]} phase]
  (let [t (* period (lp/frac (double (if libration? phase (* direction phase)))))
        n (dec (count times))
        i (loop [lo 0 hi n]
            (if (<= (- hi lo) 1) lo
                (let [mid (quot (+ lo hi) 2)]
                  (if (<= (times mid) t) (recur mid hi) (recur lo mid)))))
        f (/ (- t (times i)) (- (times (inc i)) (times i)))
        theta (* lp/tau (/ (+ i f) n))
        q (if libration? (* amplitude (Math/sin theta)) (- theta Math/PI))
        sign (if libration? (if (neg? (Math/cos theta)) -1 1) direction)]
    [q (* sign (Math/sqrt (max 0.0 (* 2.0 (+ energy (Math/cos q))))))]))

(def flow-orbits
  (delay (vec (concat (for [i (range 38)] (pendulum-orbit (+ -0.985 (* 1.97 (/ i 37.0))) 1))
                      (for [i (range 25) d [-1 1]] (pendulum-orbit (+ 1.025 (* 2.5 (/ i 24.0))) d))))))
(def flow-palette (pal/scheme :blue-magenta-orange))
(defn phase-point [orbit phase]
  (let [[q p] (pendulum-point orbit phase)] [(* 1.93 q) (* 1.06 p)]))

(defn phase-flow-frame [u]
  {:background "#02040d"
   :shapes
   (vec
    (concat
     ;; Infinite-period separatrix: stationary rather than fictitious flow.
     (for [sign [-1 1] i (range 360)
           :let [point (fn [j] (let [q (+ (- Math/PI) (* lp/tau (/ j 360.0)))]
                                 [(* 1.93 q) (* sign 2.12 (Math/cos (* 0.5 q)))]))]]
       (line (point i) (point (inc i)) "#ffe4a0" 1.5 0.7))
     (mapcat
      (fn [[idx orbit]]
        (let [col (format "#%06x" (pal/at flow-palette (/ (+ 1.0 (:energy orbit)) 5.0)))
              turns (inc (mod idx 3))]
          (mapcat
           (fn [j]
             (let [phase (+ (* turns u) (/ j 40.0) (* idx 0.61803398875)) p (phase-point orbit phase)]
               (concat
                (for [k [1 2 3]
                      :let [a (phase-point orbit (- phase (* k 0.003)))
                            b (phase-point orbit (- phase (* (dec k) 0.003)))]
                      :when (< (Math/abs (- (first a) (first b))) 1.0)]
                  (line a b col 1.0 (* 0.2 (- 4 k))))
                [(dot p col 0.014 0.9)]))) (range 40))))
      (map-indexed vector @flow-orbits))))})

(def phase-flow
  {:id :phase-flow :title "Pendulum phase space — quantised action-angle flow"
   :seconds 24 :fps 30 :size [1920 1080] :view [14.2222222222 8.0]
   :frame phase-flow-frame :post glow})
(def loops (array-map :figure-eight figure-eight :choreographies choreographies :phase-flow phase-flow))
