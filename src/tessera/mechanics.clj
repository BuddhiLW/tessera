(ns tessera.mechanics
  "Mechanics as wallpaper: motions derived from Lagrangians, each exactly
   periodic over the loop. The camera is fixed: light, colour and bodies
   move, the view stays."
  (:require [desargues.kernel :as kernel]
            [desargues.wallpaper.loop :as lp]
            [emmy.env :as e]
            [emmy.expression.compile :as compile]
            [emmy.mechanics.lagrange :as lagrange]
            [emmy.special.elliptic :as ell]
            [tessera.palette :as pal]))

(set! *warn-on-reflection* true)

(defn- hex [^long c] (format "#%06x" c))

;; ── Fixed perspective camera ──────────────────────────────────────────

(defn- v- [a b] (mapv - a b))

(defn- dot3 ^double [a b] (double (reduce + (map * a b))))

(defn- cross [[a1 a2 a3] [b1 b2 b3]]
  [(- (* a2 b3) (* a3 b2)) (- (* a3 b1) (* a1 b3)) (- (* a1 b2) (* a2 b1))])

(defn- unit [v] (let [n (Math/sqrt (dot3 v v))] (mapv #(/ % n) v)))

(defn camera
  "A pinhole camera at `eye` looking at `target`, z up; `focal` is the
   distance of the image plane in scene units."
  [eye target focal]
  (let [f (unit (v- target eye))
        r (unit (cross f [0.0 0.0 1.0]))
        up (cross r f)]
    {:eye eye :f f :r r :up up :focal (double focal)}))

(defn project
  "[[sx sy] depth scale] of world point `p` seen by `cam`: scale is the
   screen size of one world unit at that depth."
  [{:keys [eye f r up focal]} p]
  (let [d (v- p eye)
        z (dot3 d f)
        s (/ (double focal) z)]
    [[(* s (dot3 d r)) (* s (dot3 d up))] z s]))

;; ── The simple pendulum, from its Lagrangian ────────────────────────────

(defn pendulum-lagrangian
  "L(t, th, th') = 1/2 m l^2 th'^2 + m g l cos th: a bob of mass `m` on a rod
   of length `l` in gravity `g`, th from the downward vertical."
  [m g l]
  (fn [[_ th thd]]
    (e/+ (e/* 1/2 m l l (e/square thd)) (e/* m g l (e/cos th)))))

(def pendulum-kernel
  "Emmy's state derivative of `pendulum-lagrangian`, compiled to a primitive
   kernel (fn [in out ps]): in = [t th th'], out = [1 th' th''], ps = [m g l]."
  (delay
   (kernel/compile-jvm
    (compile/compile-state-fn
     (fn [m g l] (lagrange/Lagrangian->state-derivative (pendulum-lagrangian m g l)))
     [1.0 1.0 1.0] (e/up 0.0 0.0 0.0)
     {:simplify? true :calling-convention :primitive :mode :source}))))

(defn pendulum-period
  "The exact period of a pendulum of length `l` released from rest at
   amplitude `th0`: 4 sqrt(l/g) K(k), k = sin(th0/2)."
  ^double [^double g ^double l ^double th0]
  (* 4.0 (Math/sqrt (/ l g)) (ell/elliptic-k (Math/sin (/ th0 2.0)))))

(defn length-for-period
  "The length whose period at amplitude `th0` is `T`."
  ^double [^double g ^double T ^double th0]
  (let [q (/ T (* 4.0 (ell/elliptic-k (Math/sin (/ th0 2.0)))))]
    (* g q q)))

(defn swing
  "One period of the pendulum of length `l` released from rest at `th0`,
   integrated by RK4 on `pendulum-kernel` in `steps` equal steps: {:T :dt
   :th :w}, the angle and angular velocity at each step, steps + 1 samples."
  [^double g ^double l ^double th0 steps]
  (let [^clojure.lang.IFn k @pendulum-kernel
        steps (long steps)
        T (pendulum-period g l th0)
        dt (/ T steps)
        ps (double-array [1.0 g l])
        in (double-array 3) out (double-array 3)
        acc (fn ^double [^double th ^double w]
              (aset in 1 th) (aset in 2 w) (k in out ps) (aget out 2))
        ths (double-array (inc steps)) ws (double-array (inc steps))]
    (loop [i 0 th th0 w 0.0]
      (aset ths i th) (aset ws i w)
      (when (< i steps)
        (let [h (* 0.5 dt)
              a1 (acc th w)
              w2 (+ w (* h a1)) a2 (acc (+ th (* h w)) w2)
              w3 (+ w (* h a2)) a3 (acc (+ th (* h w2)) w3)
              w4 (+ w (* dt a3)) a4 (acc (+ th (* dt w3)) w4)]
          (recur (inc i)
                 (+ th (* (/ dt 6.0) (+ w (* 2.0 w2) (* 2.0 w3) w4)))
                 (+ w (* (/ dt 6.0) (+ a1 (* 2.0 a2) (* 2.0 a3) a4)))))))
    {:T T :dt dt :th ths :w ws}))

(defn swing-at
  "[th w] of swing `s` at phase `p` of its period (cubic Hermite between the
   integrated steps, so the angle is smooth and p = 0 and p = 1 agree)."
  [{:keys [^double dt ^doubles th ^doubles w]} ^double p]
  (let [n (dec (alength th))
        x (* (lp/frac p) n)
        i (min (dec n) (long x))
        t (- x i)
        t2 (* t t) t3 (* t2 t)
        y0 (aget th i) y1 (aget th (inc i))
        m0 (* dt (aget w i)) m1 (* dt (aget w (inc i)))]
    [(+ (* (+ (* 2.0 t3) (* -3.0 t2) 1.0) y0) (* (+ t3 (* -2.0 t2) t) m0)
        (* (+ (* -2.0 t3) (* 3.0 t2)) y1) (* (- t3 t2) m1))
     (+ (* (- 1.0 t) (aget w i)) (* t (aget w (inc i))))]))

;; ── Pendulum wave ────────────────────────────────────────────────────────────

(def wave-params
  "Pendulum wave: `n` pendulums, pendulum k swinging `base` + k times per
   loop of `seconds`, all released from rest at amplitude `th0` (radians,
   large enough that the small-angle period would be wrong by ~3%), pivots
   spaced along x over `width` metres, g in m/s^2."
  {:n 24 :base 18 :seconds 30 :th0 0.7 :width 1.6 :g 9.81 :steps 1024})

(defn wave-pendulums
  "The pendulums of `params`: per pendulum {:k :swings :l :T :swing :x}."
  [{:keys [n base seconds th0 width g steps]}]
  (vec (for [k (range n)
             :let [swings (+ base k)
                   T (/ (double seconds) swings)
                   l (length-for-period g T th0)]]
         {:k k :swings swings :l l :T T
          :x (* width (- (/ (double k) (dec n)) 0.5))
          :swing (swing g l th0 steps)})))

(def pendulums (delay (wave-pendulums wave-params)))

(defn bob-position
  "World position of pendulum `pd`'s bob at loop phase `u`: pivots along x
   at height 0, each swinging in its own y-z plane."
  [{:keys [x l swings swing]} u]
  (let [[th w] (swing-at swing (* (double swings) (double u)))]
    [[x (* l (Math/sin th)) (- (* l (Math/cos th)))] w]))

(def wave-camera
  "Fixed oblique view: from the front-left, a little above the pivots."
  (camera [-1.7 -2.1 0.7] [-0.15 0.0 -0.4] 17.0))

(defn wave-frame
  "The pendulum wave at phase `u`: a faint rail, thin strings, glowing bobs
   coloured along the row, each bob trailing faint after-images; drawn back
   to front."
  [u {:keys [palette trail trail-dt bob] :or {trail 14 trail-dt 0.0016 bob 0.022}}]
  (let [cam wave-camera
        pds @pendulums
        n (count pds)
        P (fn [p] (project cam p))
        [[ax ay] _ _] (P [-0.85 0.0 0.0])
        [[bx by] _ _] (P [0.85 0.0 0.0])
        rail {:node :line :from [ax ay] :to [bx by] :opacity 0.5
              :stroke {:color "#6d7cff" :width 1.6}}
        per (for [{:keys [k x] :as pd} pds
                  :let [col (hex (pal/at palette (/ (double k) n)))
                        [p w] (bob-position pd u)
                        [[sx sy] z s] (P p)
                        [[px py] _ _] (P [x 0.0 0.0])
                        speed (min 1.0 (/ (Math/abs (double w)) 3.0))]]
              {:z z
               :shapes (concat
                        [{:node :line :from [px py] :to [sx sy] :opacity 0.35
                          :stroke {:color "#9fb4ff" :width 0.9}}]
                        (for [j (range trail 0 -1)
                              :let [[q _] (bob-position pd (- (double u) (* j trail-dt)))
                                    [[qx qy] _ qs] (P q)
                                    f (- 1.0 (/ (double j) (inc trail)))]]
                          {:node :dot :at [qx qy] :opacity (* 0.22 f f)
                           :opts {:radius (* bob 0.8 qs)} :fill {:color col :opacity 1.0}})
                        [{:node :dot :at [sx sy] :opacity 1.0 :opts {:radius (* bob s 1.9)}
                          :fill {:color col :opacity (+ 0.12 (* 0.18 speed))}}
                         {:node :dot :at [sx sy] :opacity 1.0 :opts {:radius (* bob s)}
                          :fill {:color col :opacity 1.0}}
                         {:node :dot :at [sx sy] :opacity (+ 0.5 (* 0.5 speed)) :opts {:radius (* bob s 0.45)}
                          :fill {:color "#fff6ea" :opacity 1.0}}])})]
    {:background "#000000"
     :shapes (vec (cons rail (mapcat :shapes (sort-by (comp - :z) per))))}))

(def pendulum-wave
  (let [p (pal/around (pal/scheme :blue-magenta-orange))]
    {:id :pendulum-wave
     :title "Pendulum wave: 24 nonlinear pendulums, 18 to 41 swings per loop, exact elliptic periods"
     :seconds (:seconds wave-params) :fps 30 :size [1920 1080]
     :frame (fn [u] (wave-frame u {:palette p}))
     :post (fn [_] [{:type :bloom :radius 14 :strength 1.4}
                    {:type :chroma :shift 2}
                    {:type :vignette :strength 0.6}])}))

;; ── Kepler resonance ───────────────────────────────────────────────────────

(defn eccentric-anomaly
  "E solving Kepler's equation M = E - e sin E, by Newton iteration from
   E = M (or pi for e > 0.8)."
  ^double [^double M ^double e]
  (loop [E (if (> e 0.8) Math/PI M) i 0]
    (let [d (/ (- E (* e (Math/sin E)) M) (- 1.0 (* e (Math/cos E))))
          E' (- E d)]
      (if (or (< (Math/abs d) 1e-13) (>= i 40)) E' (recur E' (inc i))))))

(defn orbit-point
  "Position at mean anomaly `M` on the Kepler ellipse of semi-major axis `a`
   and eccentricity `e`, the focus at the origin, in the orbit's plane."
  [^double a ^double e ^double M]
  (let [E (eccentric-anomaly M e)]
    [(* a (- (Math/cos E) e)) (* a (Math/sqrt (- 1.0 (* e e))) (Math/sin E))]))

(defn orbit->world
  "Plane point [x y] of an orbit with argument of periapsis `w`,
   inclination `i` and ascending node `node` (radians) in world space, z up."
  [[x y] w i node]
  (let [cw (Math/cos w) sw (Math/sin w)
        x1 (- (* cw x) (* sw y)) y1 (+ (* sw x) (* cw y))
        ci (Math/cos i) si (Math/sin i)
        y2 (* ci y1) z2 (* si y1)
        cn (Math/cos node) sn (Math/sin node)]
    [(- (* cn x1) (* sn y2)) (+ (* sn x1) (* cn y2)) z2]))

(def orbits
  "Six bodies about one sun. :n is whole revolutions per loop, so Kepler's
   third law a ~ n^(-2/3) fixes each semi-major axis; the period ratios are
   the resonance chain 12:6:4:3:2:1."
  (vec (for [[k [n e w i node]] (map-indexed vector
                                             [[12 0.05 0.3 0.10 0.0]
                                              [6 0.12 1.9 0.22 0.7]
                                              [4 0.20 4.0 0.08 2.1]
                                              [3 0.10 2.6 0.30 4.0]
                                              [2 0.32 0.9 0.16 5.1]
                                              [1 0.45 3.4 0.26 1.3]])]
         {:k k :n n :e e :w w :i i :node node
          :a (* 4.2 (Math/pow (double n) (/ -2.0 3.0)))
          :M0 (* lp/tau (/ (double k) 6.0))})))

(defn body-at
  "World position of orbit `o` at loop phase `u`."
  [{:keys [n a e w i node M0]} u]
  (orbit->world (orbit-point a e (+ M0 (* lp/tau (double n) (double u)))) w i node))

(def kepler-camera
  "Fixed view of the system from a little above its plane."
  (camera [0.0 -11.0 3.4] [0.0 0.0 0.15] 21.0))

(def stars
  "A fixed star field: [x y radius twinkle-rate twinkle-phase] in screen
   units, rates whole numbers per loop."
  (let [rng (java.util.Random. 7)]
    (vec (repeatedly 260 (fn [] [(* 14.4 (- (.nextDouble rng) 0.5)) (* 8.2 (- (.nextDouble rng) 0.5))
                                 (+ 0.006 (* 0.016 (Math/pow (.nextDouble rng) 3.0)))
                                 (inc (.nextInt rng 3)) (.nextDouble rng)])))))

(defn kepler-frame
  "The resonant system at phase `u`: a star field, faint orbit lines, a
   glowing sun, and each body a comet whose tail runs back along its orbit
   over `tail` of a revolution, fading; drawn back to front."
  [u {:keys [palette tail segs] :or {tail 0.16 segs 26}}]
  (let [cam kepler-camera
        P (fn [p] (project cam p))
        sky (for [[x y r k ph] stars]
              {:node :dot :at [x y] :opacity (+ 0.25 (* 0.5 (lp/wave (+ (* k (double u)) ph))))
               :opts {:radius r} :fill {:color "#c9d6ff" :opacity 1.0}})
        lines (for [{:keys [k] :as o} orbits
                    :let [col (hex (pal/at palette (/ (double k) (count orbits))))
                          pts (mapv #(first (P (body-at (assoc o :n 1) (/ (double %) 180))))
                                    (range 181))]]
                {:node :path :points pts :opacity 0.22 :stroke {:color col :width 1.2}})
        [[sx sy] sz ss] (P [0.0 0.0 0.0])
        sun {:z sz
             :shapes (for [[r o c] [[0.55 0.10 "#ff9a3c"] [0.32 0.22 "#ffb35c"] [0.17 0.7 "#ffd9a0"] [0.09 1.0 "#fff8ec"]]]
                       {:node :dot :at [sx sy] :opacity o :opts {:radius (* r ss)} :fill {:color c :opacity 1.0}})}
        bodies (for [{:keys [k n] :as o} orbits
                     :let [col (hex (pal/at palette (/ (double k) (count orbits))))
                           span (/ (double tail) n)
                           pts (mapv #(P (body-at o (- (double u) (* span (/ (double %) segs))))) (range (inc segs)))
                           [[hx hy] hz hs] (first pts)]]
                 {:z hz
                  :shapes (concat
                           (for [j (range segs 0 -1)
                                 :let [[[x0 y0] _ s0] (nth pts j) [[x1 y1] _ _] (nth pts (dec j))
                                       f (- 1.0 (/ (double j) segs))]]
                             {:node :line :from [x0 y0] :to [x1 y1] :opacity (* 0.85 f f)
                              :stroke {:color col :width (* 30.0 s0 (+ 0.15 (* 0.85 f)) 0.05)}})
                           [{:node :dot :at [hx hy] :opacity 0.25 :opts {:radius (* 0.16 hs)} :fill {:color col :opacity 1.0}}
                            {:node :dot :at [hx hy] :opacity 1.0 :opts {:radius (* 0.07 hs)} :fill {:color col :opacity 1.0}}
                            {:node :dot :at [hx hy] :opacity 0.9 :opts {:radius (* 0.032 hs)} :fill {:color "#fffaf0" :opacity 1.0}}])})]
    {:background "#000000"
     :shapes (vec (concat sky lines (mapcat :shapes (sort-by (comp - :z) (cons sun bodies)))))}))

(def kepler-resonance
  (let [p (pal/around (pal/scheme :orange-blue))]
    {:id :kepler-resonance
     :title "Kepler resonance: six inclined orbits, periods 1:2:3:4:6:12, Kepler's equation by Newton"
     :seconds 40 :fps 30 :size [1920 1080]
     :frame (fn [u] (kepler-frame u {:palette p}))
     :post (fn [_] [{:type :bloom :radius 16 :strength 1.3}
                    {:type :chroma :shift 2}
                    {:type :vignette :strength 0.55}])}))

;; ── Lissajous oscillator ───────────────────────────────────────────────────

(defn oscillator-lagrangian
  "L = 1/2 |q'|^2 - 1/2 sum w_j^2 q_j^2: an anisotropic 3D harmonic
   oscillator with angular frequencies `ws`."
  [ws]
  (fn [[_ q qd]]
    (e/- (e/* 1/2 (e/dot-product qd qd))
         (e/* 1/2 (reduce e/+ (map (fn [w x] (e/* w w x x)) ws q))))))

(defn oscillator-equations
  "Emmy's Lagrange equations of `oscillator-lagrangian` applied to the
   closed-form motion q_j = A_j cos(w_j t + p_j), simplified: the zero
   residual is what makes the cosine the motion."
  [ws]
  (let [q (apply e/up (map (fn [w j] (fn [t] (e/* (e/literal-number (symbol (str "A" j)))
                                                    (e/cos (e/+ (e/* w t) (e/literal-number (symbol (str "p" j))))))))
                          ws (range)))]
    (e/simplify (((lagrange/Lagrange-equations (oscillator-lagrangian ws)) q) 't))))

(def bodies
  "Three bodies, each a 3D oscillator with whole frequencies per loop, so the
   figure closes: [ws amplitudes phases]."
  [[[3 4 5] [1.9 1.6 1.1] [0.0 0.5 1.1]]
   [[2 3 4] [2.6 1.2 1.3] [0.9 0.0 0.3]]
   [[5 4 3] [1.3 2.0 0.9] [0.4 1.6 0.0]]])

(defn oscillator-at
  "Position of body [ws as ps] at loop phase `u`."
  [[ws as ps] u]
  (mapv (fn [w a p] (* a (Math/cos (+ (* lp/tau w (double u)) p)))) ws as ps))

(def lissajous-camera (camera [0.0 -8.5 3.0] [0.0 0.0 0.0] 15.0))

(defn lissajous-frame
  "The three closed figures at phase `u`, faint, with a bead of light per
   body running along each and a fading ribbon behind it."
  [u {:keys [palette tail segs] :or {tail 0.05 segs 40}}]
  (let [P (fn [p] (project lissajous-camera p))
        nb (count bodies)
        figs (for [[k b] (map-indexed vector bodies)
                   :let [col (hex (pal/at palette (/ (double k) nb)))]]
               {:node :path :points (mapv #(first (P (oscillator-at b (/ (double %) 600)))) (range 601))
                :opacity 0.3 :stroke {:color col :width 1.4}})
        heads (for [[k b] (map-indexed vector bodies)
                    off [0.0 (/ 1.0 3) (/ 2.0 3)]
                    :let [col (hex (pal/at palette (+ (/ (double k) nb) (* 0.3 off))))
                          pts (mapv #(P (oscillator-at b (- (+ (double u) off) (* tail (/ (double %) segs))))) (range (inc segs)))
                          [[hx hy] _ hs] (first pts)]
                    s (concat
                       (for [j (range segs 0 -1)
                             :let [[[x0 y0] _ s0] (nth pts j) [[x1 y1] _ _] (nth pts (dec j))
                                   f (- 1.0 (/ (double j) segs))]]
                         {:node :line :from [x0 y0] :to [x1 y1] :opacity (* 0.9 f f)
                          :stroke {:color col :width (* 2.6 s0 (+ 0.2 (* 0.8 f)))}})
                       [{:node :dot :at [hx hy] :opacity 0.3 :opts {:radius (* 0.09 hs)} :fill {:color col :opacity 1.0}}
                        {:node :dot :at [hx hy] :opacity 1.0 :opts {:radius (* 0.035 hs)} :fill {:color "#fffaf0" :opacity 1.0}}])]
                s)]
    {:background "#000000" :shapes (vec (concat figs heads))}))

(def lissajous-oscillators
  (let [p (pal/around (pal/scheme :yellow-magenta-cyan))]
    {:id :lissajous-oscillators
     :title "Lissajous oscillators: three 3D harmonic oscillators, whole frequencies, closed figures"
     :seconds 30 :fps 30 :size [1920 1080]
     :frame (fn [u] (lissajous-frame u {:palette p}))
     :post (fn [_] [{:type :bloom :radius 14 :strength 1.4}
                    {:type :chroma :shift 2}
                    {:type :vignette :strength 0.55}])}))

(def loops
  "The mechanics loops, by id."
  (array-map :pendulum-wave pendulum-wave
             :kepler-resonance kepler-resonance
             :lissajous-oscillators lissajous-oscillators))
