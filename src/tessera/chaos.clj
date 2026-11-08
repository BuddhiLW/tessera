(ns tessera.chaos
  "Hamiltonian chaos and billiards as seamless loops. Exact periodicity is
   the design constraint, so every system here is one where it is natural.

     :cat-map   Arnold's cat map on a 144 x 144 torus of glowing points. The
                discrete map has period exactly 12 on that grid, so a rosette
                scrambles into stripes and noise and reassembles bit-exactly.
                Each step is drawn as its two shears, slid continuously.

   Every point position is an integer lattice state plus a partial shear,
   both functions of the loop phase u alone."
  (:require [desargues.wallpaper.loop :as lp]
            [tessera.palette :as pal]))

(set! *warn-on-reflection* true)

(defn- channels
  "[r g b] in [0, 1] of 0xRRGGBB `c`."
  [^long c]
  [(/ (double (bit-and (bit-shift-right c 16) 0xFF)) 255.0)
   (/ (double (bit-and (bit-shift-right c 8) 0xFF)) 255.0)
   (/ (double (bit-and c 0xFF)) 255.0)])

;; ── Arnold's cat map ─────────────────────────────────────────────────────

(defn cat-period
  "The least k > 0 with C^k = I mod `n`, C = [[2 1] [1 1]] the cat map."
  [n]
  (let [n (long n)]
    (loop [k 1 a 2 b 1 c 1 d 1]
      (if (and (= 1 (mod a n)) (zero? (mod b n)) (zero? (mod c n)) (= 1 (mod d n)))
        k
        (recur (inc k)
               (long (mod (+ (* 2 a) b) n)) (long (mod (+ a b) n))
               (long (mod (+ (* 2 c) d) n)) (long (mod (+ c d) n)))))))

(def cat-n
  "Grid side: on 144 x 144 the cat map returns every point after 12 steps
   (and after 6 it is multiplication by 89, a ghost of the picture)."
  144)

(defn rosette
  "Brightness in [0, 1] of the torus picture at (x, y) in [0, 1)^2: two
   eight-pointed star rosettes, one at the centre and one at the corner, so
   the picture tiles. Each is a star outline, a bead ring and a rim."
  ^double [^double x ^double y]
  (let [one (fn ^double [^double cx ^double cy]
              (let [dx (- x cx) dx (- dx (Math/rint dx))
                    dy (- y cy) dy (- dy (Math/rint dy))
                    r (Math/hypot dx dy)
                    th (Math/atan2 dy dx)
                    g (fn ^double [^double d ^double w] (Math/exp (- (* (/ d w) (/ d w)))))
                    star (+ 0.1 (* 0.11 (Math/pow (Math/abs (Math/cos (* 4.0 th))) 4.0)))
                    beads (* (g (- r 0.255) 0.016)
                             (Math/pow (+ 0.5 (* 0.5 (Math/cos (* 16.0 th)))) 6.0))
                    rim (g (- r 0.31) 0.012)]
                (max (g (- r star) 0.016) beads rim (g r 0.035))))]
    (min 1.0 (max (one 0.5 0.5) (one 0.0 0.0)))))

(defn cat-state
  "Lattice state of start point (x, y) after `j` half-steps: even half-steps
   are the shear y += x, odd ones x += y, all mod `n` (two make one cat step)."
  [n j x y]
  (let [n (long n) j (long j)]
    (loop [i 0 x (long x) y (long y)]
      (cond
        (= i j) [x y]
        (even? i) (recur (inc i) x (long (mod (+ y x) n)))
        :else (recur (inc i) (long (mod (+ x y) n)) y)))))

(defn cat-clock
  "[j f] at loop phase `u`: half-step j in [0, 2 period) and its eased
   fraction f. The first tenth of the loop (centred on u = 0) holds the
   assembled picture; each half-step eases in and out so the iterates read."
  [period u]
  (let [v (lp/frac (+ (double u) 0.05))
        halves (* 2 (long period))]
    (if (< v 0.1)
      [0 0.0]
      (let [s (* (/ (- v 0.1) 0.9) halves)
            j (min (dec halves) (long (Math/floor s)))
            f (lp/smoothstep (/ (- (- s j) 0.12) 0.76))]
        [j f]))))

(defn cat-position
  "Real torus position [x y] in [0, n) of start point (x, y) at phase `u`."
  [n period x y u]
  (let [[j f] (cat-clock period u)
        [sx sy] (cat-state n j x y)
        n (double n) f (double f) sx (double sx) sy (double sy)
        m (fn ^double [^double v] (- v (* n (Math/floor (/ v n)))))]
    (if (even? (long j))
      [sx (m (+ sy (* f sx)))]
      [(m (+ sx (* f sy))) sy])))

(def ^:private cat-points
  (delay
   (let [n cat-n
         p (pal/scheme :blue-magenta-orange)]
     (vec (for [y (range n) x (range n)
                :let [tx (/ (+ x 0.5) n) ty (/ (+ y 0.5) n)
                      b (rosette tx ty)
                      dx (- tx 0.5) dy (- ty 0.5)
                      dc (min (Math/hypot dx dy) (Math/hypot (- (Math/abs dx) 0.5) (- (Math/abs dy) 0.5)))
                      [cr cg cb] (channels (pal/at p (+ 0.15 (* 2.2 dc))))
                      k (* b b)]
                :when (> k 0.004)]
            [x y (* k cr) (* k cg) (* k cb)])))))

(defn cat-rows
  "Splat rows of the cat-map loop at phase `u`: the torus is a square of
   side `side` scene units filling the height, tiled across the width."
  [u {:keys [side half-width sigma] :or {side 8.0 half-width 7.3 sigma 0.62}}]
  (let [n cat-n period (cat-period n)
        side (double side) half-width (double half-width)
        cell (/ side n) sig (* (double sigma) cell)]
    (into []
          (mapcat (fn [[x y r g b]]
                    (let [[px py] (cat-position n period x y u)
                          X (- (* (+ (double px) 0.5) cell) (* 0.5 side))
                          Y (- (* (+ (double py) 0.5) cell) (* 0.5 side))]
                      (for [k [-1.0 0.0 1.0]
                            :let [Xk (+ X (* k side))]
                            :when (< (Math/abs Xk) half-width)]
                        [Xk Y sig r g b]))))
          @cat-points)))

(def cat-map
  {:id :cat-map
   :title "Arnold's cat map: a 144 x 144 torus of light scrambles and returns exactly after 12 steps"
   :seconds 36 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0]
   :splat (fn [u] (cat-rows u {:sigma 0.5}))
   :exposure 1.1
   :post (fn [_] [{:type :bloom :radius 12 :strength 0.8}
                  {:type :vignette :strength 0.35}])})

;; ── Elliptic billiard ─────────────────────────────────────────────────

(defn bounce
  "From point `p` inside or on the ellipse x^2/a^2 + y^2/b^2 = 1, moving in
   direction `v`: [q v'] the next wall hit and the reflected direction."
  [a b [px py] [vx vy]]
  (let [a (double a) b (double b) px (double px) py (double py) vx (double vx) vy (double vy)
        a2 (* a a) b2 (* b b)
        A (+ (/ (* vx vx) a2) (/ (* vy vy) b2))
        B (* 2.0 (+ (/ (* px vx) a2) (/ (* py vy) b2)))
        C (- (+ (/ (* px px) a2) (/ (* py py) b2)) 1.0)
        t (/ (+ (- B) (Math/sqrt (max 0.0 (- (* B B) (* 4.0 A C))))) (* 2.0 A))
        qx (+ px (* t vx)) qy (+ py (* t vy))
        nx (/ qx a2) ny (/ qy b2) nn (Math/hypot nx ny)
        nx (/ nx nn) ny (/ ny nn)
        d (+ (* vx nx) (* vy ny))]
    [[qx qy] [(- vx (* 2.0 d nx)) (- vy (* 2.0 d ny))]]))

(defn tangent-dir
  "Unit direction from wall point `p` into the ellipse (a, b), tangent to
   the confocal ellipse with parameter `lam` (semi-axes sqrt(a^2 - lam),
   sqrt(b^2 - lam)), turning counter-clockwise about the centre."
  [a b lam [px py]]
  (let [a (double a) b (double b) lam (double lam) px (double px) py (double py)
        A2 (- (* a a) lam) B2 (- (* b b) lam)
        ;; lines through p with direction (1, t) tangent to the caustic
        qa (- (* px px) A2) qb (* -2.0 px py) qc (- (* py py) B2)
        dirs (if (< (Math/abs qa) 1e-12)
               [[0.0 1.0] (let [t (/ (- qc) qb) n (Math/sqrt (+ 1.0 (* t t)))] [(/ 1.0 n) (/ t n)])]
               (let [disc (Math/sqrt (max 0.0 (- (* qb qb) (* 4.0 qa qc))))]
                 (for [t [(/ (+ (- qb) disc) (* 2.0 qa)) (/ (- (- qb) disc) (* 2.0 qa))]]
                   (let [n (Math/sqrt (+ 1.0 (* t t)))] [(/ 1.0 n) (/ t n)]))))
        nx (/ px (* a a)) ny (/ py (* b b))]
    (first (for [[dx dy] dirs s [1.0 -1.0]
                 :let [dx (* s (double dx)) dy (* s (double dy))]
                 :when (and (< (+ (* dx nx) (* dy ny)) 0.0)
                            (> (- (* px dy) (* py dx)) 0.0))]
             [dx dy]))))

(defn orbit
  "The `q` + 1 wall points of the billiard ray in ellipse (a, b) that starts
   at eccentric angle `theta` tangent to caustic `lam`."
  [a b lam theta q]
  (let [p0 [(* (double a) (Math/cos theta)) (* (double b) (Math/sin theta))]]
    (loop [i 0 p p0 v (tangent-dir a b lam p0) acc [p0]]
      (if (= i (long q))
        acc
        (let [[p' v'] (bounce a b p v)] (recur (inc i) p' v' (conj acc p')))))))

(defn winding
  "Total eccentric angle swept by `q` bounces along caustic `lam`."
  ^double [a b lam q]
  (let [pts (orbit a b lam 0.5 q)
        ang (fn ^double [[x y]] (Math/atan2 (/ (double y) (double b)) (/ (double x) (double a))))]
    (reduce + (map (fn [p p'] (lp/frac (/ (- (ang p') (ang p)) lp/tau))) pts (rest pts)))))

(defn caustic-for
  "The confocal caustic parameter of the periodic orbits with rotation
   number p/q (p turns in q bounces), by bisection: the rotation number
   grows monotonically from 0 (lam -> 0) to 1/2 (lam -> b^2)."
  ^double [a b p q]
  (let [target (double p)]
    (loop [lo 1e-9 hi (* (double b) (double b) (- 1.0 1e-9)) i 0]
      (let [mid (* 0.5 (+ lo hi))]
        (if (> i 90)
          mid
          (if (< (winding a b mid q) target)
            (recur mid hi (inc i))
            (recur lo mid (inc i))))))))

(defn polyline-at
  "Point at arc-length fraction `s` (mod 1) of closed polyline `pts` with
   cumulative lengths `cum` (first 0, last the perimeter)."
  [pts ^doubles cum s]
  (let [n (dec (alength cum))
        L (aget cum n)
        d (* L (lp/frac s))
        i (loop [i 0] (if (and (< i (dec n)) (> d (aget cum (inc i)))) (recur (inc i)) i))
        t (/ (- d (aget cum i)) (max 1e-12 (- (aget cum (inc i)) (aget cum i))))
        [x0 y0] (nth pts i) [x1 y1] (nth pts (inc i))]
    [(+ x0 (* t (- x1 x0))) (+ y0 (* t (- y1 y0)))]))

(def billiard-table
  "Semi-axes of the table, in scene units."
  [6.3 3.6])

(def billiard-families
  "Periodic families [p q rays palette-t]: rotation number p/q, `rays`
   copies evenly spaced in starting angle (a Poncelet family: every member
   closes and all share one perimeter, so all return together)."
  [[1 9 9 0.05] [1 5 10 0.3] [2 9 9 0.55] [1 3 6 0.8]])

(def ^:private billiard-rays
  (delay
   (let [[a b] billiard-table]
     (vec (for [[p q rays t] billiard-families
                :let [lam (caustic-for a b p q)]
                k (range rays)
                :let [pts (orbit a b lam (* lp/tau (/ (+ k 0.25) (* rays q))) q)
                      cum (double-array (reductions + 0.0 (map (fn [[x0 y0] [x1 y1]] (Math/hypot (- x1 x0) (- y1 y0)))
                                                               pts (rest pts))))]]
            {:p p :q q :lam lam :t t :pts pts :cum cum :k k :rays rays})))))

(defn billiard-rows
  "Splat rows of the billiard at phase `u`: each ray a comet running its
   closed orbit once per loop, its tail `tail` of the perimeter long; the
   table's rim and the caustics as faint dotted light."
  [u {:keys [palette tail segs] :or {tail 0.09 segs 90}}]
  (let [[a b] billiard-table
        rim (for [i (range 720)
                  :let [th (* lp/tau (/ i 720.0))
                        w (+ 0.35 (* 0.65 (lp/wave (- (* 3.0 (/ i 720.0)) (double u)))))]]
              [(* a (Math/cos th)) (* b (Math/sin th)) 0.035 (* 0.10 w) (* 0.13 w) (* 0.22 w)])
        caustics (for [[p q _ t] billiard-families
                       :let [lam (caustic-for a b p q)
                             ca (Math/sqrt (- (* a a) lam)) cb (Math/sqrt (- (* b b) lam))
                             [r g bl] (channels (pal/at palette t))]
                       i (range 360)
                       :let [th (* lp/tau (/ i 360.0))]]
                   [(* ca (Math/cos th)) (* cb (Math/sin th)) 0.03 (* 0.05 r) (* 0.05 g) (* 0.05 bl)])
        comets (for [{:keys [pts cum t k rays]} @billiard-rays
                     :let [[r g bl] (channels (pal/at palette (+ t (* 0.06 (/ (double k) rays)))))]
                     j (range segs)
                     :let [f (/ (double j) segs)
                           [x y] (polyline-at pts cum (- (double u) (* tail f)))
                           e (Math/pow (- 1.0 f) 2.2)
                           head (if (zero? j) 1.0 0.0)]]
                 [x y (+ 0.028 (* 0.03 head)) (* e (+ (* 0.32 r) (* 0.6 head))) (* e (+ (* 0.32 g) (* 0.6 head))) (* e (+ (* 0.32 bl) (* 0.6 head)))])]
    (vec (concat rim caustics comets))))

(def elliptic-billiard
  (let [p (pal/scheme :blue-magenta-orange)]
    {:id :elliptic-billiard
     :title "Elliptic billiard: Poncelet families of periodic rays (1/9, 1/5, 2/9, 1/3) tracing their confocal caustics"
     :seconds 30 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0]
     :splat (fn [u] (billiard-rows u {:palette p}))
     :exposure 2.2
     :post (fn [_] [{:type :bloom :radius 14 :strength 1.2}
                    {:type :chroma :shift 2}
                    {:type :vignette :strength 0.45}])}))

(def loops
  "The chaos loops, by id."
  (array-map :cat-map cat-map
             :elliptic-billiard elliptic-billiard))
