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
            [tessera.palette :as pal]
            [desargues.wallpaper.render :as r]))

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
  "Grid side: on 322 x 322 the cat map has period 24 and its 12th power is
   -I, the point reflection, which leaves a centrally symmetric picture
   unchanged. So the picture assembles twice per loop, and the iterates next
   to each assembly are recognisable sheared ghosts of it."
  322)

(defn khatam
  "Class of the torus picture at (x, y) in [0, 1)^2: a 2 x 2 tile of neon
   khatam (eight-pointed star) cells, symmetric under (x, y) -> (-x, -y).
   0 dark, 1 outer star, 2 inner star, 3 octagon ring, 4 centre bead.
   Outlines are about two lattice cells wide on the 322 grid."
  ^long [^double x ^double y]
  (let [cx (- (* 2.0 x) (Math/floor (* 2.0 x)) 0.5)
        cy (- (* 2.0 y) (Math/floor (* 2.0 y)) 0.5)
        r (Math/hypot cx cy)
        th (Math/atan2 cy cx)
        sq (fn ^double [^double a]
             (let [c (Math/cos a) s (Math/sin a)]
               (max (Math/abs (+ (* c cx) (* s cy))) (Math/abs (- (* c cy) (* s cx))))))
        star (min (sq 0.0) (sq (/ Math/PI 4.0)))
        oct (* r (Math/cos (- (mod (+ th (/ Math/PI 8.0)) (/ Math/PI 4.0)) (/ Math/PI 8.0))))
        near? (fn [^double d ^double w] (< (Math/abs d) w))]
    (cond
      (< r 0.03) 4
      (near? (- star 0.30) 0.0075) 1
      (near? (- star 0.17) 0.0065) 2
      (near? (- oct 0.445) 0.0065) 3
      :else 0)))

(defn cat-state
  "Lattice state of start point (x, y) after `j` half-steps: even half-steps
   are the shear y += x, odd ones x += y, all mod `n` (two make one cat step
   C = [[2 1] [1 1]]). Whole steps go through the matrix power."
  [n j x y]
  (let [n (long n) j (long j) x (long x) y (long y)
        [a b c d] (loop [k 0 a 1 b 0 c 0 d 1]
                    (if (= k (quot j 2))
                      [a b c d]
                      (recur (inc k)
                             (long (mod (+ (* 2 a) c) n)) (long (mod (+ (* 2 b) d) n))
                             (long (mod (+ a c) n)) (long (mod (+ b d) n)))))
        X (long (mod (+ (* (long a) x) (* (long b) y)) n))
        Y (long (mod (+ (* (long c) x) (* (long d) y)) n))]
    (if (odd? j) [X (long (mod (+ Y X) n))] [X Y])))

(def cat-hold
  "Fraction of the loop each assembly is held (two assemblies per loop)."
  0.125)

(defn- step-weight
  "Time weight of cat step `j` of a half-period of `h` steps: slow next to
   the assemblies, where the ghosts read, quick through the mixed middle."
  ^double [^long j ^long h]
  (+ 0.45 (* 2.6 (Math/exp (/ (- (double (min j (- h 1 j)))) 1.4)))))

(defn cat-clock
  "[j f] at loop phase `u`: half-step j in [0, 2 period] and its eased
   fraction f. The picture is held for `cat-hold` around u = 0 and u = 1/2
   (after period/2 steps, where C^(period/2) = -I); between, the steps run
   with `step-weight`, each half-step eased in and out."
  [period u]
  (let [h (quot (long period) 2)
        v (lp/frac (+ (double u) (* 0.5 cat-hold)))
        half (if (< v 0.5) 0 1)
        w (- v (* 0.5 half))
        run (- 0.5 cat-hold)]
    (if (< w cat-hold)
      [(* 2 h half) 0.0]
      (let [ws (mapv #(step-weight % h) (range h))
            total (reduce + ws)
            target (* total (/ (- w cat-hold) run))
            [j t] (loop [j 0 acc 0.0]
                    (let [wj (double (ws j))]
                      (if (or (= j (dec h)) (< target (+ acc wj)))
                        [j (min 1.0 (/ (- target acc) wj))]
                        (recur (inc j) (+ acc wj)))))
            s (* 2.0 (double t))
            sub (min 1 (long (Math/floor s)))
            f (lp/smoothstep (/ (- (- s sub) 0.08) 0.84))]
        [(+ (* 2 (+ (* h half) (long j))) sub) f]))))

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

(def cat-colours
  "Neon colour of each khatam class."
  {1 [0.10 0.80 1.00] 2 [1.00 0.18 0.75] 3 [1.00 0.72 0.18] 4 [1.00 0.95 0.85]})

(def ^:private cat-points
  (delay
   (let [n cat-n]
     (vec (for [y (range n) x (range n)
                :let [k (khatam (/ (double x) n) (/ (double y) n))]
                :when (pos? k)
                :let [[r g b] (cat-colours k)]]
            [x y r g b])))))

(defn cat-rows
  "Splat rows of the cat-map loop at phase `u`: the torus is a square of
   side `side` scene units filling the height, tiled across the width; each
   lattice point a crisp bead. Beads are brightest while the picture is whole
   and dim while it is mixed, so the scrambled middle reads as sparse
   sparks and the ghosts next to each assembly stand out."
  [u {:keys [side half-width sigma] :or {side 8.0 half-width 7.3 sigma 0.36}}]
  (let [n cat-n period (long (cat-period n))
        h (quot period 2)
        side (double side) half-width (double half-width)
        cell (/ side n) sig (* (double sigma) cell)
        [j f] (cat-clock period u)
        s (mod (+ (* 0.5 (double j)) (* 0.5 (double f))) (double h))
        d (min s (- h s))
        gain (+ 0.1 (* 1.1 (Math/exp (/ (- d) 1.6))))]
    (into []
          (mapcat (fn [[x y r g b]]
                    (let [[px py] (cat-position n period x y u)
                          X (- (* (double px) cell) (* 0.5 side))
                          Y (- (* (double py) cell) (* 0.5 side))]
                      (for [k [-1.0 0.0 1.0]
                            :let [Xk (+ X (* k side))]
                            :when (< (Math/abs Xk) half-width)]
                        [Xk Y sig (* gain (double r)) (* gain (double g)) (* gain (double b))]))))
          @cat-points)))

(def cat-map
  {:id :cat-map
   :title "Arnold's cat map on a 322 x 322 torus of neon beads: period 24, the khatam returns at 12 and 24"
   :seconds 40 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0]
   :splat (fn [u] (cat-rows u {}))
   :exposure 1.0
   :post (fn [_] [{:type :bloom :radius 10 :strength 1.0}
                  {:type :chroma :shift 1}
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
  "Periodic families {:p :q :rays :t}: rotation number p/q, `rays` members
   spread evenly in starting angle and in phase (a Poncelet family: every
   member closes and all share one perimeter, so all return together).
   :hyperbolic picks the confocal hyperbola caustic (rays crossing the major
   axis between the foci), with :guess the bracket fraction of lam in
   (b^2, a^2) to search for its period-q orbit."
  [{:p 1 :q 9 :rays 14 :t 0.0}
   {:p 1 :q 5 :rays 16 :t 0.2}
   {:p 2 :q 9 :rays 14 :t 0.4}
   {:p 1 :q 3 :rays 12 :t 0.6}
   {:hyperbolic true :q 4 :rays 18 :t 0.8 :guess [0.2335 0.2365]}])

(defn return-error
  "Distance in position and direction between the start and the state after
   `q` bounces of the ray from eccentric angle `theta` tangent to `lam`;
   ##Inf when no such tangent ray exists there."
  [a b lam q theta]
  (let [a (double a) b (double b) theta (double theta)
        p0 [(* a (Math/cos theta)) (* b (Math/sin theta))]
        v0 (tangent-dir a b lam p0)]
    (if-not v0
      Double/POSITIVE_INFINITY
      (loop [i 0 p p0 v v0]
        (if (= i (long q))
          (+ (Math/hypot (- (double (p 0)) (double (p0 0))) (- (double (p 1)) (double (p0 1))))
             (Math/hypot (- (double (v 0)) (double (v0 0))) (- (double (v 1)) (double (v0 1)))))
          (let [[p' v'] (bounce a b p v)] (recur (inc i) p' v')))))))

(defn hyperbolic-caustic
  "lam in (b^2, a^2) of the period-`q` orbits tangent to a confocal
   hyperbola, by golden-section search of the return error inside the
   bracket [lo hi] (fractions of the way from b^2 to a^2)."
  ^double [a b q [lo hi]]
  (let [a2 (* (double a) (double a)) b2 (* (double b) (double b))
        th (+ (/ Math/PI 2.0) 0.13)
        f (fn ^double [^double lam] (return-error a b lam q th))
        gr (/ (- (Math/sqrt 5.0) 1.0) 2.0)]
    (loop [lo (+ b2 (* (- a2 b2) (double lo))) hi (+ b2 (* (- a2 b2) (double hi))) i 0]
      (if (= i 90)
        (* 0.5 (+ lo hi))
        (let [c (- hi (* gr (- hi lo))) d (+ lo (* gr (- hi lo)))]
          (if (< (f c) (f d)) (recur lo d (inc i)) (recur c hi (inc i))))))))

(defn family-caustic
  "lam of billiard family `fam` in the table (a, b)."
  ^double [a b {:keys [p q hyperbolic guess]}]
  (if hyperbolic (hyperbolic-caustic a b q guess) (caustic-for a b p q)))

(defn- start-angles
  "`m` starting eccentric angles from which the ray tangent to `lam` closes
   after `q` bounces, spread evenly over the admissible arcs (all of the rim
   for an elliptic caustic, |x| < sqrt(a^2 - lam) for a hyperbolic one;
   grazing starts at the arc ends, which do not close, are dropped)."
  [a b lam q m]
  (let [ok (filterv #(< (double (return-error a b lam q %)) 1e-7)
                    (map #(* lp/tau (/ (+ % 0.5) 2048.0)) (range 2048)))]
    (mapv #(ok (quot (* (long %) (count ok)) (long m))) (range m))))

(def ^:private billiard-rays
  (delay
   (let [[a b] billiard-table]
     (vec (for [{:keys [rays t] :as fam} billiard-families
                :let [lam (family-caustic a b fam)
                      q (:q fam)]
                [k th] (map-indexed vector (start-angles a b lam q rays))
                :let [pts (orbit a b lam th q)
                      cum (double-array (reductions + 0.0 (map (fn [[x0 y0] [x1 y1]] (Math/hypot (- x1 x0) (- y1 y0)))
                                                               pts (rest pts))))]]
            {:lam lam :t t :pts pts :cum cum :k k :rays rays
             ;; phase spread: member k starts k/rays of the way round its orbit
             :phase (/ (double k) rays)})))))

(defn- resample
  "Points every `ds` along the open polyline `pts`."
  [pts ds]
  (let [ds (double ds)]
    (loop [[p & more] pts acc [] carry 0.0]
      (if-not (seq more)
        acc
        (let [[x0 y0] p [x1 y1] (first more)
              len (Math/hypot (- (double x1) (double x0)) (- (double y1) (double y0)))
              ts (take-while #(< % len) (iterate #(+ % ds) carry))
              new (for [t ts :let [f (/ t (max len 1e-12))]]
                    [(+ x0 (* f (- x1 x0))) (+ y0 (* f (- y1 y0)))])]
          (recur more (into acc new) (- (+ carry (* ds (count ts))) len)))))))

(defn- conic-points
  "Points of the confocal conic `lam` of the table (a, b) clipped to the
   table, spaced `ds` apart: an ellipse for lam < b^2, both branches of a
   hyperbola for b^2 < lam < a^2."
  [a b lam ds]
  (let [a (double a) b (double b) lam (double lam)
        A (Math/sqrt (- (* a a) lam))
        inside? (fn [x y] (< (+ (/ (* x x) (* a a)) (/ (* y y) (* b b))) 1.0))]
    (if (< lam (* b b))
      (let [B (Math/sqrt (- (* b b) lam))]
        (resample (for [i (range 2049) :let [th (* lp/tau (/ i 2048.0))]]
                    [(* A (Math/cos th)) (* B (Math/sin th))])
                  ds))
      (let [B (Math/sqrt (- lam (* b b)))]
        (mapcat (fn [sx]
                  (resample (for [i (range -2000 2001)
                                  :let [s (* 0.002 i)
                                        x (* sx A (Math/cosh s)) y (* B (Math/sinh s))]
                                  :when (inside? x y)]
                              [x y])
                            ds))
                [-1.0 1.0])))))

(defn billiard-rows
  "Splat rows of the billiard at phase `u`: every ray a short bright comet
   running its closed orbit once per loop, members of a family spread
   evenly in phase, so their density traces the caustic; each family's
   caustic as a thin neon line, the rim with a travelling glow, the foci as
   two small stars."
  [u {:keys [palette tail segs] :or {tail 0.035 segs 36}}]
  (let [[a b] billiard-table
        a (double a) b (double b) u (double u)
        c (Math/sqrt (- (* a a) (* b b)))
        rim (for [i (range 900)
                  :let [th (* lp/tau (/ i 900.0))
                        w (+ 0.3 (* 0.7 (Math/pow (lp/wave (- (* 4.0 (/ i 900.0)) u)) 3.0)))]]
              [(* a (Math/cos th)) (* b (Math/sin th)) 0.022 (* 0.16 w) (* 0.2 w) (* 0.34 w)])
        caustics (for [{:keys [t] :as fam} billiard-families
                       :let [lam (family-caustic a b fam)
                             [r g bl] (channels (pal/at palette t))
                             breathe (+ 0.75 (* 0.25 (lp/wave (- u (double t)))))]
                       [x y] (conic-points a b lam 0.012)]
                   [x y 0.014 (* 0.42 breathe r) (* 0.42 breathe g) (* 0.42 breathe bl)])
        foci (for [sx [-1.0 1.0] [s k] [[0.09 0.25] [0.03 1.0]]]
               [(* sx c) 0.0 s (* k 1.0) (* k 0.92) (* k 0.8)])
        comets (for [{:keys [pts cum t phase k rays]} @billiard-rays
                     :let [[r g bl] (channels (pal/at palette (+ (double t) (* 0.08 (/ (double k) rays)))))]
                     j (range segs)
                     :let [f (/ (double j) segs)
                           [x y] (polyline-at pts cum (- (+ u (double phase)) (* tail f)))
                           e (Math/pow (- 1.0 f) 1.8)
                           head (if (zero? j) 1.0 0.0)]]
                 [x y (+ 0.016 (* 0.014 head))
                  (* e (+ (* 0.5 r) (* 0.5 head))) (* e (+ (* 0.5 g) (* 0.5 head))) (* e (+ (* 0.5 bl) (* 0.5 head)))])]
    (vec (concat rim caustics foci comets))))

(def elliptic-billiard
  (let [p (pal/cosine [0.55 0.5 0.55] [0.45 0.45 0.45] [1.0 1.0 1.0] [0.0 0.33 0.67])]
    {:id :elliptic-billiard
     :title "Elliptic billiard: 74 periodic rays in five Poncelet families tracing their confocal caustics, four ellipses and a hyperbola"
     :seconds 30 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0]
     :splat (fn [u] (billiard-rows u {:palette p}))
     :exposure 1.8
     :post (fn [_] [{:type :bloom :radius 12 :strength 1.1}
                    {:type :chroma :shift 2}
                    {:type :vignette :strength 0.4}])}))

;; ── Chirikov standard map: KAM tori with light flowing along them ───────────────

(def standard-k
  "Kick strength: a little below the break-up of the golden torus
   (K = 0.9716), so rotational tori, island chains and a chaotic sea coexist."
  0.92)

(defn- wrap-pi ^double [^double a]
  (- a (* lp/tau (Math/floor (/ (+ a Math/PI) lp/tau)))))

(defn standard-orbit
  "`n` iterates of the standard map p' = p + K sin x, x' = x + p' from
   (x0, p0), both mod 2 pi: [xs ps] as double arrays, p in [-pi, pi)."
  [k x0 p0 n]
  (let [k (double k) n (long n)
        xs (double-array n) ps (double-array n)]
    (loop [i 0 x (double x0) p (wrap-pi p0)]
      (when (< i n)
        (aset xs i x) (aset ps i p)
        (let [p' (wrap-pi (+ p (* k (Math/sin x))))
              x' (let [v (+ x p')] (- v (* lp/tau (Math/floor (/ v lp/tau)))))]
          (recur (inc i) x' p'))))
    [xs ps]))

(defn- birkhoff
  "Weighted Birkhoff average of the first `n` of `incs` (radians per step),
   in turns: weights exp(-1/(t(1-t))), super-convergent on invariant curves."
  ^double [^doubles incs n]
  (let [n (long n)]
    (loop [i 1 s 0.0 w 0.0]
      (if (>= i n)
        (/ s (* lp/tau w))
        (let [t (/ (double i) n)
              g (Math/exp (/ -1.0 (* t (- 1.0 t))))]
          (recur (inc i) (+ s (* g (aget incs i))) (+ w g)))))))

(defn rotation
  "{:regular? :omega :kind} of the orbit [xs ps]. A rotational torus turns
   in x; an island curve about (pi, 0) has x-rotation 0 and turns about the
   island centre instead. regular? when the full and half averages agree."
  [[^doubles xs ^doubles ps]]
  (let [n (dec (alength xs))
        dx (double-array n) da (double-array n)
        b0 (aget ps 1)]
    (dotimes [i n]
      (let [d (- (aget xs (inc i)) (aget xs i))
            d (+ b0 (wrap-pi (- d b0)))
            a0 (Math/atan2 (aget ps i) (- (aget xs i) Math/PI))
            a1 (Math/atan2 (aget ps (inc i)) (- (aget xs (inc i)) Math/PI))]
        (aset dx i d) (aset da i (wrap-pi (- a1 a0)))))
    (let [wx (birkhoff dx n) wx2 (birkhoff dx (quot n 2))
          wa (birkhoff da n) wa2 (birkhoff da (quot n 2))
          regular? (< (Math/abs (- wx wx2)) 1e-7)
          island? (and regular? (< (Math/abs wx) 1e-7) (< (Math/abs (- wa wa2)) 1e-7))]
      {:regular? regular?
       :kind (cond island? :island regular? :torus :else :chaos)
       :omega (if island? wa wx)})))

(def portrait-size
  "Cells of the phase-portrait buffer: one per pixel at 1080p."
  [1920 1080])

(defn- deposit!
  "Bilinear deposit of weight `w` with colour [r g b] and phase vector
   [c s] at buffer coordinates (bx, by), wrapping on the torus."
  [{:keys [^floats W ^floats R ^floats G ^floats B ^floats C ^floats S]} bw bh bx by w r g b c s]
  (let [bw (long bw) bh (long bh) bx (double bx) by (double by) w (double w)
        ix (long (Math/floor bx)) iy (long (Math/floor by))
        fx (- bx ix) fy (- by iy)]
    (doseq [[ox oy k] [[0 0 (* (- 1.0 fx) (- 1.0 fy))] [1 0 (* fx (- 1.0 fy))]
                       [0 1 (* (- 1.0 fx) fy)] [1 1 (* fx fy)]]]
      (let [i (+ (* (long (mod (+ iy (long oy)) bh)) bw) (long (mod (+ ix (long ox)) bw)))
            kw (* w (double k))]
        (aset W i (float (+ (aget W i) kw)))
        (aset R i (float (+ (aget R i) (* kw (double r)))))
        (aset G i (float (+ (aget G i) (* kw (double g)))))
        (aset B i (float (+ (aget B i) (* kw (double b)))))
        (aset C i (float (+ (aget C i) (* kw (double c)))))
        (aset S i (float (+ (aget S i) (* kw (double s)))))))))

(def portrait-crests
  "Light crests per invariant curve."
  3)

(defn portrait
  "The standard map's phase portrait as buffers {:W :R :G :B :C :S}: every
   seed orbit deposited point by point. A regular orbit's n-th point carries
   the phase vector of crests x (theta0 + n omega), its position along the
   curve in the curve's own conjugating angle; a chaotic orbit carries none."
  [{:keys [k steps seeds palette] :or {k standard-k steps 16384}}]
  (let [[bw bh] portrait-size
        n (* (long bw) (long bh))
        bufs (into {} (for [key [:W :R :G :B :C :S]] [key (float-array n)]))
        rng (java.util.Random. 11)
        chaos-left (atom 14)]
    (doseq [[x0 p0] seeds]
      (let [[^doubles xs ^doubles ps :as orb] (standard-orbit k x0 p0 steps)
            {:keys [kind omega]} (rotation orb)
            omega (double omega)
            th0 (.nextDouble rng)
            [r g b] (case kind
                      :chaos [0.16 0.10 0.42]
                      :island (channels (pal/at palette (+ 0.62 (* 0.3 (Math/hypot (- (double x0) Math/PI) (double p0))))))
                      :torus (channels (pal/at palette (+ 0.5 (* 0.5 (Math/sin (double p0)))))))
            w (if (= kind :chaos) 0.25 1.0)]
        (when (or (not= kind :chaos) (pos? (long (swap! chaos-left dec))))
          (dotimes [i (alength xs)]
            (let [ph (* lp/tau portrait-crests (lp/frac (+ th0 (* i omega))))
                  [c s] (if (= kind :chaos) [0.0 0.0] [(Math/cos ph) (Math/sin ph)])]
              (deposit! bufs bw bh
                        (* (/ (aget xs i) lp/tau) bw)
                        (* (- 0.5 (/ (aget ps i) lp/tau)) bh)
                        w r g b c s))))))
    bufs))

(def standard-seeds
  "Seeds along the two symmetry lines x = 0 and x = pi, and a few off them."
  (vec (concat (for [i (range 110)] [Math/PI (* lp/tau (- (/ (+ i 0.5) 110.0) 0.5))])
               (for [i (range 70)] [0.0 (* lp/tau (- (/ (+ i 0.5) 70.0) 0.5))])
               (for [i (range 40)] [(* 0.5 Math/PI) (* lp/tau (- (/ (+ i 0.5) 40.0) 0.5))]))))

(def ^:private standard-portrait
  (delay (portrait {:seeds standard-seeds :palette (pal/scheme :yellow-magenta-cyan)})))

(defn standard-shader
  "The portrait at phase `u`: each cell's curve brightens where one of its
   crests passes; the crests advance one crest spacing per loop along the
   curve's own angle, i.e. with the flow. The chaotic sea glows dim and
   steady."
  [u]
  (let [{:keys [^floats W ^floats R ^floats G ^floats B ^floats C ^floats S]} @standard-portrait
        [bw bh] portrait-size
        bw (long bw) bh (long bh)
        [vw vh] r/default-view
        vw (double vw) vh (double vh)
        shift (* 2.0 lp/tau (double u))
        cu (Math/cos shift) su (Math/sin shift)]
    (fn ^long [^double x ^double y]
      (let [bx (- (* (+ (/ x vw) 0.5) bw) 0.5)
            by (- (* (- 0.5 (/ y vh)) bh) 0.5)
            ix (long (Math/floor bx)) iy (long (Math/floor by))
            fx (- bx ix) fy (- by iy)
            i00 (+ (* (Math/floorMod iy bh) bw) (Math/floorMod ix bw))
            i10 (+ (* (Math/floorMod iy bh) bw) (Math/floorMod (inc ix) bw))
            i01 (+ (* (Math/floorMod (inc iy) bh) bw) (Math/floorMod ix bw))
            i11 (+ (* (Math/floorMod (inc iy) bh) bw) (Math/floorMod (inc ix) bw))
            k00 (* (- 1.0 fx) (- 1.0 fy)) k10 (* fx (- 1.0 fy))
            k01 (* (- 1.0 fx) fy) k11 (* fx fy)
            smp (fn ^double [^floats a]
                  (+ (* k00 (aget a i00)) (* k10 (aget a i10)) (* k01 (aget a i01)) (* k11 (aget a i11))))
            w (smp W)]
        (if (< w 1e-6)
          0
          (let [c (/ (smp C) w) s (/ (smp S) w)
                coh (min 1.0 (Math/hypot c s))
                ;; cos(phase - 2 pi u) without the atan: the crest moves forward
                cphase (if (> coh 1e-6) (/ (+ (* c cu) (* s su)) coh) 0.0)
                pulse (Math/pow (* 0.5 (+ 1.0 cphase)) 8.0)
                base (- 1.0 (Math/exp (* -0.22 w)))
                lit (* base (+ 0.3 (* coh 1.9 pulse) (* 0.4 (- 1.0 coh))))
                hot (* base coh (Math/pow pulse 3.0) 0.7)
                rr (/ (smp R) w) gg (/ (smp G) w) bb (/ (smp B) w)]
            (r/rgb (+ (* lit rr) hot) (+ (* lit gg) hot) (+ (* lit bb) hot))))))))

(def standard-map
  {:id :standard-map
   :title "Chirikov standard map, K = 0.92: light flowing along KAM tori and island curves over the chaotic sea"
   :seconds 30 :fps 30 :size [1920 1080] :view r/default-view
   :field standard-shader
   :post (fn [_] [{:type :bloom :radius 10 :strength 0.9}
                  {:type :vignette :strength 0.35}])})

(def loops
  "The chaos loops, by id."
  (array-map :cat-map cat-map
             :elliptic-billiard elliptic-billiard
             :standard-map standard-map))
