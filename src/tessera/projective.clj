(ns tessera.projective
  "Projective geometry as luminous, seamless field loops. Homogeneous points
   and lines meet by cross products; conics and their polars are level sets.
   Fixed geometry is unrolled when each field form is built, never iterated
   per pixel. Every temporal term is an integer harmonic of tau*u."
  (:require [desargues.wallpaper.field :as field]
            [tessera.neon :as neon]))

(def view [8.0 4.5])
(def tau (* 2.0 Math/PI))

(defn bind
  "The field of a projective layer at phase `u`."
  [layer u]
  (field/bind (:form layer) u))

(defn- hyp [a b]
  (list 'sqrt (list '+ (list '* a a) (list '* b b))))

(defn- seg
  "Field distance to segment [ax ay]--[bx by], a straight-line SDF."
  [a b]
  (let [dx (list '- (first b) (first a)) dy (list '- (second b) (second a))
        vx (list '- 'x (first a)) vy (list '- 'y (second a))
        t (list 'clamp (list '/ (list '+ (list '* vx dx) (list '* vy dy))
                                (list '+ (list '* dx dx) (list '* dy dy) 1.0e-12)) 0.0 1.0)]
    (hyp (list '- vx (list '* t dx)) (list '- vy (list '* t dy)))))

(defn- point-distance [p]
  (hyp (list '- 'x (first p)) (list '- 'y (second p))))

(defn- circle-distance [p r]
  (list 'abs (list '- (point-distance p) r)))

(defn- line-distance [line]
  (let [[a b c] line]
    (list '/ (list 'abs (list '+ (list '* a 'x) (list '* b 'y) c))
          (list 'max 1.0e-9 (hyp a b)))))

(defn- cross
  "The homogeneous meet of two lines, or join of two points. Numeric vectors
   produce numbers; symbolic vectors produce field expressions."
  [[a b c] [d e f]]
  (if (every? number? [a b c d e f])
    [(- (* b f) (* c e)) (- (* c d) (* a f)) (- (* a e) (* b d))]
    [(list '- (list '* b f) (list '* c e))
     (list '- (list '* c d) (list '* a f))
     (list '- (list '* a e) (list '* b d))]))

(defn- affine [[x y w]]
  (if (every? number? [x y w])
    [(/ x w) (/ y w)]
    [(list '/ x (list 'max 0.001 (list 'abs w)))
     (list '/ y (list 'max 0.001 (list 'abs w)))]))

(defn join [p q] (cross p q))
(defn meet [l m] (affine (cross l m)))
(defn- vjoin
  "Numeric homogeneous join for fixed construction geometry."
  [[a b c] [d e f]]
  [(- (* b f) (* c e)) (- (* c d) (* a f)) (- (* a e) (* b d))])

(defn- vmeet [l m]
  (let [[x y w] (vjoin l m)] [(/ x w) (/ y w) 1.0]))
(defn- triangle-bindings
  "Three finite side segments of homogeneous triangle points a b c."
  [prefix [a b c]]
  (vec (mapcat (fn [i p q]
                 (let [s #(symbol (str prefix %))]
                   [(s i) (seg (affine p) (affine q))]))
               ["a" "b" "c"] [a b c] [b c a])))

(defn- layer [id title period bindings channels]
  {:id id :title title :period period :view view :blend :lighten
   :params []
   :form {:params '[u]
          :body (list 'let bindings (neon/neon-form (channels 0) (channels 1) (channels 2)))}})

(defn- section-bindings
  "The face-on conic cut of z^2=k^2(x^2+y^2) by z=m*x+c.
   X is distance along the tilted plane, Y its perpendicular direction."
  [prefix k c m]
  (let [s #(symbol (str prefix %))
        den (list '+ 1.0 (list '* m m))
        k2 (list '* k k)
        B (list '/ (list '- (list '* m m) k2) den)]
    [(s "x") '(/ (- x 3.0) 0.55) (s "y") '(/ y 0.55)
     (s "m") m (s "b") B (s "c") (list '/ (list '* (* 2.0 c) m) den)
     (s "f") (list '+ (list '* B (s "x") (s "x")) (list '* (s "c") (s "x"))
                   (* c c) (list '- (list '* k2 (s "y") (s "y"))))
     (s "gx") (list '+ (list '* 2.0 B (s "x")) (s "c"))
     (s "gy") (list '* -2.0 k2 (s "y"))
     (s "d") (list '* 0.55 (list '/ (list 'abs (s "f"))
                                (hyp (s "gx") (s "gy"))))]))

(defn- dandelin-bindings
  "Two Dandelin spheres in the axial projection. lambda = cos(beta)/cos(alpha)
   controls the centres z=c/(1 +/- lambda); plane contact is a focus."
  [k c m]
  (let [ncone (hyp k 1.0) nplane (hyp m 1.0)
        lambda (list '/ (list '* k nplane) ncone)
        sphere (fn [z0]
                 (let [g (list '- c z0) den (list '+ 1.0 (list '* m m))
                       r (list 'abs (list '/ (list '* k z0) ncone))
                       xt (list '/ (list '* -1.0 m g) den)
                       zt (list '+ z0 (list '/ g den))]
                   [(point-distance [0.0 z0]) r
                    (point-distance [xt zt])
                    (point-distance [(list '+ 3.0 (list '* 0.55 xt nplane)) 0.0])]))
        s0 (sphere (list '/ c (list '+ 1.0 lambda)))
        s1 (sphere (list '/ c (list '- 1.0 lambda)))]
    ['h '(sqrt 3.0) 'k k 'cut_m m 'cut_c c
     'plane (line-distance [m -1.0 c])
     'generator0 (seg [-4.3 -4.3] [0.0 0.0])
     'generator1 (seg [-4.3 4.3] [0.0 0.0])
     'generator2 (seg [4.3 -4.3] [0.0 0.0])
     'generator3 (seg [4.3 4.3] [0.0 0.0])
     'd0 (list 'min 'generator0 (list 'min 'generator1 (list 'min 'generator2 'generator3)))
     'sphere0 (nth s0 0) 'r0 (nth s0 1) 't0 (nth s0 2) 'f0 (nth s0 3)
     'sphere1 (nth s1 0) 'r1 (nth s1 1) 't1 (nth s1 2) 'f1 (nth s1 3)]))

(def dandelin-conics
  "A cone cut by a plane through circle, ellipse, parabola and hyperbola.
   The axial Dandelin construction is at left, the face-on section at right."
  (let [m '(tan (* 1.35 (sin (* tau u))))
        k 'h
        bs (dandelin-bindings k 2.4 m)]
    (layer :dandelin_conics
           "Dandelin spheres: a tilting plane cycles through the conic sections"
           18
           (into bs (section-bindings "section_" k 2.4 m))
           (let [cone (neon/glow-form 'd0 0.032)
                 cut (neon/glow-form 'plane 0.025)
                 spheres (list '+ (neon/glow-form (list 'abs (list '- 'sphere0 'r0)) 0.024)
                               (neon/glow-form (list 'abs (list '- 'sphere1 'r1)) 0.024))
                 foci (list '+ (neon/glow-form 'f0 0.045) (neon/glow-form 'f1 0.045))
                 contacts (list '+ (neon/glow-form 't0 0.025) (neon/glow-form 't1 0.025))
                 section (neon/glow-form 'section_d 0.035)]
             [(list '+ (list '* 0.13 cone) (list '* 0.65 cut) (list '* 0.10 spheres)
                    (list '* 0.9 foci) (list '* 0.1 section))
              (list '+ (list '* 0.70 cone) (list '* 0.45 cut) (list '* 0.3 spheres)
                    (list '* 0.8 foci) (list '* 0.95 contacts) (list '* 0.65 section))
              (list '+ (list '* 0.85 cone) (list '* 0.9 cut) (list '* 0.85 spheres)
                    (list '* 0.15 foci) (list '* 0.3 contacts) (list '* 0.12 section))]))))

(defn desargues-geometry
  "Two triangles perspective from O, in scene coordinates at phase u."
  [u]
  (let [a (* tau u) c (Math/cos a) s (Math/sin a)
        ta [[(+ 2.9 (* 0.638 s)) (+ 0.5 (* 0.522 c))]
            [(- -0.5 (* 0.528 s)) (+ 2.4 (* 0.18 c))]
            [(- -1.6 (* 0.396 s)) (+ -1.8 (* 0.288 c))]]
        tb (mapv (fn [[x y] k] [(* k x) (* k y)]) ta
                 [(+ 1.12 (* 0.12 c)) (+ 0.92 (* 0.13 s)) (+ 1.04 (* 0.10 c))])
        hp (fn [p] (conj p 1.0))
        sides (fn [t] (mapv vjoin t (cycle (rest t))))
        axis (vjoin (vjoin ((sides (mapv hp ta)) 0) ((sides (mapv hp tb)) 0))
                    (vjoin ((sides (mapv hp ta)) 1) ((sides (mapv hp tb)) 1)))]
    {:centre [0.0 0.0] :triangles [ta tb] :axis axis}))

(defn- desargues-bindings
  "The three side-pair intersections lie on one axis by construction."
  [prefix [ta tb] axis]
  (let [s #(symbol (str prefix %))
        sides (fn [tri] (mapv join tri (cycle (rest tri))))
        ps (mapv meet (sides ta) (sides tb))]
    (vec (concat (triangle-bindings "ta_" ta)
                 (triangle-bindings "tb_" tb)
                 (mapcat (fn [i p q]
                           [(s (str "ray" i)) (seg (affine p) (affine q))])
                         (range 3) ta tb)
                 ['axis (line-distance axis)
                  'p0 (point-distance (ps 0)) 'p1 (point-distance (ps 1))
                  'p2 (point-distance (ps 2)) 'centre (point-distance [0.0 0.0])]))))

(def desargues
  "Two triangles breathe about their centre of perspectivity; all three
   meets of corresponding sides stay on the luminous axis of perspectivity."
  (let [A ['(+ 2.9 (* 0.638 (sin (* tau u)))) '(+ 0.5 (* 0.522 (cos (* tau u)))) 1.0]
        B ['(- -0.5 (* 0.528 (sin (* tau u)))) '(+ 2.4 (* 0.18 (cos (* tau u)))) 1.0]
        C ['(- -1.6 (* 0.396 (sin (* tau u)))) '(+ -1.8 (* 0.288 (cos (* tau u)))) 1.0]
        ta [A B C]
        scales ['(+ 1.12 (* 0.12 (cos (* tau u))))
                '(+ 0.92 (* 0.13 (sin (* tau u))))
                '(+ 1.04 (* 0.10 (cos (* tau u))))]
        tb (mapv (fn [[x y w] k] [(list '* k x) (list '* k y) (list '* k w)]) ta scales)
        coords (vec (mapcat (fn [prefix tri]
                              (mapcat (fn [i [x y _]]
                                        [(symbol (str prefix "x" i)) x (symbol (str prefix "y" i)) y])
                                      (range 3) tri))
                            ["a" "b"] [ta tb]))
        point (fn [prefix i] [(symbol (str prefix "x" i)) (symbol (str prefix "y" i)) 1.0])
        ta (mapv #(point "a" %) (range 3)) tb (mapv #(point "b" %) (range 3))
        axis (join (cross (join (ta 0) (ta 1)) (join (tb 0) (tb 1)))
                   (cross (join (ta 1) (ta 2)) (join (tb 1) (tb 2))))]
    (layer :desargues "Desargues: perspective triangles and their luminous axis" 16
           (into coords (desargues-bindings "" [ta tb] axis))
           (let [a (neon/glow-form (list 'min 'ta_a (list 'min 'ta_b 'ta_c)) 0.032)
                 b (neon/glow-form (list 'min 'tb_a (list 'min 'tb_b 'tb_c)) 0.030)
                 r (neon/glow-form (list 'min 'ray0 (list 'min 'ray1 'ray2)) 0.016)
                 l (neon/glow-form 'axis 0.032)
                 pts (list '+ (neon/glow-form 'p0 0.045) (neon/glow-form 'p1 0.045) (neon/glow-form 'p2 0.045))
                 o (neon/glow-form 'centre 0.05)]
             [(list '+ (list '* 0.10 a) (list '* 0.85 b) (list '* 0.2 r) (list '* 0.9 l) (list '* 0.85 pts) o)
              (list '+ (list '* 0.75 a) (list '* 0.35 b) (list '* 0.2 r) (list '* 0.8 l) (list '* 0.75 pts) (list '* 0.85 o))
              (list '+ (list '* 0.9 a) (list '* 0.12 b) (list '* 0.7 r) (list '* 0.12 l) (list '* 0.12 pts) (list '* 0.08 o))]))))

(def conic-a 3.1)
(def conic-b 2.15)

(defn- conic-distance
  "Gradient-normalised implicit ellipse x^2/a^2 + y^2/b^2 = 1."
  [x y]
  (list '/ (list 'abs (list '- (list '/ (list '* x x) (* conic-a conic-a))
                             (list '/ (list '* y y) (* conic-b conic-b)) 1.0))
        (list 'max 0.1 (hyp (list '/ x (* conic-a conic-a))
                            (list '/ y (* conic-b conic-b))))))

(defn- ellipse-point [a]
  (if (number? a)
    [(* conic-a (Math/cos a)) (* conic-b (Math/sin a)) 1.0]
    [(list '* conic-a (list 'cos a)) (list '* conic-b (list 'sin a)) 1.0]))

(defn- tangent [p]
  (let [[x y _] p]
    [(list '/ x (* conic-a conic-a)) (list '/ y (* conic-b conic-b)) -1.0]))

(defn pascal-geometry
  "Six ellipse points and the three meets of opposite hexagon sides."
  [u]
  (let [ps (mapv #(ellipse-point (* tau (+ % u))) [0.03 0.17 0.32 0.51 0.68 0.83])
        edge (fn [i] (join (ps i) (ps (mod (inc i) 6))))]
    {:points ps :pascal (mapv meet (map edge [0 1 2]) (map edge [3 4 5]))}))

(defn- hexagon-bindings
  "Named six conic points and their tangent lines; joins of the tangent
   line objects are precisely the Brianchon diagonals."
  [ps]
  (let [sym (fn [p i] (symbol (str p i)))
        hps (mapv (fn [i [x y _]] [(sym "hx" i) (sym "hy" i) 1.0]) (range 6) ps)
        ts (mapv tangent hps)
        es (mapv join hps (cycle (rest hps)))
        ds (mapv join ts (cycle (drop 3 ts)))]
    (vec (concat
          (mapcat (fn [i [x y _]] [(sym "hx" i) x (sym "hy" i) y]) (range 6) ps)
          (mapcat (fn [i l] [(sym "edge" i) (line-distance l)]) (range 6) es)
          (mapcat (fn [i l] [(sym "tangent" i) (line-distance l)]) (range 6) ts)
          (mapcat (fn [i p] [(sym "pp" i) (point-distance p)]) (range 3)
                  (map meet (take 3 es) (drop 3 es)))
          (mapcat (fn [i p] [(sym "bp" i) (point-distance p)]) (range 6)
                  (map meet ts (cycle (rest ts))))
          (mapcat (fn [i l] [(sym "bd" i) (line-distance l)]) (range 3) ds)
          ['pascal_axis (line-distance (join (cross (es 0) (es 3))
                                             (cross (es 1) (es 4))))
           'brianchon_point (point-distance (meet (ds 0) (ds 1)))]))))

(def pascal-brianchon
  "Pascal's six points and collinear opposite-side meets crossfade to the
   tangent-line dual: Brianchon's three diagonals through one point."
  (let [ps (mapv #(ellipse-point (list '* 'tau (list '+ % 'u))) [0.03 0.17 0.32 0.51 0.68 0.83])]
    (layer :pascal_brianchon "Pascal and Brianchon: a projective hexagon and its tangent dual" 20
           (into ['conic (conic-distance 'x 'y) 'dual '(wave (- (* 2.0 u) 0.5))]
                 (hexagon-bindings ps))
           (let [edges (reduce (fn [d i] (list 'min d (symbol (str "edge" i)))) 1.0e6 (range 6))
                 tangents (reduce (fn [d i] (list 'min d (symbol (str "tangent" i)))) 1.0e6 (range 6))
                 pp (reduce (fn [d i] (list 'min d (symbol (str "pp" i)))) 1.0e6 (range 3))
                 bp (reduce (fn [d i] (list 'min d (symbol (str "bp" i)))) 1.0e6 (range 6))
                 bd (reduce (fn [d i] (list 'min d (symbol (str "bd" i)))) 1.0e6 (range 3))
                 hexagon (neon/glow-form edges 0.025) ppts (neon/glow-form pp 0.05)
                 axis (neon/glow-form 'pascal_axis 0.030)
                 tanlight (neon/glow-form tangents 0.022) bpts (neon/glow-form bp 0.036)
                 diagonals (neon/glow-form bd 0.035) centre (neon/glow-form 'brianchon_point 0.055)
                 conic (neon/glow-form 'conic 0.035)]
             [(list '+ (list '* 0.35 conic)
                    (list '* (list '- 1.0 'dual) (list '+ (list '* 0.08 hexagon) (list '* 0.9 ppts) (list '* 0.8 axis)))
                    (list '* 'dual (list '+ (list '* 0.15 tanlight) (list '* 0.8 diagonals) (list '* 0.9 centre))))
              (list '+ (list '* 0.75 conic)
                    (list '* (list '- 1.0 'dual) (list '+ (list '* 0.7 hexagon) (list '* 0.75 ppts) (list '* 0.85 axis)))
                    (list '* 'dual (list '+ (list '* 0.6 tanlight) (list '* 0.85 bpts) (list '* 0.8 diagonals) (list '* 0.85 centre))))
              (list '+ (list '* 0.9 conic)
                    (list '* (list '- 1.0 'dual) (list '+ (list '* 0.95 hexagon) (list '* 0.12 ppts) (list '* 0.15 axis)))
                    (list '* 'dual (list '+ (list '* 0.9 tanlight) (list '* 0.9 bpts) (list '* 0.12 diagonals) (list '* 0.15 centre))))]))))

(defn pole-geometry
  "Orbit point and its polar for the common ellipse at phase u."
  [u]
  (let [p [(* (+ conic-a 1.35) (Math/cos (* tau u)))
           (* (+ conic-b 0.75) (Math/sin (* tau u))) 1.0]]
    {:pole p :polar (tangent p)}))

(def pole-polar
  "An exterior point orbits its conic; its polar and the two tangent contacts
   travel with it, the pole/polar incidence always exact."
  (let [px (list '* (+ conic-a 1.35) (list 'cos (list '* 'tau 'u)))
        py (list '* (+ conic-b 0.75) (list 'sin (list '* 'tau 'u)))
        l [(list '/ px (* conic-a conic-a)) (list '/ py (* conic-b conic-b)) -1.0]
        den (list '+ (list '/ (list '* px px) (* conic-a conic-a))
                  (list '/ (list '* py py) (* conic-b conic-b)))
        x0 (list '/ (* conic-a conic-a) den) y0 (list '/ (* conic-b conic-b) den)
        sx (list '* (* conic-a conic-a) py) sy (list '* (* conic-b conic-b) px)
        scale (list '/ (list 'sqrt (list '- 1.0 (list '/ 1.0 den))) (hyp sx sy))
        p1 [(list '- x0 (list '* sx scale)) (list '+ y0 (list '* sy scale))]
        p2 [(list '+ x0 (list '* sx scale)) (list '- y0 (list '* sy scale))]]
    (layer :pole_polar "Pole and polar: tangent contacts of an orbiting point" 14
           ['conic (conic-distance 'x 'y) 'pole (point-distance [px py])
            'polar (line-distance l) 'contact0 (point-distance p1)
            'contact1 (point-distance p2) 'radius0 (seg [px py] p1)
            'radius1 (seg [px py] p2)]
           (let [c (neon/glow-form 'conic 0.035) l (neon/glow-form 'polar 0.030)
                 p (neon/glow-form 'pole 0.05)
                 t (list '+ (neon/glow-form 'contact0 0.045) (neon/glow-form 'contact1 0.045))
                 r (list '+ (neon/glow-form 'radius0 0.015) (neon/glow-form 'radius1 0.015))]
             [(list '+ (list '* 0.25 c) (list '* 0.85 l) (list '* 0.95 p) (list '* 0.75 t) (list '* 0.10 r))
              (list '+ (list '* 0.75 c) (list '* 0.8 l) (list '* 0.75 p) (list '* 0.9 t) (list '* 0.25 r))
              (list '+ (list '* 0.90 c) (list '* 0.15 l) (list '* 0.12 p) (list '* 0.15 t) (list '* 0.65 r))]))))

(def pencil-bases
  "Four projectively general base points of the conic pencil."
  [[-3.4 -1.45 1.0] [-2.8 1.65 1.0] [2.9 1.5 1.0] [3.5 -1.35 1.0]])

(defn- line-pair-conic
  "Six homogeneous quadratic coefficients of the line pair (l)(m)=0."
  [[a b c] [d e f]]
  [(* a d) (+ (* a e) (* b d)) (* b e)
   (+ (* a f) (* c d)) (+ (* b f) (* c e)) (* c f)])

(def pencil-first
  (line-pair-conic (vjoin (pencil-bases 0) (pencil-bases 1))
                   (vjoin (pencil-bases 2) (pencil-bases 3))))
(def pencil-second
  (line-pair-conic (vjoin (pencil-bases 0) (pencil-bases 2))
                   (vjoin (pencil-bases 1) (pencil-bases 3))))

(defn pencil-coefficients
  "Six quadratic coefficients at phase u; the cosine/sine homogeneous pair
   traverses RP^1 once, with no branch cut at a point at infinity."
  [u]
  (mapv (fn [a b] (if (number? u)
                    (+ (* (Math/cos (* tau u)) a) (* (Math/sin (* tau u)) b))
                    (list '+ (list '* (list 'cos (list '* 'tau u)) a)
                          (list '* (list 'sin (list '* 'tau u)) b))))
        pencil-first pencil-second))

(defn- quad-form [coeff p]
  (let [[A B C D E F] coeff [x y w] p]
    (list '+ (list '* A x x) (list '* B x y) (list '* C y y)
          (list '* D x w) (list '* E y w) (list '* F w w))))

(defn- pencil-bindings [coeff]
  (let [[A B C D E F] coeff
        body (quad-form coeff ['x 'y 1.0])
        d (hyp (list '+ (list '* 2.0 A 'x) (list '* B 'y) D)
               (list '+ (list '* B 'x) (list '* 2.0 C 'y) E))
        dc (hyp A C)
        scale (list '/ 1.0 (list 'max 1.0e-9 (hyp dc (hyp D E))))]
    ['pencil_q (list '* scale body) 'pencil_g (list '* scale d)
     'pencil_d (list '/ (list 'abs 'pencil_q) (list 'max 0.001 'pencil_g))
     'base0 (point-distance (pencil-bases 0)) 'base1 (point-distance (pencil-bases 1))
     'base2 (point-distance (pencil-bases 2)) 'base3 (point-distance (pencil-bases 3))]))

(def conic-pencil
  "All conics through four points. A circular homogeneous coefficient path
   visits ellipses, hyperbolas and the degenerate members of the pencil."
  (layer :conic_pencil "A pencil of conics through four invariant projective points" 18
         (pencil-bindings (pencil-coefficients 'u))
         (let [c (neon/glow-form 'pencil_d 0.035)
               b (list '+ (neon/glow-form 'base0 0.045) (neon/glow-form 'base1 0.045)
                       (neon/glow-form 'base2 0.045) (neon/glow-form 'base3 0.045))]
           [(list '+ (list '* 0.9 c) (list '* 0.9 b))
            (list '+ (list '* 0.65 c) (list '* 0.75 b))
            (list '+ (list '* 0.12 c) (list '* 0.12 b))])))

(def layers [dandelin-conics desargues pascal-brianchon pole-polar conic-pencil])

(def loops
  "The projective gallery, rendered from exactly the live field forms."
  (into (array-map)
        (map (fn [{:keys [id title period form] :as l}]
               [(keyword (clojure.string/replace (name id) "_" "-"))
                {:id (keyword (clojure.string/replace (name id) "_" "-"))
                 :title title :seconds period :fps 30 :size [1920 1080] :view view
                 :field (fn [u] (bind l u))
                 :post (fn [_] [{:type :bloom :radius 12 :strength 0.75}
                                {:type :vignette :strength 0.35}])}]))
        layers))

(defn projective-probe
  "Numerical invariants of a scene at phase u and point [x y]. These are the
   distances the neon strokes use, evaluated without colour quantization."
  [id u sym x y]
  (let [l (first (filter #(= id (:id %)) layers))
        bv (second (:body (:form l)))
        bs (vec (take-while (fn [[s _]] (not= s sym)) (partition 2 bv)))
        bs (conj bs sym (get (apply hash-map bv) sym))]
    (when-let [f (field/jvm-shader (field/bind {:params '[u] :body (list 'let bs [sym 0.0 0.0])} u))]
      (double (/ (bit-and 255 (bit-shift-right (.invokePrim ^clojure.lang.IFn$DDL f (double x) (double y)) 16)) 255.0)))))
