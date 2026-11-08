(ns tessera.figures
  "Curves and knots drawn as light. Every time term is a whole multiple of
   2 pi u: phases turn whole cycles, figures turn by their own symmetry
   angles, and comets of light run whole laps, so frame N is frame 0."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.space :as sp]
            [tessera.palette :as pal]))

(def view [(/ 128.0 9.0) 8.0])

(defn- hex [^long c] (format "#%06x" c))

(defn comets
  "Brightness 0..1 at curve parameter s of `n` comets with heads at
   h + k/n, tails `len` long (s and h in turns of the closed curve)."
  ^double [^double h ^long n ^double s ^double len]
  (loop [k 0 best 0.0]
    (if (= k n)
      best
      (let [d (lp/frac (- (+ h (/ (double k) n)) s))]
        (recur (inc k) (if (< d len) (max best (Math/pow (- 1.0 (/ d len)) 1.8)) best))))))

(defn glow-strokes
  "Line shapes of polyline `pts` (scene [x y]) with per-segment brightness
   (bright i) in 0..1 and colour (color i): a wide dim stroke under a thin
   core, so the figure reads as a lit filament and the comets as hot spots.
   :order, a seq of segment indices, sets the drawing order (overlapping
   translucent strokes composite in order); default along the polyline."
  [pts bright color {:keys [width core order] :or {width 2.4 core 1.0}}]
  (let [pts (vec pts)
        segs (map (fn [i] [i (nth pts i) (nth pts (inc i))])
                  (or order (range (dec (count pts)))))]
    (concat
     (for [[i a b] segs :let [k (double (bright i))]]
       {:node :line :from a :to b :opacity (+ 0.10 (* 0.30 k))
        :stroke {:color (color i) :width (* width (+ 1.0 (* 2.0 k)))}})
     (for [[i a b] segs :let [k (double (bright i))] :when (> k 0.02)]
       {:node :line :from a :to b :opacity (min 0.75 (* 0.9 k))
        :stroke {:color (if (> k 0.8) "#e8d2bc" (color i)) :width (* core (+ 0.8 (* 1.4 k)))}}))))

;; ── Harmonograph ─────────────────────────────────────────────────────────

(def harmonograph-pendulums
  "Per axis, terms [amplitude frequency turns-per-loop phase0]: frequencies
   near 2:3 with a small detuning draw the woven rose; each term's phase
   advances by a whole number of turns per loop."
  {:x [[2.15 2.0 1 0.0] [1.25 3.01 -1 0.7]]
   :y [[2.15 3.0 1 1.57] [1.25 2.0 2 0.3]]})

(def ^:const harmonograph-damping 0.0045)
(def ^:const harmonograph-span 360.0)
(def ^:const harmonograph-samples 2400)

(defn harmonograph-point
  "The pen at drawing time t in [0, span] for loop phase u: each axis a sum
   of damped pendulums, the damping e^(-0.0045 t) shared."
  [u t]
  (let [t (double t)
        d (Math/exp (- (* harmonograph-damping t)))
        term (fn [[a f k p0]] (* (double a) (Math/sin (+ (* (double f) t) (double p0) (* lp/tau (double k) (double u))))))]
    [(* d (reduce + (map term (:x harmonograph-pendulums))))
     (* d (reduce + (map term (:y harmonograph-pendulums))))]))

(def harmonograph-palette (pal/around (pal/scheme :blue-magenta-orange)))

(defn harmonograph-frame
  "The figure drawn over its whole damped run at phase u, three pens of
   light retracing it from the outside in, once per loop."
  [u]
  (let [n harmonograph-samples
        pts (mapv #(harmonograph-point u (* harmonograph-span (/ (double %) n))) (range (inc n)))
        bright (fn [i] (+ 0.10 (* 0.90 (comets (double u) 3 (/ (double i) n) 0.07))))
        color (fn [i] (hex (pal/at harmonograph-palette (+ (* 1.5 (/ (double i) n)) (double u)))))]
    {:background "#000000"
     :shapes (vec (glow-strokes pts bright color {:width 1.2 :core 1.0}))}))

(def harmonograph
  {:id :harmonograph
   :title "Harmonograph: damped pendulums, their phases turning whole cycles"
   :seconds 20 :fps 30 :size [1920 1080] :view view
   :frame harmonograph-frame
   :post (fn [_] [{:type :bloom :radius 14 :strength 1.1}
                  {:type :chroma :shift 2}
                  {:type :vignette :strength 0.55}])})

;; ── Spirograph ───────────────────────────────────────────────────────────

(def spiro-gears
  "Hypotrochoids [R r d]: a wheel of radius r rolling inside a ring of
   radius R, the pen d from the wheel's centre. With R/r = p/q in lowest
   terms the curve closes after q turns of the wheel and has p-fold
   symmetry. Listed in the order the grid is filled, centre column first."
  [[7 2 1.5] [11 4 3.2] [5 3 2.2] [8 3 2.6] [9 4 2.4] [13 5 3.4]
   [10 3 2.4] [12 5 3.0] [7 3 2.1] [11 3 2.6] [9 2 1.6] [13 4 3.0]])

(defn- gcd [a b] (if (zero? b) a (recur b (mod a b))))

(defn spiro-cells
  "Grid cells for a window `half-width` wide either side: columns 5 apart
   about the centre, two rows, each cell given a gear in order. A wider
   window continues the lattice: it gains every column whose figure
   reaches into it (centre within half-width + 0.9), the middle three
   kept where they were, so 2560 x 1080 shows two more columns at the
   edges, as a lattice loop shows more cells."
  [half-width]
  (let [cols (long (Math/floor (/ (+ (double half-width) 0.9) 5.0)))
        xs (sort-by #(Math/abs (double %)) (for [c (range (- cols) (inc cols))] (* 5.0 c)))]
    (vec (map-indexed (fn [i [x y]]
                        {:gear (nth spiro-gears (mod i (count spiro-gears)))
                         :centre [x y] :phase (lp/frac (* 0.61803398875 i))})
                      (for [x xs y [1.9 -1.9]] [x y])))))

(defn spiro-point
  "Point of hypotrochoid [R r d0] with pen arm d at s in [0, 1) (one closed
   traversal), scaled to radius about 1.6 and turned by `turn` about its
   centre."
  [[R r d0] d turn [cx cy] s]
  (let [q (quot r (gcd R r))
        t (* lp/tau q (double s))
        k (/ (- R r) (double r))
        x (+ (* (- R r) (Math/cos t)) (* d (Math/cos (* k t))))
        y (- (* (- R r) (Math/sin t)) (* d (Math/sin (* k t))))
        sc (/ 1.62 (+ (- R r) d0))
        c (Math/cos turn) sn (Math/sin turn)]
    [(+ cx (* sc (- (* c x) (* sn y)))) (+ cy (* sc (+ (* sn x) (* c y))))]))

(def spiro-palette (pal/around (pal/scheme :yellow-magenta-cyan)))

(defn spiro-shift
  "The parameter shift tau = k/p (k in [0, p)) by which turning hypotrochoid
   `gear` by `sign` 2 pi/p maps it onto itself: turn(c(s)) = c(s + tau).
   Found by testing each k on a few sample points."
  [[R r d0 :as gear] sign]
  (let [p (quot R (gcd R r))
        turn (* sign (/ lp/tau p))
        err (fn [k] (reduce + (for [s [0.013 0.271 0.538 0.804]
                                    :let [[x0 y0] (spiro-point gear d0 turn [0.0 0.0] s)
                                          [x1 y1] (spiro-point gear d0 0.0 [0.0 0.0] (+ s (/ (double k) p)))]]
                                (Math/hypot (- x0 x1) (- y0 y1)))))]
    (/ (double (apply min-key err (range p))) p)))

(defn spirograph-frame
  "The cells at phase u: each figure turns by its symmetry angle 2 pi/p per
   loop (alternate cells counter-rotating), its pen arm breathes on one
   cycle, and two comets run its closed path. The turn maps the curve onto
   itself shifted in parameter by tau (`spiro-shift`), so comets and colour
   are keyed on s + tau u: at u = 1 every vertex carries the light its new
   position had at u = 0, and frame N is frame 0."
  [cells u]
  (let [u (double u)]
    {:background "#000000"
     :shapes
     (vec (mapcat
           (fn [{[R r d0 :as gear] :gear :keys [centre phase]} sign]
             (let [p (quot R (gcd R r))
                   tau (spiro-shift gear sign)
                   turn (* sign (/ lp/tau p) u)
                   d (* d0 (+ 1.0 (* 0.16 (Math/sin (* lp/tau (+ u phase))))))
                   ;; a multiple of p, so the turned vertices are the same
                   ;; set shifted by tau
                   n (* p (long (Math/ceil (/ 900.0 p))))
                   pts (mapv #(spiro-point gear d turn centre (/ (double %) n)) (range (inc n)))
                   s-of (fn [i] (+ (/ (double i) n) (* tau u)))
                   bright (fn [i] (+ 0.12 (* 0.88 (comets (+ u phase) 2 (s-of i) 0.09))))
                   color (fn [i] (hex (pal/at spiro-palette (+ phase (s-of i) u))))
                   ;; draw in order of the curve's own parameter, which the
                   ;; turn preserves, so overlaps composite the same way at
                   ;; u = 0 and u = 1
                   k0 (long (Math/round (* tau u n)))
                   order (map #(mod (- % k0) n) (range n))]
               (glow-strokes pts bright color {:width 1.2 :core 1.0 :order order})))
           cells (cycle [1.0 -1.0])))}))

(defn spirograph-at
  "The spirograph over a window of half-width `half-width`."
  [half-width]
  (let [cells (spiro-cells half-width)]
    {:id :spirograph
     :title "Spirograph: hypotrochoids, each turning by its own symmetry"
     :seconds 16 :fps 30 :size [1920 1080] :view view
     :frame (fn [u] (spirograph-frame cells u))
     :post (fn [_] [{:type :bloom :radius 12 :strength 1.1}
                    {:type :chroma :shift 2}
                    {:type :vignette :strength 0.5}])}))

(def spirograph
  (assoc (spirograph-at (/ (first view) 2.0))
         :reframe (fn [[w _]] (spirograph-at (/ (double w) 2.0)))))

;; ── Knot tubes ───────────────────────────────────────────────────────────

(def knot-camera
  (sp/camera {:eye [0.0 -13.0 5.0] :target [0.0 0.0 -0.2]
              :up [0.0 0.0 1.0] :fov 34.0 :near 0.1 :view view}))

(defn trefoil
  "The trefoil in its three-fold symmetric form at t in turns: a turn of
   2 pi/3 about z is the shift t -> t + 1/3."
  [t]
  (let [a (* lp/tau (double t))]
    [(+ (Math/sin a) (* 2.0 (Math/sin (* 2.0 a))))
     (- (Math/cos a) (* 2.0 (Math/cos (* 2.0 a))))
     (- (Math/sin (* 3.0 a)))]))

(defn figure-eight-knot
  "The figure-eight knot ((2 + cos 2t) cos 3t, (2 + cos 2t) sin 3t, sin 4t)
   at t in turns: a half turn about z is the shift t -> t + 1/2."
  [t]
  (let [a (* lp/tau (double t))
        r (+ 2.0 (Math/cos (* 2.0 a)))]
    [(* r (Math/cos (* 3.0 a))) (* r (Math/sin (* 3.0 a))) (Math/sin (* 4.0 a))]))

(defn- place
  "Local point [x y z] turned by `a` about its z axis, tilted toward the
   camera by `tilt`, scaled and moved to `centre`."
  [[x y z] a [cx cy cz] scale tilt]
  (let [c (Math/cos a) s (Math/sin a)
        x1 (- (* c x) (* s y)) y1 (+ (* s x) (* c y))
        ct (Math/cos tilt) st (Math/sin tilt)
        y2 (- (* ct y1) (* st z)) z2 (+ (* st y1) (* ct z))]
    [(+ cx (* scale x1)) (+ cy (* scale y2)) (+ cz (* scale z2))]))

(def knots
  "The two knots: curve, symmetry order (the turn per loop is 2 pi/sym, the
   matching parameter shift 1/sym), placement, palette and pulse count."
  [{:curve trefoil :sym 3 :centre [-3.7 0.0 0.0] :scale 0.95 :tilt 0.5 :pulses 3
    :palette (pal/cosine [0.20 0.72 0.88] [0.12 0.22 0.11] [1.0 1.0 1.0] [0.0 0.08 0.17])}
   {:curve figure-eight-knot :sym 2 :centre [3.7 0.0 0.0] :scale 0.88 :tilt 0.5 :pulses 2
    :palette (pal/cosine [0.89 0.43 0.10] [0.10 0.26 0.08] [1.0 1.0 1.0] [0.0 0.12 0.24])}])

(def ^:const knot-samples 600)

(defn knot-frame
  "The knots at phase u as glowing tubes: each segment drawn as a wide halo,
   a body and a hot core, nearer segments over farther ones and lighter.
   Each knot turns by its symmetry angle per loop and its pulses and colour
   are keyed on s - u/sym, the parameter the turn carries a point to, so
   frame N is frame 0. Pulses also run a whole lap per loop."
  [u]
  (let [u (double u)
        cue {:range [10.0 17.0] :alpha [1.0 0.55] :size [1.15 0.75]}]
    (sp/frame
     (mapcat
      (fn [{:keys [curve sym centre scale tilt pulses palette]}]
        (let [n knot-samples
              a (* (/ lp/tau sym) u)
              shift (/ u (double sym))
              pts (mapv #(place (curve (/ (double %) n)) a centre scale tilt) (range (inc n)))]
          (for [i (range n)
                :let [s (- (/ (double i) n) shift)
                      [x0 y0 d0] (sp/project knot-camera (nth pts i))
                      [x1 y1 d1] (sp/project knot-camera (nth pts (inc i)))
                      d (* 0.5 (+ d0 d1))
                      {:keys [alpha size]} (sp/depth-cue cue d)
                      k (comets u pulses s 0.11)
                      shade (- 1.0 (* 0.5 (sp/clamp01 (/ (- d 10.5) 5.0))))
                      col (hex (pal/at palette (+ (* 2.0 s) (* 0.05 (lp/wave u)))))
                      mag (* 30.0 (sp/magnification knot-camera d))]
                [w o c dz] [[(* 0.16 mag) (* 0.40 shade) col 0.0]
                            [(* 0.085 mag) (* 0.85 shade) col 1.0e-4]
                            [(* 0.03 mag (+ 1.0 k)) (min 0.9 (+ (* 0.3 shade) (* 0.8 k)))
                             (if (> k 0.5) "#f0e2d0" "#d8eef4") 2.0e-4]]]
            {:node :line :from [x0 y0] :to [x1 y1] :depth (- d dz)
             :opacity (* alpha o) :stroke {:color c :width (* size w)}})))
      knots)
     {:background "#010306"})))

(def knot-tubes
  {:id :knot-tubes
   :title "Trefoil and figure-eight knots as neon tubes, each turning by its symmetry"
   :seconds 18 :fps 30 :size [1920 1080] :view view
   :frame knot-frame
   :post (fn [_] [{:type :bloom :radius 13 :strength 0.9}
                  {:type :vignette :strength 0.3}])})

(def loops
  "The curve and knot loops, by id."
  (array-map :harmonograph harmonograph :spirograph spirograph :knot-tubes knot-tubes))


