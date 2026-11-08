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
   core, so the figure reads as a lit filament and the comets as hot spots."
  [pts bright color {:keys [width core] :or {width 2.4 core 1.0}}]
  (let [segs (map vector (range) pts (rest pts))]
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

(def loops
  "The curve and knot loops, by id."
  (array-map :harmonograph harmonograph))
