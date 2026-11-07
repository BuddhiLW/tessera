(ns tessera.curves
  "Space-filling curves as loops. Each curve is parametrised by arc length
   s in [0, 1]; colour and light are functions of s - u, so they flow along
   the curve and wrap with the loop.

     :moore-comets  two closed Moore curves side by side, colour flowing
                    along them (a thi.ng/color scheme walked there and back),
                    three comets of light running around each"
  (:require [desargues.wallpaper.curves :as cv]
            [desargues.wallpaper.loop :as lp]
            [tessera.palette :as pal]))

(defn- hex [^long c] (format "#%06x" c))

(defn tiled
  "Segments [a b s offset] of curve `k` at order `n`, one copy per centre in
   `centres`, scaled by `scale`; copy i carries phase offset i/count."
  [k n scale centres]
  (let [base (cv/curve k n)
        m (count centres)]
    (vec (for [[i [cx cy]] (map-indexed vector centres)
               :let [pts (mapv (fn [[x y]] [(+ cx (* scale x)) (+ cy (* scale y))]) base)]
               [a b s] (map vector pts (rest pts) (cv/arc-params pts))]
           [a b s (/ (double i) m)]))))

(defn comet
  "Brightness 0..1 at arc position `s` of comets whose heads sit at `heads`,
   tails `len` long behind them."
  ^double [heads ^double s ^double len]
  (reduce max 0.0 (for [h heads
                        :let [d (lp/frac (- (double h) s))]]
                    (if (< d len) (Math/pow (- 1.0 (/ d len)) 1.6) 0.0))))

(defn flow-frame
  "Frame of segments `segs` at phase `u`: base colour from palette `p` at
   s - u, comets brighten and whiten it."
  [segs p u {:keys [comets tail flow] :or {comets 3 tail 0.12 flow 1.0}}]
  (let [heads (fn [off] (map #(+ (/ (double %) comets) off u) (range comets)))]
    {:background "#000000"
     :shapes (vec (for [[a b s off] segs
                        :let [hit (comet (heads off) s tail)
                              base (pal/at p (* flow (- s u)))]]
                    {:node :line :from a :to b :opacity (+ 0.3 (* 0.7 hit))
                     :stroke {:color (if (> hit 0.6) "#fff3e0" (hex base)) :width (+ 2.2 (* 3.0 hit))}}))}))

(def moore-comets
  (let [segs (tiled :moore 5 3.55 [[-3.6 0.0] [3.6 0.0]])
        p (pal/around (pal/scheme :blue-magenta-orange))]
    {:id :moore-comets
     :title "Moore curves: colour flowing along, comets of light running around"
     :seconds 12 :fps 30 :size [1920 1080]
     :frame (fn [u] (flow-frame segs p u {}))
     :post (fn [_] [{:type :bloom :radius 16 :strength 1.3}
                    {:type :chroma :shift 3}
                    {:type :vignette :strength 0.6}])}))
