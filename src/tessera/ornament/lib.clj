(ns tessera.ornament.lib
  "Ornament SVG library: seeded, deterministic vector ornament.

   Geometry is plain data: a motif is {:id :paths [{:d \"M..\" :fill .. :stroke ..}]}
   in a y-down SVG plane. Symmetry acts on points: Cn (rotations), Dn
   (rotations and mirrors), and the seven frieze groups along a strip.
   Primitives (droplet, pin, bead, band, sun) are closed outlines, so
   offsetting them gives clean rings. Every motif is emitted as its own
   <g id=..>, so a per-shape id mask (svg-offset Order=Group) splits them.

   Plane vectors and the girih tilings come from desargues.wallpaper.girih,
   colour from tessera.palette. Same seed, same SVG, byte for byte."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [desargues.wallpaper.girih :as g]
            [desargues.wallpaper.loop :as lp]
            [tessera.palette :as pal]
            [desargues.player.svg-path :as sp])
  (:import (java.util Random)))

(set! *warn-on-reflection* true)

(defn rng
  "A java.util.Random for `seed`: same seed, same stream."
  ^Random [seed]
  (Random. (long seed)))

(defn uniform
  "A double in [lo, hi) from `r`."
  ^double [^Random r lo hi]
  (+ (double lo) (* (- (double hi) (double lo)) (.nextDouble r))))

(defn pick
  "One element of `coll`, chosen by `r`."
  [^Random r coll]
  (nth (vec coll) (.nextInt r (count coll))))

(def identity-xf
  "Affine maps are [a b c d e f] in SVG matrix order:
   [x y] -> [a x + c y + e, b x + d y + f]. They send a cubic's control
   points to the image curve's, so motifs transform exactly."
  [1.0 0.0 0.0 1.0 0.0 0.0])

(defn xf-apply
  "Point [x y] under affine map `m`."
  [[a b c d e f] [x y]]
  [(+ (* a x) (* c y) e) (+ (* b x) (* d y) f)])

(defn xf*
  "Composition: (xf* m n) applies n first. Variadic, right to left."
  ([m] m)
  ([[a b c d e f] [a' b' c' d' e' f']]
   [(+ (* a a') (* c b')) (+ (* b a') (* d b'))
    (+ (* a c') (* c d')) (+ (* b c') (* d d'))
    (+ (* a e') (* c f') e) (+ (* b e') (* d f') f)])
  ([m n & more] (xf* m (apply xf* n more))))

(defn rotation
  "Turn by `theta` radians about the origin."
  [theta]
  (let [c (Math/cos theta) s (Math/sin theta)]
    [c s (- s) c 0.0 0.0]))

(defn mirror
  "Reflection in the line through the origin at angle `theta`."
  [theta]
  (let [c (Math/cos (* 2.0 theta)) s (Math/sin (* 2.0 theta))]
    [c s s (- c) 0.0 0.0]))

(defn translation [dx dy] [1.0 0.0 0.0 1.0 (double dx) (double dy)])

(defn scaling
  ([k] (scaling k k))
  ([kx ky] [(double kx) 0.0 0.0 (double ky) 0.0 0.0]))

(defn about
  "Map `m` acting about point [cx cy] instead of the origin."
  [[cx cy] m]
  (xf* (translation cx cy) m (translation (- cx) (- cy))))

(defn cyclic
  "The cyclic group Cn: the n rotations by multiples of 2π/n."
  [n]
  (mapv #(rotation (/ (* lp/tau %) n)) (range n)))

(defn dihedral
  "The dihedral group Dn, order 2n: the rotations of Cn and n mirrors whose
   axes lie π/n apart, the first along the x axis."
  [n]
  (into (cyclic n) (map #(mirror (/ (* Math/PI %) n)) (range n))))

(defn point-group
  "[:c n] or [:d n] as its vector of maps."
  [[kind n]]
  (case kind :c (cyclic n) :d (dihedral n)))

(defn seg-xf
  "Segment (desargues.player.svg-path form: [:move x y] [:line x y]
   [:quad x1 y1 x y] [:cubic x1 y1 x2 y2 x y] [:close]) under map `m`."
  [m [k & nums]]
  (if (= k :close)
    [:close]
    (into [k] (mapcat #(xf-apply m %) (partition 2 nums)))))

(defn segs-xf
  "Every segment of `segs` under map `m`."
  [m segs]
  (mapv #(seg-xf m %) segs))

(defn- fmt
  "A coordinate, 3 decimals, no trailing zeros, never \"-0\"."
  [x]
  (let [s (format "%.3f" (double x))
        s (str/replace s #"\.?0+$" "")]
    (if (= s "-0") "0" s)))

(defn segs->d
  "The SVG path `d` of absolute segments."
  [segs]
  (str/join " "
            (for [[k & nums] segs]
              (str (case k :move "M" :line "L" :quad "Q" :cubic "C" :close "Z")
                   (when (seq nums) (str " " (str/join " " (map fmt nums))))))))

(defn d->segs
  "Absolute segments of SVG path string `d` (desargues.player.svg-path)."
  [d]
  (sp/segments d))

(defn polygon
  "Closed polygon through `pts`."
  [pts]
  (let [[[x y] & more] pts]
    (-> [[:move x y]] (into (map (fn [[x y]] [:line x y])) more) (conj [:close]))))

(defn polyline
  "Open polyline through `pts`."
  [pts]
  (let [[[x y] & more] pts]
    (into [[:move x y]] (map (fn [[x y]] [:line x y])) more)))

(defn circle
  "Closed circle of radius `r` about [cx cy], as four cubics."
  [[cx cy] r]
  (let [k (* 0.5522847498 r)]
    [[:move (+ cx r) cy]
     [:cubic (+ cx r) (+ cy k) (+ cx k) (+ cy r) cx (+ cy r)]
     [:cubic (- cx k) (+ cy r) (- cx r) (+ cy k) (- cx r) cy]
     [:cubic (- cx r) (- cy k) (- cx k) (- cy r) cx (- cy r)]
     [:cubic (+ cx k) (- cy r) (+ cx r) (- cy k) (+ cx r) cy]
     [:close]]))

(defn droplet
  "A teardrop of length `len` and belly radius `r`: the point at the origin,
   the belly centred at [len - r, 0] on the +x axis. The two flanks are
   tangent lines from the tip to the belly circle, closed by its far arc
   (cubics), so the outline is G1 everywhere but at the tip."
  [len r]
  (let [len (double len) r (double r)
        c (- len r)
        a (Math/acos (/ r c))          ; tangent point angle, from the centre
        t1 [(+ c (* r (Math/cos (- Math/PI a)))) (* r (Math/sin (- Math/PI a)))]
        arc (fn [a0 a1]                ; cubic for a circle arc a0 -> a1
              (let [k (* (/ 4.0 3.0) (Math/tan (/ (- a1 a0) 4.0)))
                    p (fn [t] [(+ c (* r (Math/cos t))) (* r (Math/sin t))])
                    [x0 y0] (p a0) [x1 y1] (p a1)]
                [:cubic (- x0 (* k r (Math/sin a0))) (+ y0 (* k r (Math/cos a0)))
                 (+ x1 (* k r (Math/sin a1))) (- y1 (* k r (Math/cos a1))) x1 y1]))
        a0 (- a Math/PI) a1 (- Math/PI a)
        n 4 step (/ (- a1 a0) n)]
    (-> [[:move 0.0 0.0] [:line (first t1) (- (second t1))]]
        (into (map #(arc (+ a0 (* % step)) (+ a0 (* (inc %) step)))) (range n))
        (conj [:close]))))

(defn pin
  "A pin: a slender droplet of length `len` with a round head of radius `r`
   past its end: two closed subpaths of one winding, so they fill and
   offset as their union."
  [len r]
  (let [len (double len) r (double r)]
    (into (droplet (* 0.82 len) (* 0.38 r))
          (circle [(- len r) 0.0] r))))

(defn bead
  "A bead: an ellipse with semi-axes `rx` along x and `ry` across, centred
   at [cx cy]."
  [[cx cy] rx ry]
  (segs-xf (xf* (translation cx cy) (scaling rx ry)) (circle [0.0 0.0] 1.0)))

(defn bead-chain
  "Beads every `spacing` along polyline `pts` (desargues girih/beads), each
   turned along the line; `phase` slides them along by whole spacings."
  [pts spacing rx ry phase closed?]
  (let [centres (g/beads pts spacing phase closed?)]
    (vec (for [[i c] (map-indexed vector centres)
               :let [nxt (nth centres (mod (inc i) (count centres)))
                     [dx dy] (g/v- nxt c)
                     ang (Math/atan2 dy dx)]]
           (segs-xf (about c (rotation ang)) (bead c rx ry))))))

(defn band
  "An offset sine band: the strip between y = ±w/2 + a sin(2π k x / L) over
   x in [0, L], k whole waves, closed so its offset is a clean ring."
  [L w a k]
  (let [n (* 24 (max 1 (long k)))
        xs (map #(* (double L) (/ % n)) (range (inc n)))
        y (fn [x] (* (double a) (Math/sin (/ (* lp/tau k x) L))))
        top (map (fn [x] [x (- (y x) (/ w 2.0))]) xs)
        bot (map (fn [x] [x (+ (y x) (/ w 2.0))]) (reverse xs))]
    (polygon (concat top bot))))

(defn sun
  "A sun disc: `n` rays around a disc of radius `r-in`, alternating long
   (tip at `r-out`) and short (tip 55% of the way), as one closed star
   outline. Each ray's base spans `sharp` (0, 1] of its share of the circle,
   so small `sharp` gives needle rays with arcs of disc between them."
  [n r-in r-out sharp]
  (let [n (long n) r-in (double r-in) r-out (double r-out)
        half (* (double sharp) (/ Math/PI n))
        at (fn [r t] [(* r (Math/cos t)) (* r (Math/sin t))])]
    (polygon
     (mapcat (fn [i]
               (let [t (/ (* lp/tau i) n)
                     tip (if (even? i) r-out (+ r-in (* 0.55 (- r-out r-in))))
                     gap (- (/ Math/PI n) half)]
                 (concat [(at r-in (- t half)) (at tip t) (at r-in (+ t half))]
                         (when (pos? gap) [(at r-in (+ t (/ Math/PI n)))]))))
             (range n)))))

(defn motif
  "A motif: {:id id :paths [{:segs .. :fill .. :stroke .. :width ..}]}.
   One motif becomes one <g>."
  [id & paths]
  {:id (str id) :paths (vec paths)})

(defn place
  "Motif `m` under map `xf`, renamed `id`."
  [m xf id]
  (-> m (assoc :id (str id))
      (update :paths (fn [ps] (mapv #(update % :segs (partial segs-xf xf)) ps)))))

(defn orbit
  "The images of motif `m` under every map in `group` (a vector of maps,
   e.g. (dihedral 6)) taken about point `c`, one motif each, ids
   <prefix>-0, <prefix>-1 .."
  [m group c prefix]
  (vec (map-indexed (fn [i h] (place m (about c h) (str prefix "-" i))) group)))

(defn hex
  "A palette value (0xRRGGBB long) or a colour string as an SVG colour."
  [c]
  (if (number? c) (format "#%06x" (long c)) c))

(defn- esc [s] (str/escape (str s) {\< "&lt;" \> "&gt;" \& "&amp;" \" "&quot;"}))

(defn path->svg
  [{:keys [segs fill stroke width opacity]}]
  (str "<path d=\"" (segs->d segs) "\""
       " fill=\"" (if fill (hex fill) "none") "\""
       (when stroke (str " stroke=\"" (hex stroke) "\" stroke-width=\"" (fmt (or width 1.0))
                         "\" stroke-linejoin=\"round\" stroke-linecap=\"round\""))
       (when opacity (str " opacity=\"" (fmt opacity) "\""))
       " fill-rule=\"nonzero\"/>"))

(defn motif->svg
  "One <g id=..> per motif."
  [{:keys [id paths]}]
  (str "  <g id=\"" (esc id) "\">"
       (str/join "" (map path->svg paths))
       "</g>"))

(defn svg
  "A whole SVG document of `motifs` on a `w` x `h` canvas. `background`, when
   given, is the first <g id=\"background\">; metadata {:series :seed ..} goes
   into a <desc>. Coordinates rounded to 1e-3, so equal input gives equal bytes."
  [{:keys [w h background meta]} motifs]
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
       "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" w "\" height=\"" h
       "\" viewBox=\"0 0 " w " " h "\">\n"
       (when meta (str "  <desc>" (esc (pr-str meta)) "</desc>\n"))
       (when background
         (str "  <g id=\"background\"><rect width=\"" w "\" height=\"" h
              "\" fill=\"" (hex background) "\"/></g>\n"))
       (str/join "\n" (map motif->svg motifs))
       "\n</svg>\n"))

(defn write-svg!
  "Write `doc` to art/svg/<series>/<name>.svg under `root` (default \".\");
   returns the file."
  ([series name doc] (write-svg! "." series name doc))
  ([root series name doc]
   (let [f (io/file root "art" "svg" (str series) (str name ".svg"))]
     (io/make-parents f)
     (spit f doc)
     f)))

(defn colour
  "Palette `p` (tessera.palette primitive) at `t` as an SVG colour."
  [p t]
  (hex (pal/at p (double t))))

(def palettes
  "Named palettes a seed may draw from."
  {:lapis-gold pal/lapis-gold :ember pal/ember})

(defn rosette
  "A seeded rosette about [cx cy] of radius `R`: a focal sun disc, then rings
   of droplets, pins and beads, each ring the orbit of ONE primitive under the
   point group [:c n] or [:d n]. Under Dn the primitive sits off its mirror
   axis (so the mirror pairs are visibly distinct shapes); under Cn it leans,
   which is what makes the pattern chiral. Returns motifs, one per placed
   primitive, ids <prefix>-<ring>-<k>."
  [seed [cx cy] R {:keys [group palette prefix] :or {prefix "r"}}]
  (let [r (rng seed)
        n (or (second group) (pick r [5 6 7 8 9 10 12]))
        kind (or (first group) (pick r [:c :d]))
        p (get palettes (or palette (pick r (sort (keys palettes)))))
        c [cx cy]
        focal (motif (str prefix "-sun")
                     {:segs (segs-xf (translation cx cy)
                                     (sun n (* 0.13 R) (* 0.24 R) (uniform r 0.35 0.8)))
                      :fill (colour p 0.92)})
        rings (for [ring (range 3)
                    :let [rad (* R (+ 0.32 (* 0.24 ring)))
                          prim (pick r [:droplet :pin :bead])
                          size (* R (uniform r 0.12 0.2))
                          lean (if (= kind :d) 0.0 (uniform r 0.25 0.7))
                          off (if (= kind :d) (uniform r 0.08 0.3) 0.0)
                          shape (case prim
                                  :droplet (droplet size (* 0.3 size))
                                  :pin (pin size (* 0.18 size))
                                  :bead (bead [(* 0.5 size) 0.0] (* 0.5 size) (* 0.22 size)))
                          base (segs-xf (xf* (translation (+ cx rad) cy)
                                             (rotation (/ (* off Math/PI) n))
                                             (rotation lean)
                                             (translation (* -0.5 size) 0.0))
                                        shape)
                          m (motif "_" {:segs base :fill (colour p (+ 0.15 (* 0.3 ring)))})]]
                (orbit m (point-group [kind n]) c (str prefix "-" ring)))]
    {:group [kind n] :motifs (into [focal] cat rings)}))

(defn frieze-cell
  "The seven frieze groups, along the x axis (the strip's centre line) with
   translation period `T`: the coset representatives of one period, as
   maps. The whole group is these composed with translations by kT.

     :p1    hop             translations only
     :p11g  step            glide reflection (half period along, mirrored across)
     :p1m1  sidle           vertical mirrors
     :p2    spinning hop    half turns
     :p2mg  spinning sidle  vertical mirrors and glides (so half turns)
     :p11m  jump            the horizontal mirror
     :p2mm  spinning jump   both mirrors (so half turns)"
  [group T]
  (let [I identity-xf
        Rh (mirror 0.0)
        Rv (mirror (/ Math/PI 2.0))
        H (rotation Math/PI)
        G (xf* (translation (/ T 2.0) 0.0) Rh)]
    (case group
      :p1 [I]
      :p11g [I G]
      :p1m1 [I Rv]
      :p2 [I H]
      :p2mg [I Rv G (xf* G Rv)]
      :p11m [I Rh]
      :p2mm [I Rh Rv H])))

(def frieze-groups
  "The seven frieze groups, in order."
  [:p1 :p11g :p1m1 :p2 :p2mg :p11m :p2mm])

(defn frieze
  "A seeded frieze of group `group` (see frieze-cell) on the strip whose
   centre line is y = `cy`, from x = `x0` over `periods` periods of `T`.
   The motif is ONE primitive placed in the fundamental domain, off every
   mirror and rotation centre and leaning, so it has no symmetry of its own
   and the strip's symmetry is exactly the group's. Returns motifs, ids
   <prefix>-<period>-<coset>; under :background? it also sets an offset sine
   band under the motifs as its own motif."
  [seed group {:keys [T periods x0 cy palette prefix band?]
               :or {T 120.0 periods 8 x0 0.0 cy 0.0 prefix "f"}}]
  (let [r (rng seed)
        p (get palettes (or palette (pick r (sort (keys palettes)))))
        prim (pick r [:droplet :pin :bead])
        size (* T (uniform r 0.26 0.36))
        lean (uniform r 0.35 1.1)
        shape (case prim
                :droplet (droplet size (* 0.3 size))
                :pin (pin size (* 0.17 size))
                :bead (bead [(* 0.5 size) 0.0] (* 0.5 size) (* 0.2 size)))
        local (segs-xf (xf* (translation (* T 0.13) (* T 0.08)) (rotation lean)) shape)
        tint (uniform r 0.1 0.9)
        m (motif "_" {:segs local :fill (colour p tint)})
        cell (frieze-cell group T)
        placed (for [k (range periods) [j h] (map-indexed vector cell)]
                 (place m (xf* (translation (+ x0 (* k T) (/ T 4.0)) cy) h)
                        (str prefix "-" k "-" j)))
        strip (motif (str prefix "-band")
                     {:segs (segs-xf (translation x0 cy)
                                     (band (* periods T) (* 0.08 T) (* 0.36 T) (* 2 periods)))
                      :fill (colour p (+ tint 0.5)) :opacity 0.55})]
    (vec (if band? (cons strip placed) placed))))

(defn primitives-sheet
  "Every primitive once, each its own <g id=prim-..>, on a 1200 x 400 sheet:
   droplet, pin, bead, a bead chain, an offset sine band and a sun disc."
  [seed]
  (let [r (rng seed)
        p (get palettes (pick r (sort (keys palettes))))
        at (fn [x y segs] (segs-xf (translation x y) segs))
        fill (fn [t] (colour p t))]
    (into
     [(motif "prim-droplet" {:segs (at 40.0 130.0 (droplet 150.0 42.0)) :fill (fill 0.2)})
      (motif "prim-pin" {:segs (at 240.0 130.0 (pin 170.0 24.0)) :fill (fill 0.4)})
      (motif "prim-bead" {:segs (bead [520.0 130.0] 60.0 28.0) :fill (fill 0.6)})
      (motif "prim-sun" {:segs (at 980.0 200.0 (sun 12 70.0 150.0 (uniform r 0.4 0.7)))
                         :fill (fill 0.9)})
      (motif "prim-band" {:segs (at 40.0 330.0 (band 760.0 22.0 26.0 5)) :fill (fill 0.75)})]
     (map-indexed (fn [i segs] (motif (str "prim-bead-chain-" i) {:segs segs :fill (fill 0.5)}))
                  (bead-chain [[60.0 240.0] [380.0 210.0] [760.0 250.0]] 34.0 13.0 6.0 0.0 false)))))

(defn rosette-doc
  "SVG of the seeded rosette `seed` (1024 x 1024)."
  [seed opts]
  (let [{:keys [group motifs]} (rosette seed [512.0 512.0] 470.0 opts)]
    (svg {:w 1024 :h 1024 :background pal/ink
          :meta {:series "rosette" :seed seed :group group}}
         motifs)))

(defn frieze-doc
  "SVG of one frieze of `group`, seed `seed` (1200 x 240, 8 periods)."
  [seed group]
  (svg {:w 1200 :h 240 :background pal/ink
        :meta {:series "frieze" :seed seed :group group}}
       (frieze seed group {:T 150.0 :periods 8 :x0 0.0 :cy 120.0 :band? true
                           :prefix (name group)})))
