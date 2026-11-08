(ns tessera.layers.ports.uv
  "Field ports of the black-ground UV ornament loops: the textile tunnel and
   sprite lattice (tessera.vj), and the hyperspace warp and flower mandala
   (tessera.vj2).

   Beyond the techniques of tessera.layers.ports.forms:
   - Math/hypot is sqrt(x*x + y*y). hypot is correctly rounded and the sqrt
     form can differ in the last ulp; the parity tests measure the effect at
     the 8-bit output (none seen) rather than assume it.
   - Clojure's double (mod a s) is a - s*trunc(a/s), plus s when the
     remainder is nonzero and its sign differs from s's (`cmod`).
   - The exposure (neon/uv) and the 8-bit packing (r/rgb, blend) of the
     closures are restated as masks; a closure that blends two PACKED
     colours is reproduced on integer levels, floor(255 c + 1/2).
   - sprite-lattice's per-frame 24 x 16 glyph table is re-derived per pixel
     at each of the nine neighbouring cells the closure reads."
  (:require [clojure.walk :as walk]
            [desargues.wallpaper.loop :as lp]
            [tessera.layers.ports.forms :as f]
            [tessera.vj2 :as vj2]))

(def ^:private log2 (Math/log 2.0))

(def ^:private half12 (/ Math/PI 12.0))

(defn- hyp
  "sqrt(a^2 + b^2): Math/hypot without its correct rounding."
  [a b]
  (list 'sqrt (list '+ (list '* a a) (list '* b b))))

(defn- glow
  "The closures' filament: a Gaussian and a finite-support halo, cut at
   q^2 > 36 exactly."
  [d w]
  (list 'let ['q (list '/ d w) 'q2 '(* q q) 'inside (f/ge '(- 36.0 q2))]
        '(* inside (+ (* 0.88 (exp (- q2))) (* 0.12 (exp (* -0.16 q2)))))))

(defn- cmod
  "Clojure's (mod a s) for doubles, s > 0 a constant."
  [a s]
  (list 'let ['ma a 'mq (list '/ 'ma s) 'mpos (f/ge 'mq)
              'mt '(+ (* mpos (floor mq)) (* (- 1.0 mpos) (- (floor (- mq)))))
              'mr (list '- 'ma (list '* 'mt s))
              'madd (list '* (list '- 1.0 (f/lt 0.0 'ma))
                          (list '- 1.0 (list '* (f/ge 'mr) (f/ge '(- mr)))))]
        (list '+ 'mr (list '* 'madd s))))

(defn- expand
  "Rewrite the pseudo-ops (glow d w) (ge d) (lt a b) (cmod a s) of `form`
   into field vocabulary."
  [form]
  (walk/postwalk
   (fn [e]
     (if (seq? e)
       (case (first e)
         glow (apply glow (rest e))
         ge (f/ge (second e))
         lt (apply f/lt (rest e))
         cmod (apply cmod (rest e))
         e)
       e))
   form))

(def ^:private vj-neon
  "tessera.vj/neon."
  {:gain 0.95 :k 1.65 :sat 0.98 :base 0.02})

(def ^:private uv-neon
  "tessera.vj2/uv."
  {:gain 0.94 :k 1.8 :sat 0.99 :base 0.01})

(defn- neon-bind
  "[bindings [r g b]]: the hue-preserving exposure of channels r g b, with
   its black cutoff (hi < .003 or span < 1e-12) as a mask, bound under
   prefix p."
  [p {:keys [gain k sat base]} r g b]
  (let [s #(symbol (str p "_" %))]
    [[(s "r") r (s "g") g (s "b") b
      (s "hi") (list 'max (list 'max (s "r") (s "g")) (s "b"))
      (s "lo") (list 'min (list 'min (s "r") (s "g")) (s "b"))
      (s "span") (list '- (s "hi") (s "lo"))
      (s "vis") (list '* (f/ge (list '- (s "hi") 0.003)) (f/ge (list '- (s "span") 1.0e-12)))
      (s "v") (list '* gain (list '- 1.0 (list 'exp (list '* (- k) (s "hi")))))
      (s "scale") (list '/ (list '* sat (s "v")) (list 'max (s "span") 1.0e-12))
      (s "base") (list '* base (s "v"))
      (s "or") (list '* (s "vis") (list '+ (s "base") (list '* (s "scale") (list '- (s "r") (s "lo")))))
      (s "og") (list '* (s "vis") (list '+ (s "base") (list '* (s "scale") (list '- (s "g") (s "lo")))))
      (s "ob") (list '* (s "vis") (list '+ (s "base") (list '* (s "scale") (list '- (s "b") (s "lo")))))]
     [(s "or") (s "og") (s "ob")]]))

(defn- level
  "The 8-bit level floor(255 clamp(c) + 1/2) of r/rgb, as a double."
  [c]
  (list 'floor (list '+ (list '* 255.0 (list 'clamp c 0.0 1.0)) 0.5)))

;; ── Textile tunnel ───────────────────────────────────────────────────────

(def textile-tunnel
  (let [[nb [nr ng nbl]]
        (neon-bind "n" vj-neon
                   '(+ (* 0.035 cyan) (* 0.92 magenta) (* 0.35 lime) gold (* 0.05 blue) ember)
                   '(+ (* 0.86 cyan) (* 0.025 magenta) lime (* 0.57 gold) (* 0.15 blue) (* 0.12 ember))
                   '(+ cyan (* 0.95 magenta) (* 0.025 lime) (* 0.015 gold) blue (* 0.025 ember)))]
    {:id :textile_tunnel
     :title "UV textile tunnel: exact cylindrical rays and periodic stepped crosses"
     :period 18 :view [8.0 4.5] :blend :lighten
     :params [['depth_fade {:default 0.047 :min 0.0 :max 0.3 :label "Depth fade (per unit depth squared)"}]
              ['shear {:default 0.12 :min 0.0 :max 0.5 :label "Sinusoidal column shear"}]]
     :form
     {:params '[u depth_fade shear]
      :body
      (expand
       (list 'let
             (-> ['radius (hyp 'x 'y)
                  ;; radius < .23 is black; rs keeps 3/r finite there.
                  'inside '(ge (- radius 0.23))
                  'rs '(max radius 0.23)
                  'ring_pulse '(+ 0.64 (* 0.30 (wave (* 2.0 u))))
                  'theta '(/ (atan2 y x) tau)
                  'z '(/ 3.0 rs) 't '(- z u)
                  's '(+ (* 12.0 theta) (* shear (sin (* tau t))))
                  'a '(- (frac s) 0.5) 'b '(- (frac t) 0.5)
                  'ax '(abs a) 'by '(abs b)
                  'd '(min (min (max (- ax 0.39) (- by 0.12)) (max (- ax 0.27) (- by 0.27)))
                           (max (- ax 0.12) (- by 0.39)))
                  'outer '(glow (- d 0.014) 0.011)
                  'inner '(glow (+ d 0.052) 0.009)
                  'diamond '(glow (- (+ ax by) 0.125) 0.010)
                  'chevron '(glow (- (+ ax by) 0.685) 0.009)
                  'hatch '(* (lt d -0.09) (* 0.48 (glow (- (frac (* 5.0 (+ ax by))) 0.5) 0.045)))
                  'bead_x '(- ax 0.457)
                  'bead_y '(* 0.125 (- (frac (* 8.0 t)) 0.5))
                  'chain '(exp (* -4100.0 (+ (* bead_x bead_x) (* bead_y bead_y))))
                  'zig '(- (frac (+ (* 8.0 by) (* 3.0 ax))) 0.5)
                  'gap '(* (lt 0.045 d) (glow zig 0.05))
                  'eye '(exp (* -2400.0 (+ (* a a) (* b b))))
                  'eye_ring '(glow (- (+ (* 1.6 ax) by) 0.045) 0.008)
                  'alternating '(* 0.5 (- 1.0 (cos (* pi (floor s)))))
                  'pulse '(+ 0.70 (* 0.30 (wave (+ t (* 0.2 (cos (* tau s)))))))
                  'fade '(* pulse (exp (* (- depth_fade) z z)))
                  'cyan '(* fade (+ outer (* 0.44 chain)))
                  'magenta '(* fade (+ inner (* 0.65 chevron)))
                  'lime '(* fade (+ (* (- 1.0 alternating) diamond) (* 0.52 hatch)))
                  'gold '(* fade (+ (* alternating diamond) (* 0.75 chain)))
                  'blue '(* fade 0.28 gap)
                  'ember '(* fade (+ (* 0.95 eye) (* 0.48 eye_ring)))]
                 (into nb)
                 ;; The closure unpacks the packed neon colour before mixing.
                 (into ['cr (f/quantized-channel nr) 'cg (f/quantized-channel ng)
                        'cb (f/quantized-channel nbl)
                        'dash '(pow (wave (* 24.0 theta)) 3.0)
                        'dash2 '(pow (wave (+ (* 36.0 theta) 0.5)) 4.0)
                        'ring '(* ring_pulse (+ (* dash (glow (- rs 0.315) 0.008))
                                                (* 0.72 dash2 (glow (- rs 0.397) 0.006))))]))
             '[(* inside (+ (* (- 1.0 ring) cr) (* ring 0.77)))
               (* inside (+ (* (- 1.0 ring) cg) (* ring 0.86)))
               (* inside (+ (* (- 1.0 ring) cb) (* ring 0.94)))]))}}))

;; ── Flower mandala ───────────────────────────────────────────────────────

(defn- flower-octave
  "Bindings of octave i (k = i - 1 in the closure's loop from -1 to 1),
   accumulating red_i green_i bl_i onto octave i - 1."
  [i k]
  (let [s #(symbol (str % "_" i))
        prev #(if (zero? i) 0.0 (symbol (str % "_" (dec i))))]
    [(s "s") (list 'pow 2.0 (list '- '(- q octave) (double k)))
     (s "px") (list '* (s "s") '(cos angle)) (s "py") (list '* (s "s") '(sin angle))
     (s "diag") (list '/ (list '- (list 'abs (s "py")) (list '* 0.52688172043 (list '- (s "px") 1.0))) 1.130314)
     (s "d") (list 'max (list '- (s "px") 1.93) (s "diag"))
     (s "magenta") (list '+ (list 'glow (list '- (s "d") 0.014) 0.008)
                         (list '* 0.80 (list 'glow (list '+ (s "d") 0.045) 0.007)))
     (s "blue") (list '+ (list 'glow (list '- (s "d") 0.040) 0.006)
                      (list '* 0.68 (list 'glow (list '+ (s "d") 0.070) 0.006)))
     (s "a") (list '+ (list '* 9.0 (s "px")) (list '* 'direction 6.0 (s "py")))
     (s "b") (list '- (list '* 4.0 (s "px")) (list '* 'direction 13.0 (s "py")))
     (s "c") (list '+ (list '* 15.0 (s "px")) (list '* 'direction 3.0 (s "py")))
     (s "field") (list '+ (list 'sin (s "a")) (list '* 0.55 (list 'sin (s "b"))) (list '* 0.28 (list 'cos (s "c"))))
     (s "gx") (list '+ (list '* 9.0 (list 'cos (s "a"))) (list '* 2.20 (list 'cos (s "b"))) (list '* -4.20 (list 'sin (s "c"))))
     (s "gy") (list '* 'direction (list '+ (list '* 6.0 (list 'cos (s "a"))) (list '* -7.15 (list 'cos (s "b")))
                                       (list '* -0.84 (list 'sin (s "c")))))
     (s "contour") (list '/ (list '- (list 'frac (list '* 'worm_density (s "field"))) 0.5)
                         (list 'max 6.0 (list '* 'worm_density (hyp (s "gx") (s "gy")))))
     (s "worms") (list 'glow (s "contour") 0.0038)
     (s "parallel") (list 'glow (list '/ (list '- (list 'frac (list '+ (list '* 14.0 (s "px")) (list '* 'direction 21.0 (s "py")))) 0.5)
                                      25.24) 0.0035)
     (s "patch") (list 'wave (list '+ (list '* 1.8 (s "px")) (list '* 2.1 (s "py"))))
     (s "pm") (list 'lt 'patch_threshold (s "patch"))
     (s "hatch") (list '* (list 'lt (s "d") -0.095)
                       (list '* 0.76 (list '+ (list '* (s "pm") (s "parallel")) (list '* (list '- 1.0 (s "pm")) (s "worms")))))
     (s "gold") (list '* (s "hatch") (list '+ 0.16 (list '* 0.70 (list 'wave (list '+ (list '* 2.0 (s "px")) (s "py"))))))
     (s "lime") (list '- (s "hatch") (s "gold"))
     (s "nested") (list 'glow (list '+ (s "d") 0.14) 0.006)
     (s "red") (list '+ (prev "red") (list '* 0.98 (s "magenta")) (list '* 0.02 (s "blue")) (list '* 0.40 (s "lime"))
                     (s "gold") (list '* 0.10 (s "nested")))
     (s "green") (list '+ (prev "green") (list '* 0.015 (s "magenta")) (list '* 0.12 (s "blue")) (s "lime")
                       (list '* 0.76 (s "gold")) (list '* 0.38 (s "nested")))
     (s "bl") (list '+ (prev "bl") (list '* 0.66 (s "magenta")) (s "blue") (list '* 0.015 (s "hatch"))
                    (list '* 0.75 (s "nested")))]))

(defn- flower-ring
  "[bindings term] of dashed circle i of the eighteen around the hole."
  [i]
  (let [a (* f/tau (/ i 18.0)) s #(symbol (str % "_" i))]
    [[(s "dx") (list '- 'x (list '* 'ring_radius (Math/cos a)))
      (s "dy") (list '- 'y (list '* 'ring_radius (Math/sin a)))
      (s "dist") (hyp (s "dx") (s "dy"))
      (s "dash") (list 'pow (list 'wave (list '* 12.0 (list '/ (list 'atan2 (s "dy") (s "dx")) 'tau))) 2.0)]
     (list '* 'pulse (s "dash") (list 'glow (list '- (s "dist") 0.077) 0.007))]))

(def flower-mandala
  (let [rings (map flower-ring (range 18))
        [cb [cr cg cbl]] (neon-bind "c" uv-neon '(* fade red_2) '(* fade green_2) '(* fade bl_2))
        [rb [rr rg rbl]] (neon-bind "o" uv-neon '(* 0.80 red_stroke)
                                    '(+ (* 0.84 green_stroke) (* 0.72 cyan_stroke)) '(* 0.92 cyan_stroke))]
    {:id :flower_mandala
     :title "Flower mandala: dyadic triangular labyrinth tunnel"
     :period 18 :view [8.0 4.5] :blend :lighten
     :params [['worm_density {:default 2.4 :min 0.5 :max 8.0 :label "Level-set worms per unit field"}]
              ['patch_threshold {:default 0.70 :min 0.0 :max 1.0 :label "Hatch-block patch threshold"}]
              ['ring_radius {:default 0.34 :min 0.2 :max 0.5 :label "Dashed ring chain radius"}]]
     :form
     {:params '[u worm_density patch_threshold ring_radius]
      :body
      (expand
       (list 'let
             (-> ['radius (hyp 'x 'y) 'theta '(atan2 y x)
                  'v '(- u (* 0.72 (/ (sin (* tau u)) tau)))
                  'pulse '(+ 0.80 (* 0.16 (wave (* 2.0 u))))
                  'wedge (list 'floor (list '/ (list '+ 'theta half12) (* 2.0 half12)))
                  'parity '(- wedge (* 2.0 (floor (/ wedge 2.0))))
                  'direction '(- 1.0 (* 2.0 parity))
                  'angle (list '- (list 'cmod (list '+ 'theta half12) (* 2.0 half12)) half12)
                  'q (list '- (list '/ '(log (max 1.0e-12 (/ radius 0.48))) log2) 'v)
                  'octave '(floor q)
                  'ft '(clamp (/ (- radius 0.35) 0.25) 0.0 1.0)
                  'fade '(* (* ft ft (- 3.0 (* 2.0 ft))) (- 1.0 (* 0.035 radius)))]
                 (into (mapcat flower-octave (range 3) [-1 0 1]))
                 (into cb)
                 (into (mapcat first rings))
                 (into ['ring_sum (f/sum-of (map second rings))
                        'ring '(* (lt 0.22 radius) (lt radius 0.47) (min 1.0 ring_sum))
                        'rosette_r '(+ 0.115 (* 0.022 (cos (* 9.0 theta))))
                        'red_stroke '(glow (- radius rosette_r) 0.007)
                        'green_stroke '(glow (- radius (* 0.77 rosette_r)) 0.005)
                        'cyan_stroke '(glow (- radius (* 0.53 rosette_r)) 0.004)])
                 (into rb)
                 ;; blend works on packed levels: background rosette or
                 ;; tunnel, foreground (r/rgb .88 .92 .94) = 224 235 240,
                 ;; each channel truncated by (long ..).
                 (into ['hole '(lt radius 0.18)
                        'k1 (list '+ (list '* 'hole (level rr)) (list '* '(- 1.0 hole) (level cr)))
                        'k2 (list '+ (list '* 'hole (level rg)) (list '* '(- 1.0 hole) (level cg)))
                        'k3 (list '+ (list '* 'hole (level rbl)) (list '* '(- 1.0 hole) (level cbl)))
                        'al '(clamp ring 0.0 1.0) 'be '(- 1.0 al)]))
             '[(/ (floor (+ (* be k1) (* al 224.0))) 255.0)
               (/ (floor (+ (* be k2) (* al 235.0))) 255.0)
               (/ (floor (+ (* be k3) (* al 240.0))) 255.0)]))}}))

;; ── Hyperspace ───────────────────────────────────────────────────────────

(defn- hyperspace-star
  "Bindings of star i of tessera.vj2/hyperspace-seeds, accumulating
   red_i green_i blue_i. The closure's skip |da| > 4 width is a mask."
  [i {:keys [q angle]}]
  (let [s #(symbol (str % "_" i))
        prev #(if (zero? i) 0.0 (symbol (str % "_" (dec i))))
        width (+ 0.0050 (* 0.0030 (lp/frac (* i 0.61803398875))))
        [cr cg cb] (nth [[0.015 0.90 1.0] [0.62 0.015 1.0] [1.0 0.25 0.012]] (mod i 3))]
    [(s "da") (list '- 'angle (double angle))
     (s "on") (list 'ge (list '- (* 4.0 width) (list 'abs (s "da"))))
     (s "dq") (list '- (list 'frac (list '+ (list '- 'q (double q)) 0.5)) 0.5)
     (s "lat") (list '/ (s "da") width)
     (s "across") (list 'exp (list '- (list '* (s "lat") (s "lat"))))
     (s "head") (list '* 1.50 (list 'exp (list '* '(- head_sharpness) (s "dq") (s "dq"))))
     (s "tm") (list '* (list 'ge (list '+ (s "dq") 0.48)) (list 'ge (list '- (s "dq"))))
     (s "tail") (list '* (s "tm") (list '* 0.94 (list 'exp (list '* 'streak_decay (s "dq")))
                                       (list 'min 1.0 (list '* 35.0 (list '+ (s "dq") 0.48)))))
     (s "spark") (list '* 0.40 (list 'exp (list '* -6000.0 (s "dq") (s "dq")))
                       (list 'exp (list '* -0.10 (s "lat") (s "lat"))))
     (s "light") (list '+ (list '* (s "across") (list '+ (s "head") (s "tail"))) (s "spark"))
     (s "red") (list '+ (prev "red") (list '* (s "on") (list '* (s "light") cr)))
     (s "green") (list '+ (prev "green") (list '* (s "on") (list '* (s "light") cg)))
     (s "blue") (list '+ (prev "blue") (list '* (s "on") (list '* (s "light") cb)))]))

(def hyperspace
  (let [seeds (vj2/hyperspace-seeds)
        n (dec (count seeds))
        last-of #(symbol (str % "_" n))
        [nb out] (neon-bind "n" uv-neon
                            (list '+ (list '* 'fade (last-of "red")) '(* 0.45 core) '(* 0.015 icy))
                            (list '+ (list '* 'fade (last-of "green")) '(* 0.025 core) '(* 0.88 icy))
                            (list '+ (list '* 'fade (last-of "blue")) 'core 'icy))]
    {:id :hyperspace
     :title "Hyperspace: D12 folded A2 stars in a dyadic log-radial warp"
     :period 18 :view [8.0 4.5] :blend :lighten
     :params [['streak_decay {:default 4.6 :min 0.5 :max 20.0 :label "Tail decay per octave"}]
              ['head_sharpness {:default 1600.0 :min 100.0 :max 8000.0 :label "Star head sharpness"}]
              ['core_glow {:default 0.15 :min 0.0 :max 0.6 :label "Central core glow"}]]
     :form
     {:params '[u streak_decay head_sharpness core_glow]
      :body
      (expand
       (list 'let
             (-> ['radius (hyp 'x 'y)
                  'v '(- u (* 0.72 (/ (sin (* tau u)) tau)))
                  'angle (list 'abs (list '- (list 'cmod (list '+ '(atan2 y x) half12) (* 2.0 half12)) half12))
                  'q (list '- (list '/ '(log (max 1.0e-12 (/ radius 0.38))) log2) 'v)
                  'ft '(clamp (/ (- radius 0.20) 0.42) 0.0 1.0)
                  'fade '(* ft ft (- 3.0 (* 2.0 ft)) (- 1.0 (* 0.045 radius)))
                  'core '(* core_glow (exp (* -24.0 radius radius)))
                  'icy '(* 0.065 (exp (* -90.0 radius radius)))]
                 (into (mapcat hyperspace-star (range) seeds))
                 (into nb))
             out))}}))

;; ── Sprite lattice ───────────────────────────────────────────────────────

(defn- sprite-cell
  "Bindings of neighbour k of the closure's 3 x 3 loop (di = k rem 3 - 1,
   dj = k quot 3 - 1), accumulating red_k green_k blue_k. The table entry
   at (col, row) = (i+di+12, j+dj+8) is re-derived from sx = col - 12 - v,
   sy = row - 8; the skip (outside .73 or off the 24 x 16 table) is a mask
   selecting the previous sums exactly."
  [k]
  (let [di (double (- (rem k 3) 1)) dj (double (- (quot k 3) 1))
        s #(symbol (str % "_" k))
        prev #(if (zero? k) 0.0 (symbol (str % "_" (dec k))))
        acc (fn [c add] (list '+ (list '* (s "on") (list '+ (prev c) add))
                              (list '* (list '- 1.0 (s "on")) (prev c))))]
    [(s "dx") (list '- 'a di) (s "dy") (list '- 'b dj)
     (s "ci") (list '+ 'i di) (s "cj") (list '+ 'j dj)
     (s "on") (list '* (list 'ge (list '- 0.73 (list 'abs (s "dx")))) (list 'ge (list '- 0.73 (list 'abs (s "dy"))))
                    (list 'ge (list '+ (s "ci") 12.0)) (list 'lt (s "ci") 12.0)
                    (list 'ge (list '+ (s "cj") 8.0)) (list 'lt (s "cj") 8.0))
     (s "sx") (list '- (s "ci") 'v) (s "sy") (s "cj")
     (s "k1") (list '+ (list '* 0.62 (s "sx")) (list '* 0.23 (s "sy")) 'phase)
     (s "k2") (list '- (list '- (list '* 0.62 (s "sx")) (list '* 0.72 (s "sy"))) 'phase)
     (s "re") (list '+ 1.0 (list 'cos (s "k1")) (list 'cos (s "k2")))
     (s "im") (list '+ (list 'sin (s "k1")) (list 'sin (s "k2")))
     (s "energy") (hyp (s "re") (s "im"))
     (s "size") (list '+ 0.06 (list '* 'size_span (list 'pow (list '/ (s "energy") 3.0) 1.25)))
     (s "hue") (list '+ 0.50 (list '* 'hue_swing (list 'sin (list '+ (list '* 0.46 (s "sx")) (list '* 0.20 (s "sy")) 'phase)))
                     (list '* 0.10 (list 'cos (list '- (list '- (list '* 0.30 (s "sx")) (list '* 0.40 (s "sy"))) 'phase))))
     (s "r") (list '+ 0.5 (list '* 0.5 (list 'cos (list '* 'tau (s "hue")))))
     (s "g") (list '+ 0.5 (list '* 0.5 (list 'cos (list '* 'tau (list '+ (s "hue") 0.333333333333)))))
     (s "b") (list '+ 0.5 (list '* 0.5 (list 'cos (list '* 'tau (list '+ (s "hue") 0.666666666667)))))
     (s "aa") (list '/ (s "dx") (s "size")) (s "bb") (list '/ (s "dy") (s "size"))
     (s "ax") (list 'abs (s "aa")) (s "by") (list 'abs (s "bb"))
     ;; kind = (mod row' 3) of an integer, exact; its case as three masks.
     (s "kind") (list '- (s "cj") (list '* 3.0 (list 'floor (list '/ (s "cj") 3.0))))
     (s "m0") (list 'lt (s "kind") 0.5)
     (s "m1") (list '* (list 'ge (list '- (s "kind") 0.5)) (list 'lt (s "kind") 1.5))
     (s "m2") (list 'ge (list '- (s "kind") 1.5))
     (s "cross") (list 'min (list 'min (list 'max (list '- (s "ax") 0.39) (list '- (s "by") 0.12))
                                  (list 'max (list '- (s "ax") 0.27) (list '- (s "by") 0.27)))
                       (list 'max (list '- (s "ax") 0.12) (list '- (s "by") 0.39)))
     (s "diamond") (list '* 0.707106781187 (list '- (list '+ (s "ax") (s "by")) 0.43))
     (s "arrow") (list 'max (list '- (s "ax") 0.39) (list '- (list 'abs (list '- (s "by") (list '* 0.68 (s "ax")))) 0.12))
     (s "raw") (list '+ (list '* (s "m0") (s "cross")) (list '* (s "m1") (s "diamond")) (list '* (s "m2") (s "arrow")))
     (s "d") (list '* (s "size") (s "raw"))
     (s "outline") (list 'glow (list '- (s "d") 0.009) 0.017)
     (s "inline") (list 'glow (list '+ (s "d") 0.045) 0.012)
     (s "fill") (list '* 0.20 (list 'pow (list '/ (s "size") 1.66) 0.7)
                      (list 'max 0.0 (list 'min 1.0 (list '/ (list '- (s "d")) 0.035))))
     (s "nested") (list 'glow (list '* (s "size") (list '- (list '+ (s "ax") (s "by")) 0.125)) 0.012)
     (s "hatch") (list '* (list 'lt (s "raw") -0.08)
                       (list '* 0.22 (list 'glow (list '- (list 'frac (list '* 4.0 (list '+ (s "ax") (s "by")))) 0.5) 0.080)))
     (s "eye") (list 'exp (list '* -1300.0 (list '+ (list '* (s "aa") (s "aa")) (list '* (s "bb") (s "bb")))))
     (s "mask") (list '- 1.0 (list '* 0.95 (s "eye")))
     (s "main") (list '* (s "mask") (list '+ (list '* 1.30 (s "outline")) (s "fill")))
     (s "inner") (list '* (s "mask") 0.72 (s "inline"))
     (s "lime") (list '* (s "mask") 0.43 (list '+ (s "nested") (s "hatch")))
     (s "red") (acc "red" (list '+ (list '* (s "r") (s "main")) (list '* (s "b") (s "inner"))
                                (list '* 0.38 (s "lime")) (list '* 0.95 (s "eye"))))
     (s "green") (acc "green" (list '+ (list '* (s "g") (s "main")) (list '* (s "r") (s "inner"))
                                    (s "lime") (list '* 0.095 (s "eye"))))
     (s "blue") (acc "blue" (list '+ (list '* (s "b") (s "main")) (list '* (s "g") (s "inner"))
                                  (list '* 0.02 (s "lime")) (list '* 0.014 (s "eye"))))]))

(def sprite-lattice
  (let [[nb out] (neon-bind "n" vj-neon 'red_8 'green_8 'blue_8)]
    {:id :sprite_lattice
     :title "UV sprite lattice: glyph sizes from the exact graphene hopping band"
     :period 18 :view [8.0 4.5] :blend :lighten
     :params [['size_span {:default 1.60 :min 0.2 :max 1.66 :label "Glyph size span (cells)"}]
              ['hue_swing {:default 0.33 :min 0.0 :max 0.5 :label "Hue band swing"}]]
     :form
     {:params '[u size_span hue_swing]
      :body
      (expand
       (list 'let
             (-> ['v '(- u (* 0.60 (/ (sin (* tau u)) tau)))
                  'phase '(* tau v)
                  'gx '(+ (/ x 0.50) v) 'gy '(/ y 0.50)
                  'i '(floor gx) 'j '(floor gy)
                  'a '(- (frac gx) 0.5) 'b '(- (frac gy) 0.5)]
                 (into (mapcat sprite-cell (range 9)))
                 (into nb))
             out))}}))
