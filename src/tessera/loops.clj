(ns tessera.loops
  "The wallpaper loops: specs of desargues.wallpaper.render, every motion
   periodic in the phase u.

     :girih-light     khatam stars on 4.8.8, the contact angle breathing,
                      a band of light sweeping the ceiling
     :girih-drift     six-fold two-point stars on 3.6.3.6, drifting one
                      lattice period per loop
     :girih-assembly  the 4.8.8 ceiling comes apart in a radial wave and
                      settles back, each tile turning as it goes
     :quasicrystal    seven plane waves summed, their phases once around
     :julia-orbit     the Julia set of c = 0.7885 e^{2πiu}
     :sierpinski-zoom zoom by 2 into a corner of Sierpinski's triangle"
  (:require [desargues.wallpaper.girih :as g]
            [desargues.wallpaper.loop :as lp]
            [tessera.ornament :as orn]
            [tessera.palette :as pal]
            [desargues.wallpaper.luxcore :as lx]
            [tessera.neon :as neon]
            [tessera.curves :as curves]
            [tessera.reciprocal :as recip]
            [tessera.quantum :as quantum]))

(set! *warn-on-reflection* true)

(def view
  "Manim's frame, 128/9 x 8 scene units."
  [(/ 128.0 9.0) 8.0])

(def girih-light
  (let [scale 1.15]
    {:id :girih-light
     :title "Khatam stars, breathing, under a passing light"
     :seconds 16 :fps 30 :size [1920 1080] :view view
     :frame (fn [u]
              (let [theta (+ (g/khatam-angle) (* 0.16 (Math/sin (* lp/tau u))))]
                {:background (:ground pal/girih)
                 :shapes (orn/girih-shapes (g/pattern :4.8.8 (orn/rect-of view scale [0 0]) theta 0.0)
                                           scale {})}))
     :light (fn [u] (orn/sweep-light u {}))}))

(def girih-drift
  (let [scale 0.95
        [a _] (:lattice (g/tiling :3.6.3.6))]
    {:id :girih-drift
     :title "Six-fold two-point stars, drifting one lattice period"
     :seconds 20 :fps 30 :size [1920 1080] :view view
     :frame (fn [u]
              (let [shift (g/v* (double u) a)
                    back (g/v* (- scale) shift)]
                {:background (:ground pal/girih)
                 :shapes (mapv (fn [s] (update s :points (partial mapv #(g/v+ % back))))
                               (orn/girih-shapes (g/pattern :3.6.3.6 (orn/rect-of view scale shift) 1.15 0.12)
                                                 scale {:band 11 :line 5}))}))
     :light (fn [u] (orn/sweep-light u {:angle -0.9 :lambda 11.0 :floor 0.8 :peak 0.35}))}))

(def girih-assembly
  (let [scale 1.15
        rect (orn/rect-of view scale [0 0])
        rmax (g/vlen [(nth rect 1) (nth rect 3)])]
    {:id :girih-assembly
     :title "Assembly and disassembly: a radial wave takes the ceiling apart"
     :seconds 14 :fps 30 :size [1920 1080] :view view
     :frame (fn [u]
              (let [xf (fn [{:keys [centre cell]}]
                         (let [r (/ (g/vlen centre) rmax)
                               start (+ 0.04 (* 0.40 r))
                               w (lp/window u start (+ start 0.52))
                               dir (g/unit (if (zero? (g/vlen centre)) [0.0 1.0] centre))
                               sign (if (even? (reduce + cell)) 1.0 -1.0)]
                           {:offset (g/v* (* 1.6 w) dir)
                            :turn (* sign w (/ Math/PI 2.0))
                            :opacity (- 1.0 (* 0.9 w))}))]
                {:background pal/ink
                 :shapes (orn/girih-shapes (g/pattern :4.8.8 rect (g/khatam-angle) 0.0) scale {:xf xf})}))
     :light (fn [u] (orn/sweep-light u {:angle 1.2 :floor 0.85 :peak 0.3}))}))

(def quasicrystal
  {:id :quasicrystal
   :title "Seven waves: a quasicrystal breathing"
   :seconds 12 :fps 30 :size [1920 1080] :view view
   :field (fn [u]
            (let [n 7
                  k (/ lp/tau 0.9)
                  dirs (double-array (mapcat (fn [j] [(Math/cos (/ (* Math/PI j) n))
                                                      (Math/sin (/ (* Math/PI j) n))])
                                             (range n)))
                  ph (* lp/tau (double u))]
              (fn ^long [^double x ^double y]
                (loop [j 0 acc 0.0]
                  (if (< j n)
                    (recur (inc j)
                           (+ acc (Math/cos (+ ph (* k (+ (* x (aget dirs (* 2 j)))
                                                          (* y (aget dirs (inc (* 2 j))))))))))
                    (let [s (+ 0.5 (* 0.5 (Math/tanh (/ acc 1.3))))]
                      (pal/at pal/lapis-gold (+ 0.1 (* 0.85 s)))))))))})

(def julia-orbit
  {:id :julia-orbit
   :title "The Julia set of c = 0.7885 e^{2πiu}"
   :seconds 24 :fps 30 :size [1920 1080] :view [3.6 2.025] :ss 2
   :field (fn [u]
            (let [[cr ci] (lp/circle u 0.7885)
                  cr (double cr) ci (double ci)
                  max-iter 300
                  shift (double u)
                  ln2 (Math/log 2.0)]
              (fn ^long [^double x ^double y]
                (loop [zr x zi y i 0]
                  (let [m (+ (* zr zr) (* zi zi))]
                    (cond
                      (> m 256.0)
                      (let [mu (- (+ i 1) (/ (Math/log (/ (Math/log m) 2.0)) ln2))]
                        (pal/at pal/ember (+ (* 0.035 mu) shift)))
                      (>= i max-iter) 0x050812
                      :else (recur (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 zr zi) ci) (inc i))))))))})

(def sierpinski-zoom
  {:id :sierpinski-zoom
   :title "Sierpinski's triangle, zoomed by its own ratio"
   :seconds 10 :fps 30 :size [1920 1080] :view [1.6 0.9] :ss 2
   :field (fn [u]
            (let [u (double u)
                  k (* 2.6 (Math/pow 2.0 u))
                  s3 (Math/sqrt 3.0)]
              (fn ^long [^double x ^double y]
                ;; triangle (0,0) (1,0) (1/2,√3/2); p = (s + c) / (2.6·2^u), so p(1) = p(0)/2
                (let [px (/ (+ x 0.9) k) py (/ (+ y 0.5) k)
                      b (/ (* 2.0 py) s3)
                      a (- px (* 0.5 b))]
                  (if (or (neg? a) (neg? b) (> (+ a b) 1.0))
                    0x060a14
                    (loop [a a b b d 0]
                      (cond
                        (>= d 26) (pal/at pal/lapis-gold (* 0.18 (- d u)))
                        (and (< a 0.5) (< b 0.5) (> (+ a b) 0.5))
                        (pal/at pal/lapis-gold (* 0.18 (- d u)))
                        :else (let [a2 (* 2.0 a) b2 (* 2.0 b)]
                                (cond (>= a2 1.0) (recur (- a2 1.0) b2 (inc d))
                                      (>= b2 1.0) (recur a2 (- b2 1.0) (inc d))
                                      :else (recur a2 b2 (inc d)))))))))))})

(defn- quad [[x y] half z]
  {:vertices [[(- x half) (- y half) z] [(+ x half) (- y half) z]
              [(+ x half) (+ y half) z] [(- x half) (+ y half) z]]
   :faces [[0 2 1] [0 3 2]]})

(defn girih-gold-scene
  "The 4.8.8 khatam ceiling in 3D at phase `u`: gold strands over glazed
   lapis stars and terracotta squares, an area lamp orbiting once per loop."
  [u]
  (let [polys (g/pattern :4.8.8 [-7.0 7.0 -4.0 7.5] (g/khatam-angle) 0.0)
        rosettes (fn [kind] (lx/merge-meshes (for [p polys :when (= kind (:kind p))]
                                               (lx/prism (:rosette p) 0.0 0.02))))]
    {:camera {:orig [0 -4.2 7.5] :target [0 0.3 0] :fov 52}
     :materials {:gold {:type "metal2" :preset "gold" :roughness 0.12}
                 :lapis {:type "glossy2" :kd [0.03 0.12 0.28] :ks [0.08 0.08 0.08] :uroughness 0.05 :vroughness 0.05}
                 :terra {:type "glossy2" :kd [0.30 0.08 0.04] :ks [0.06 0.06 0.06] :uroughness 0.08 :vroughness 0.08}
                 :ground {:type "matte" :kd [0.02 0.03 0.06]}
                 :lamp {:type "matte" :kd [0 0 0] :emission [45 39 30]}}
     :lights {:amb {:type "constantinfinite" :color [0.05 0.07 0.12]}}
     :objects {:ground (merge {:material :ground} (quad [0 0] 12.0 -0.01))
               :octagons (merge {:material :lapis} (rosettes :octagon))
               :squares (merge {:material :terra} (rosettes :square))
               :strands (merge {:material :gold} (lx/ribbons (mapcat :strands polys) 0.07 0.0 0.06))
               :lamp (merge {:material :lamp :camerainvisible true} (quad (lp/circle u 3.0) 0.6 3.5))}
     :spp 48}))

(def girih-gold
  {:id :girih-gold
   :title "Gold girih under an orbiting lamp (LuxCore path tracing)"
   :seconds 8 :fps 15 :size [640 360]
   :scene girih-gold-scene})

(def all
  "Every loop, by id, in gallery order."
  (into (array-map)
        (map (juxt :id identity))
        [neon/neon-pulse neon/neon-khatam
         recip/graphene-fermi recip/kagome-fermi recip/twisted-bilayer
         recip/brillouin-zones recip/brillouin-square recip/brillouin-oblique recip/brillouin-rect
         quantum/quantum-carpet quantum/double-slit
         curves/moore-comets girih-light girih-drift girih-assembly
         quasicrystal julia-orbit sierpinski-zoom]))

(def path-traced
  "Loops rendered by LuxCore (luxcoreconsole on PATH), by id."
  (array-map :girih-gold girih-gold))

(def catalog
  "Every loop the command line knows: `all`, then `path-traced`."
  (merge all path-traced))
