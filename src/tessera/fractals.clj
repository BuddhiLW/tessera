(ns tessera.fractals
  "Fractal loops: 3D sets sphere traced from distance estimators
   (tessera.raymarch), 2D sets by escape time and by inversion. Every time
   term is a function of 2 pi u and every iteration count is fixed, so a
   frame is a pure function of the phase and the loop closes exactly."
  (:require [desargues.wallpaper.loop :as lp]
            [tessera.palette :as pal]
            [tessera.raymarch :as rm]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def view [(/ 128.0 9.0) 8.0])

(defn- rot-y
  "[x y z] turned by angle a about +y (x toward z)."
  [[x y z] a]
  (let [c (Math/cos a) s (Math/sin a) x (double x) z (double z)]
    [(- (* x c) (* z s)) (double y) (+ (* x s) (* z c))]))

(defn surface-light
  "Surface shading shared by the 3D fractals: palette colour `c` lit by
   `light` [lx ly lz key rim-r rim-g rim-b] (unit key direction, its
   strength, a rim tint), with a sky fill, occlusion and a soft specular."
  ^long [^doubles h ^long c ^doubles light]
  (let [lx (aget light 0) ly (aget light 1) lz (aget light 2) key (aget light 3)
        rr (aget light 4) rg (aget light 5) rb (aget light 6)
        nx (aget h rm/NX) ny (aget h rm/NY) nz (aget h rm/NZ)
        ao (aget h rm/AO)
        dif (max 0.0 (+ (* nx lx) (* ny ly) (* nz lz)))
        vx (- (aget h rm/DX)) vy (- (aget h rm/DY)) vz (- (aget h rm/DZ))
        hx (+ lx vx) hy (+ ly vy) hz (+ lz vz)
        hn (/ 1.0 (Math/sqrt (+ (* hx hx) (* hy hy) (* hz hz) 1.0e-12)))
        spec (Math/pow (max 0.0 (* hn (+ (* nx hx) (* ny hy) (* nz hz)))) 28.0)
        rim (Math/pow (- 1.0 (max 0.0 (+ (* nx vx) (* ny vy) (* nz vz)))) 3.0)
        sky (* 0.5 (+ 1.0 ny))
        k (+ (* key dif (+ 0.3 (* 0.7 ao))) (* 0.30 ao sky))
        s (* 0.45 spec ao)]
    (rm/pack (+ (* k (rm/linear c 16)) s (* rr rim ao))
             (+ (* k (rm/linear c 8)) s (* rg rim ao))
             (+ (* k (rm/linear c 0)) s (* rb rim ao)))))

;; ── Mandelbulb ────────────────────────────────────────────────────────────

(def ^:const bulb-iter 5)

(defn bulb-de
  "Distance estimate to the power-8 Mandelbulb (Inigo Quilez's trig-free
   triplex form, y the polar axis), `bulb-iter` iterations. The map
   commutes with a turn of 2 pi/7 about y: z^8 turns by 8 sevenths of a
   turn, which is one seventh, so the set has seven-fold symmetry."
  ^double [^double px ^double py ^double pz]
  (loop [i 0 x px y py z pz dz 1.0 m (+ (* px px) (* py py) (* pz pz))]
    (if (or (>= i bulb-iter) (> m 256.0))
      (* 0.25 (Math/log m) (/ (Math/sqrt m) dz))
      (let [r (Math/sqrt m)
            dz (+ (* 8.0 m m m r dz) 1.0)
            x2 (* x x) x4 (* x2 x2) y2 (* y y) y4 (* y2 y2) z2 (* z z) z4 (* z2 z2)
            k3 (+ x2 z2)
            k2 (/ 1.0 (* k3 k3 k3 (Math/sqrt k3)))
            k1 (+ x4 y4 z4 (* -6.0 y2 z2) (* -6.0 x2 y2) (* 2.0 z2 x2))
            k4 (+ (- x2 y2) z2)
            nx (+ px (* 64.0 x y z (- x2 z2) k4 (+ (- x4 (* 6.0 x2 z2)) z4) k1 k2))
            ny (+ py (* -16.0 y2 k3 k4 k4) (* k1 k1))
            nz (+ pz (* -8.0 y k4 (+ (* x4 x4) (* -28.0 x4 x2 z2) (* 70.0 x4 z4)
                                     (* -28.0 x2 z2 z4) (* z4 z4)) k1 k2))]
        (recur (inc i) nx ny nz dz (+ (* nx nx) (* ny ny) (* nz nz)))))))

(defn bulb-trap
  "Orbit trap of a surface point, written into `out`: the least |z|^2 over
   the iterations (0 deep in a crease, about 1 on the outer shell)."
  [^double px ^double py ^double pz ^doubles out]
  (loop [i 0 x px y py z pz tm 1.0e9]
    (let [m (+ (* x x) (* y y) (* z z))
          tm (min tm m)]
      (if (or (>= i bulb-iter) (> m 256.0) (< m 1.0e-12))
        (do (aset out 0 tm) out)
        (let [x2 (* x x) x4 (* x2 x2) y2 (* y y) y4 (* y2 y2) z2 (* z z) z4 (* z2 z2)
              k3 (+ x2 z2)
              k2 (/ 1.0 (* k3 k3 k3 (Math/sqrt k3)))
              k1 (+ x4 y4 z4 (* -6.0 y2 z2) (* -6.0 x2 y2) (* 2.0 z2 x2))
              k4 (+ (- x2 y2) z2)]
          (recur (inc i)
                 (+ px (* 64.0 x y z (- x2 z2) k4 (+ (- x4 (* 6.0 x2 z2)) z4) k1 k2))
                 (+ py (* -16.0 y2 k3 k4 k4) (* k1 k1))
                 (+ pz (* -8.0 y k4 (+ (* x4 x4) (* -28.0 x4 x2 z2) (* 70.0 x4 z4)
                                       (* -28.0 x2 z2 z4) (* z4 z4)) k1 k2))
                 tm))))))

(def bulb-palette
  "Deep violet in the creases, through rose to ember gold on the shell."
  (pal/cosine [0.55 0.38 0.40] [0.45 0.35 0.38] [0.9 0.9 0.8] [0.02 0.18 0.36]))

(defn bulb-shader
  "The bulb at phase u, turned a seventh of a turn per loop about its axis
   (its own symmetry: the turned bulb at u = 1 is the bulb at u = 0) under a
   fixed camera, the key light once around. The bulb turns, not the camera,
   so the normal and occlusion taps, fixed in the camera's frame, see the
   same field at u = 0 and u = 1."
  [u]
  (let [u (double u)
        th (- (+ 0.35 (* (/ lp/tau 7.0) u)))
        c (Math/cos th) s (Math/sin th)
        de (fn ^double [^double x ^double y ^double z]
             (bulb-de (- (* x c) (* z s)) y (+ (* x s) (* z c))))
        cam (rm/camera [0.0 1.3 3.25] [0.0 -0.08 0.0] 34.0 view)
        la (* lp/tau u)
        [lx ly lz] (rm/unit [(* 1.4 (Math/cos la)) 1.6 (+ 1.2 (* 1.4 (Math/sin la)))])
        light (double-array [lx ly lz 2.6 0.35 0.10 0.55])
        trap (ThreadLocal/withInitial (reify java.util.function.Supplier (get [_] (double-array 1))))]
    (rm/shader
     {:de de :cam cam :bound 1.2 :eps 0.0012 :steps 110 :ao 0.08
      :shade (fn ^long [^doubles h]
               (if (neg? (aget h rm/T))
                 (let [g (Math/exp (* -55.0 (aget h rm/HALO)))]
                   (rm/pack (+ 0.004 (* 0.30 g)) (+ 0.006 (* 0.07 g)) (+ 0.030 (* 0.24 g))))
                 (let [^doubles tr (.get ^ThreadLocal trap)
                       x (aget h rm/PX) z (aget h rm/PZ)
                       _ (bulb-trap (- (* x c) (* z s)) (aget h rm/PY) (+ (* x s) (* z c)) tr)
                       col (pal/at bulb-palette (+ 0.08 (* 0.8 (Math/sqrt (min 1.0 (aget tr 0))))))]
                   (surface-light h col light))))})))

(def mandelbulb
  {:id :mandelbulb
   :title "Mandelbulb, power 8: a seventh of a turn, the bulb's own symmetry, the light once around"
   :seconds 14 :fps 30 :size [1920 1080] :view view :ss 2
   :field bulb-shader
   :post (fn [_] [{:type :bloom :radius 14 :strength 0.55}
                  {:type :vignette :strength 0.4}])})

;; ── Quaternion Julia ─────────────────────────────────────────────────────

(def ^:const qj-iter 7)

(defn qj-de
  "Distance estimator of the quaternion Julia set of c = [ca cb cc cd] in
   the slice w = 0: z <- z^2 + c, |dz| <- 2|z||dz|, d = |z| log|z| / 2|dz|."
  [[ca cb cc cd]]
  (let [ca (double ca) cb (double cb) cc (double cc) cd (double cd)]
    (fn ^double [^double px ^double py ^double pz]
      (loop [i 0 a px b py c pz d 0.0 md 1.0]
        (let [m (+ (* a a) (* b b) (* c c) (* d d))]
          (if (or (>= i qj-iter) (> m 16.0))
            (let [r (Math/sqrt m)] (/ (* 0.5 r (Math/log (max r 1.0e-9))) md))
            (recur (inc i)
                   (+ (- (* a a) (* b b) (* c c) (* d d)) ca)
                   (+ (* 2.0 a b) cb) (+ (* 2.0 a c) cc) (+ (* 2.0 a d) cd)
                   (* 2.0 (Math/sqrt m) md))))))))

(defn qj-trap
  "The orbit trap of c: p -> least |z|^2 over its orbit (deep folds near 0)."
  [[ca cb cc cd]]
  (let [ca (double ca) cb (double cb) cc (double cc) cd (double cd)]
    (fn ^double [^double px ^double py ^double pz]
      (loop [i 0 a px b py c pz d 0.0 tm 1.0e9]
        (let [m (+ (* a a) (* b b) (* c c) (* d d)) tm (min tm m)]
          (if (or (>= i qj-iter) (> m 16.0))
            tm
            (recur (inc i)
                   (+ (- (* a a) (* b b) (* c c) (* d d)) ca)
                   (+ (* 2.0 a b) cb) (+ (* 2.0 a c) cc) (+ (* 2.0 a d) cd) tm)))))))

(defn qj-c
  "c on a closed ellipse about (-0.2, 0.6, 0.2, 0.2), once around per loop."
  [u]
  (let [a (* lp/tau (double u))]
    [(+ -0.2 (* 0.11 (Math/cos a))) (+ 0.6 (* 0.07 (Math/sin a)))
     (+ 0.2 (* 0.06 (Math/sin a))) (+ 0.2 (* 0.05 (Math/cos a)))]))

(def qj-palette
  "Ice: deep teal folds, cyan skin, a violet edge."
  (pal/cosine [0.42 0.52 0.62] [0.38 0.36 0.32] [0.8 0.8 0.8] [0.55 0.42 0.30]))

(defn qj-shader
  "The quaternion Julia set of c(u), camera fixed, key light once around."
  [u]
  (let [c (qj-c u)
        de (qj-de c)
        ^clojure.lang.IFn$DDDD trap (qj-trap c)
        cam (rm/camera [1.9 1.05 2.9] [-0.05 0.0 0.0] 38.0 view)
        la (* lp/tau (double u))
        [lx ly lz] (rm/unit [(* 1.5 (Math/cos la)) 1.4 (+ 1.6 (* 1.5 (Math/sin la)))])
        light (double-array [lx ly lz 2.4 0.25 0.12 0.45])]
    (rm/shader
     {:de de :cam cam :bound 1.5 :eps 0.0016 :steps 80 :ao 0.10
      :shade (fn ^long [^doubles h]
               (if (neg? (aget h rm/T))
                 (let [g (Math/exp (* -45.0 (aget h rm/HALO)))]
                   (rm/pack (+ 0.004 (* 0.05 g)) (+ 0.010 (* 0.20 g)) (+ 0.028 (* 0.30 g))))
                 (let [t (.invokePrim trap (aget h rm/PX) (aget h rm/PY) (aget h rm/PZ))
                       col (pal/at qj-palette (+ 0.05 (* 0.75 (Math/sqrt (min 1.0 t)))))]
                   (surface-light h col light))))})))

(def quaternion-julia
  {:id :quaternion-julia
   :title "Quaternion Julia set, c once around a closed loop in (1, i, j, k)"
   :seconds 16 :fps 30 :size [1920 1080] :view view :ss 2
   :field qj-shader
   :post (fn [_] [{:type :bloom :radius 14 :strength 0.6}
                  {:type :vignette :strength 0.4}])})

;; ── Menger sponge flight ─────────────────────────────────────────────────

(def ^:const menger-levels 4)

(defn- wrap2
  "v mod 2, shifted to [-1, 1)."
  ^double [^double v]
  (- (- v (* 2.0 (Math/floor (/ v 2.0)))) 1.0))

(defn menger-de
  "Distance estimate to the infinite Menger sponge of period 2: solid
   2-cubes from which each level carves the galleries of half-width 1/3 of
   its cell, `menger-levels` levels, each three times finer. The level-0
   galleries run along the lines where two coordinates are 0 mod 2."
  ^double [^double px ^double py ^double pz]
  (loop [i 0 s 1.0 d -1.0e9]
    (if (>= i menger-levels)
      d
      (let [rx (Math/abs (- 1.0 (* 3.0 (Math/abs (wrap2 (* px s))))))
            ry (Math/abs (- 1.0 (* 3.0 (Math/abs (wrap2 (* py s))))))
            rz (Math/abs (- 1.0 (* 3.0 (Math/abs (wrap2 (* pz s))))))
            c (/ (- (min (max rx ry) (min (max ry rz) (max rz rx))) 1.0) (* 3.0 s))]
        (recur (inc i) (* 3.0 s) (max d c))))))

(defn menger-eye
  "The camera at phase u: down the gallery x = y = 0, one period (2) of the
   sponge per loop, weaving on whole harmonics."
  [u]
  (let [a (* lp/tau (double u))]
    [(* 0.09 (Math/cos a)) (* 0.07 (Math/cos (+ (* 2.0 a) 0.6))) (- 1.0 (* 2.0 (double u)))]))

(def menger-palette
  "Bronze and copper walls, cooling to teal in the distance."
  (pal/cosine [0.52 0.40 0.30] [0.42 0.32 0.24] [1.0 1.0 1.0] [0.02 0.10 0.22]))

(defn menger-shader
  "The flight at phase u: a lamp rides with the camera, fog falls to deep
   blue, and a warm light burns at the far end of the gallery. The sponge
   has period 2 along z, so the eye one period further on sees the same
   field: frame N is frame 0."
  [u]
  (let [a (* lp/tau (double u))
        eye (menger-eye u)
        look [(* 0.22 (Math/cos (+ a 0.5))) (* 0.14 (Math/cos (- (* 2.0 a) 0.4))) -1.0]
        cam (rm/camera eye (mapv + eye look) 64.0 view [(* 0.12 (Math/cos a)) 1.0 0.0])
        [ex ey ez] eye ex (double ex) ey (double ey) ez (double ez)]
    (rm/shader
     {:de menger-de :cam cam :tmax 16.0 :eps 0.0018 :steps 90 :ao 0.06
      :shade (fn ^long [^doubles h]
               (let [t (aget h rm/T)
                     beam (Math/pow (max 0.0 (- (aget h rm/DZ))) 300.0)]
                 (if (neg? t)
                   (rm/pack (+ 0.005 (* 1.8 beam)) (+ 0.008 (* 1.1 beam)) (+ 0.02 (* 0.45 beam)))
                   (let [px (aget h rm/PX) py (aget h rm/PY) pz (aget h rm/PZ)
                         nx (aget h rm/NX) ny (aget h rm/NY) nz (aget h rm/NZ)
                         ao (aget h rm/AO)
                         lx (- ex px) ly (- (+ ey 0.06) py) lz (- ez pz)
                         l2 (+ (* lx lx) (* ly ly) (* lz lz))
                         il (/ 1.0 (Math/sqrt l2))
                         dif (max 0.0 (* il (+ (* nx lx) (* ny ly) (* nz lz))))
                         head (/ (* 2.2 dif) (+ 0.15 (* 1.2 l2)))
                         far (* 1.0 (max 0.0 nz) (Math/exp (* -0.15 t)))
                         fog (- 1.0 (Math/exp (* -0.30 t)))
                         c (pal/at menger-palette (+ 0.10 (* 0.08 (Math/abs nx)) (* 0.5 fog)))
                         k (* (+ head far) (* ao ao))
                         mixf (fn ^double [^double v ^double fogc]
                                (+ (* (- 1.0 fog) k v) (* fog fogc)))]
                     (rm/pack (mixf (rm/linear c 16) 0.004)
                              (mixf (rm/linear c 8) 0.012)
                              (mixf (rm/linear c 0) 0.035))))))})))

(def menger-flight
  {:id :menger-flight
   :title "Flight down a gallery of the infinite Menger sponge, one period per loop"
   :seconds 12 :fps 30 :size [1920 1080] :view view :ss 2
   :field menger-shader
   :post (fn [_] [{:type :bloom :radius 16 :strength 0.7}
                  {:type :vignette :strength 0.5}])})

;; ── Julia sets along the main cardioid ───────────────────────────────────

(def ^:const cj-iter 160)
(def ^:const cj-r 0.93)

(defn cardioid-c
  "c(u) = mu/2 - mu^2/4 with mu = 0.93 e^{2 pi i u}: the point of the main
   cardioid of the Mandelbrot set whose attracting fixed point mu/2 has
   multiplier mu. As u goes once around, c traces a curve just inside the
   cardioid and the Julia set passes every rotation number in turn."
  [u]
  (let [a (* lp/tau (double u))
        mr (* cj-r (Math/cos a)) mi (* cj-r (Math/sin a))]
    [(- (* 0.5 mr) (* 0.25 (- (* mr mr) (* mi mi))))
     (- (* 0.5 mi) (* 0.5 mr mi))]))

(def cj-palette
  "Midnight blue, violet, rose and gold, integer frequencies (cyclic)."
  (pal/cosine [0.45 0.32 0.42] [0.45 0.32 0.36] [1.0 1.0 1.0] [0.0 0.08 0.24]))

(defn cardioid-shader
  "The filled Julia set of c(u), turned a quarter so its long axis lies
   along the frame. Escaping points: smooth escape time through the cyclic
   palette, shifted by u, and a thin glow from the distance estimate
   |z| log|z| / |z'|. Points caught by the attracting fixed point mu/2 are
   the Fatou interior, a dim tint by how long the capture took."
  [u]
  (let [[cr ci] (cardioid-c u)
        cr (double cr) ci (double ci)
        a (* lp/tau (double u))
        fr (* 0.5 cj-r (Math/cos a)) fi (* 0.5 cj-r (Math/sin a))
        shift (double u)
        ln2 (Math/log 2.0)]
    (fn ^long [^double x ^double y]
      (loop [zr (- y) zi x i 0 dz 1.0]
        (let [m (+ (* zr zr) (* zi zi))
              er (- zr fr) ei (- zi fi)]
          (cond
            (> m 256.0)
            (let [mu (- (+ i 1) (/ (Math/log (/ (Math/log m) 2.0)) ln2))
                  r (Math/sqrt m)
                  de (/ (* r (Math/log r)) dz)
                  edge (Math/exp (* -260.0 de))
                  c (pal/at cj-palette (+ (* 0.045 mu) shift))
                  k (+ 0.25 (* 0.75 (- 1.0 (Math/exp (* -0.06 mu)))))
                  ch (fn ^double [^long s] (* 1.4 (+ (* k (rm/channel c s)) (* 0.55 edge))))]
              (rm/pack (ch 16) (ch 8) (ch 0)))
            (or (< (+ (* er er) (* ei ei)) 2.5e-3) (>= i cj-iter))
            (let [c (pal/at cj-palette (+ 0.55 (* 0.012 i) shift))
                  k 0.10]
              (rm/pack (* k (rm/channel c 16)) (* k (rm/channel c 8)) (* k (rm/channel c 0))))
            :else (recur (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 zr zi) ci) (inc i)
                         (* 2.0 (Math/sqrt m) dz))))))))

(def cardioid-julia
  {:id :cardioid-julia
   :title "Julia sets along the main cardioid: c = mu/2 - mu^2/4, mu = 0.93 e^{2 pi i u}"
   :seconds 16 :fps 30 :size [1920 1080] :view [3.2 1.8] :ss 2
   :field cardioid-shader
   :post (fn [_] [{:type :bloom :radius 10 :strength 0.6}
                  {:type :vignette :strength 0.45}])})

(def loops
  "The fractal loops, by id."
  (array-map :mandelbulb mandelbulb :quaternion-julia quaternion-julia
             :menger-flight menger-flight :cardioid-julia cardioid-julia))
