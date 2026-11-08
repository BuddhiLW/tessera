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

(def loops
  "The fractal loops, by id."
  (array-map :mandelbulb mandelbulb))
