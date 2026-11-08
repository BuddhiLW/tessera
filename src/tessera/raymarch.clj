(ns tessera.raymarch
  "Sphere tracing of distance estimators: the renderer of the 3D fractal loops.

   A distance estimator `de` is a primitive fn (fn ^double [^double x ^double y
   ^double z] d): a lower bound on the distance from the point to the surface.
   `shader` turns it into a field shader of the render layer: each pixel's
   ray of a pinhole camera is marched with a cone-shaped hit tolerance (eps·t,
   about a pixel at every depth), and the hit, or the miss, goes to a shading
   fn as a hit record. Every count is fixed (march steps, normal taps,
   occlusion taps), so a pixel's cost is bounded and a frame is a pure
   function of the loop phase.

   The hit record is a double array, indexed by the constants below:
   t (negative on a miss), the point p, the unit normal n, ambient occlusion
   (1 open), the fraction of the step budget used, the halo (the least d/t the
   ray met, 0 on a hit: small where a missing ray grazed the surface) and the
   unit ray direction."
  (:import (clojure.lang IFn$DDDD IFn$OL)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const T 0)
(def ^:const PX 1) (def ^:const PY 2) (def ^:const PZ 3)
(def ^:const NX 4) (def ^:const NY 5) (def ^:const NZ 6)
(def ^:const AO 7)
(def ^:const STEPS 8)
(def ^:const HALO 9)
(def ^:const DX 10) (def ^:const DY 11) (def ^:const DZ 12)

(defn- v- [[a b c] [x y z]] [(- (double a) (double x)) (- (double b) (double y)) (- (double c) (double z))])

(defn- cross [[a b c] [x y z]]
  (let [a (double a) b (double b) c (double c) x (double x) y (double y) z (double z)]
    [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))]))

(defn unit
  "`v` scaled to length 1."
  [v]
  (let [n (Math/sqrt (double (reduce + (map #(* (double %) (double %)) v))))]
    (mapv #(/ (double %) n) v)))

(defn camera
  "A pinhole camera with world up `up` (default +y): `eye`, `target`, the
   vertical field of view `fov` in degrees across the spec's view [w h]. A
   pixel at scene point (x, y) looks along focal·forward + x·right + y·up."
  ([eye target fov view] (camera eye target fov view [0.0 1.0 0.0]))
  ([eye target fov [_ vh] up]
   (let [f (unit (v- target eye))
         r (unit (cross f up))
         u (cross r f)]
     {:eye (mapv double eye) :forward f :right r :up u
      :focal (/ (/ (double vh) 2.0) (Math/tan (/ (* (double fov) Math/PI) 360.0)))})))

(defn shader
  "A field shader (fn ^long [x y] 0xRRGGBB) ray marching `de`. opts:
     :cam    a `camera`
     :bound  radius of a sphere about the origin that holds the surface
             (rays are marched only inside it); nil marches from t = 0
     :tmax   farthest t (default 20)
     :eps    hit tolerance per unit distance (default 0.0012)
     :steps  march step budget (default 110)
     :ao     reach of the occlusion taps (default 0.12; 0 skips them)
     :shade  (fn ^long [^doubles hit] 0xRRGGBB), called for hits and misses"
  [{:keys [de cam bound tmax eps steps ao shade]
    :or {tmax 20.0 eps 0.0012 steps 110 ao 0.12}}]
  (let [^IFn$DDDD de de
        ^IFn$OL shade shade
        [ex ey ez] (:eye cam) [fx fy fz] (:forward cam) [rx ry rz] (:right cam) [ux uy uz] (:up cam)
        ex (double ex) ey (double ey) ez (double ez)
        fx (double fx) fy (double fy) fz (double fz)
        rx (double rx) ry (double ry) rz (double rz)
        ux (double ux) uy (double uy) uz (double uz)
        k (double (:focal cam))
        bound (when bound (double bound))
        tmax (double tmax) eps (double eps) steps (long steps) reach (double ao)]
    (fn ^long [^double x ^double y]
      (let [h (double-array 13)
            dx0 (+ (* k fx) (* x rx) (* y ux))
            dy0 (+ (* k fy) (* x ry) (* y uy))
            dz0 (+ (* k fz) (* x rz) (* y uz))
            inv (/ 1.0 (Math/sqrt (+ (* dx0 dx0) (* dy0 dy0) (* dz0 dz0))))
            dx (* dx0 inv) dy (* dy0 inv) dz (* dz0 inv)
            ;; the bounding sphere |e + t d| = R: t = -b -+ sqrt(b^2 - c)
            [t0 t1] (if bound
                      (let [R (double bound)
                            b (+ (* ex dx) (* ey dy) (* ez dz))
                            c (- (+ (* ex ex) (* ey ey) (* ez ez)) (* R R))
                            disc (- (* b b) c)]
                        (if (neg? disc)
                          [1.0 0.0]
                          (let [s (Math/sqrt disc)]
                            [(max 0.0 (- (- b) s)) (min tmax (+ (- b) s))])))
                      [0.0 tmax])
            t0 (double t0) t1 (double t1)]
        (aset h DX dx) (aset h DY dy) (aset h DZ dz)
        (loop [i 0 t t0 halo 1.0e9]
          (if (or (>= i steps) (> t t1))
            (do (aset h T -1.0) (aset h STEPS (/ (double i) steps)) (aset h HALO halo))
            (let [px (+ ex (* t dx)) py (+ ey (* t dy)) pz (+ ez (* t dz))
                  d (.invokePrim de px py pz)]
              (if (< d (* eps t))
                (let [e (max 1.0e-5 (* 0.5 eps t))
                      a (.invokePrim de (+ px e) (- py e) (- pz e))
                      b (.invokePrim de (- px e) (- py e) (+ pz e))
                      c (.invokePrim de (- px e) (+ py e) (- pz e))
                      g (.invokePrim de (+ px e) (+ py e) (+ pz e))
                      nx (+ (- a b c) g) ny (+ (- c a b) g) nz (+ (- b a c) g)
                      nn (/ 1.0 (max 1.0e-30 (Math/sqrt (+ (* nx nx) (* ny ny) (* nz nz)))))
                      nx (* nx nn) ny (* ny nn) nz (* nz nn)
                      occ (if (pos? reach)
                            (loop [j 1 occ 0.0 w 1.0]
                              (if (> j 4)
                                (max 0.0 (min 1.0 (- 1.0 (* 1.6 occ))))
                                (let [s (* reach (/ (double j) 4.0))
                                      ds (.invokePrim de (+ px (* s nx)) (+ py (* s ny)) (+ pz (* s nz)))]
                                  (recur (inc j) (+ occ (* w (/ (max 0.0 (- s ds)) reach))) (* w 0.55)))))
                            1.0)]
                  (aset h T t) (aset h PX px) (aset h PY py) (aset h PZ pz)
                  (aset h NX nx) (aset h NY ny) (aset h NZ nz)
                  (aset h AO (double occ)) (aset h STEPS (/ (double i) steps)) (aset h HALO 0.0))
                (recur (inc i) (+ t d) (min halo (/ d (max t 1.0e-3))))))))
        (.invokePrim shade h)))))

(defn tone
  "Light v >= 0 to a display value below 1 by the saturating exposure
   0.96 (1 - e^(-v)): linear near black, so a dark ground stays black, and
   never white-clipping."
  ^double [^double v]
  (* 0.96 (- 1.0 (Math/exp (- (max 0.0 v))))))

(defn pack
  "Linear light r g b, tone mapped, as 0xRRGGBB."
  ^long [^double r ^double g ^double b]
  (bit-or (bit-shift-left (Math/round (* 255.0 (tone r))) 16)
          (bit-shift-left (Math/round (* 255.0 (tone g))) 8)
          (Math/round (* 255.0 (tone b)))))

(defn channel
  "Channel `shift` (16 red, 8 green, 0 blue) of 0xRRGGBB `c` in [0, 1]."
  ^double [^long c ^long shift]
  (/ (double (bit-and (bit-shift-right c shift) 0xFF)) 255.0))

(defn linear
  "Channel `shift` of 0xRRGGBB `c` as a reflectance in [0, 1], squared so
   that a palette's dark stops stay dark under `tone`."
  ^double [^long c ^long shift]
  (let [v (channel c shift)] (* v v)))
