(ns tessera.quantum
  "Solutions of quantum mechanics as wallpaper.

     :double-slit     a plane wave meets a barrier with two slits; Huygens
                      sources in each slit interfere. Re(psi) ripples in two
                      colours, brightness by |psi|, and the time-averaged
                      fringes glow on a screen at the right. The wave
                      advances whole periods per loop.
     :quantum-carpet  a Gaussian packet in an infinite well. Level n has
                      energy proportional to n^2, so psi revives exactly after
                      one revival time: the loop is seamless by physics. Shown
                      as |psi(x, t)|^2 with x across and time scrolling up
                      through one full revival (the Talbot carpet)."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [tessera.palette :as pal]))

(set! *warn-on-reflection* true)

(defn- channels [^long c]
  [(/ (bit-and (bit-shift-right c 16) 0xFF) 255.0)
   (/ (bit-and (bit-shift-right c 8) 0xFF) 255.0)
   (/ (bit-and c 0xFF) 255.0)])

;; ── Double slit ───────────────────────────────────────────────────────────

(defn slit-sources
  "Flat [x y ..] of `per` Huygens sources across each of two slits of width
   `a`, centres at y = ±d/2, in the barrier at x = `bx`."
  ^doubles [bx d a per]
  (double-array
   (mapcat identity
           (for [cy [(- (/ d 2.0)) (/ d 2.0)]
                 i (range per)
                 :let [y (+ cy (* a (- (/ (+ i 0.5) per) 0.5)))]]
             [bx y]))))

(defn slit-shader
  "Shader of the two-slit field at phase `u` (`periods` wave periods per
   loop): crests of Re(psi) as thin neon bands in two colours on black, a
   faint |psi| glow, fringes of the time-averaged intensity on the screen."
  [u {:keys [k bx d a per screen periods pos neg sharp]
      :or {k 7.0 bx -4.5 d 1.6 a 0.35 per 6 screen 6.6 periods 2 sharp 4.0}}]
  (let [^doubles src (slit-sources bx d a per)
        n (quot (alength src) 2)
        k (double k) bx (double bx) d (double d) a (double a) screen (double screen)
        sharp (double sharp)
        wt (* lp/tau (double periods) (double u))
        cw (Math/cos wt) sw (Math/sin wt)
        [pr pg pb] (channels (r/hex->rgb (or pos "#ff2b55")))
        [nr ng nb] (channels (r/hex->rgb (or neg "#2f6bff")))
        half (/ d 2.0) ha (/ a 2.0)
        norm (/ 1.0 (* 0.6 n))
        amp (fn ^doubles [^double x ^double y]
              (loop [i 0 re 0.0 im 0.0]
                (if (< i n)
                  (let [dx (- x (aget src (* 2 i))) dy (- y (aget src (inc (* 2 i))))
                        rr (Math/sqrt (+ (* dx dx) (* dy dy) 1e-6))
                        w (/ 1.0 (Math/sqrt rr))]
                    (recur (inc i) (+ re (* w (Math/cos (* k rr)))) (+ im (* w (Math/sin (* k rr))))))
                  (doto (double-array 2) (aset 0 (* norm re)) (aset 1 (* norm im))))))
        paint (fn ^long [^double re ^double mag]
                (let [s (* (min 1.0 mag) (Math/pow (min 1.0 (Math/abs re)) sharp))
                      glow (* 0.10 (min 1.0 mag))]
                  (if (pos? re)
                    (r/rgb (+ glow (* pr s)) (+ (* 0.6 glow) (* pg s)) (+ glow (* pb s)))
                    (r/rgb (+ (* 0.6 glow) (* nr s)) (+ (* 0.6 glow) (* ng s)) (+ glow (* nb s))))))]
    (fn ^long [^double x ^double y]
      (cond
        (and (< (Math/abs (- x bx)) 0.05)
             (not (or (< (Math/abs (- y half)) ha) (< (Math/abs (+ y half)) ha))))
        (r/rgb 0.35 0.36 0.45)
        (< x bx)
        (paint (Math/cos (- (* k x) wt)) 0.8)
        (> x screen)
        (let [^doubles c (amp screen y)
              i2 (+ (* (aget c 0) (aget c 0)) (* (aget c 1) (aget c 1)))
              g (min 1.0 (* 3.0 i2))]
          (r/rgb (* g 1.0) (* g 0.78) (* g 0.45)))
        :else
        (let [^doubles c (amp x y)
              cr (aget c 0) ci (aget c 1)
              re (+ (* cr cw) (* ci sw))
              mag (Math/sqrt (+ (* cr cr) (* ci ci)))]
          (paint (/ re (max 1e-9 mag)) (* 1.4 mag)))))))

(def double-slit
  {:id :double-slit
   :title "Double slit: a plane wave through two slits, fringes on the screen"
   :seconds 12 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0]
   :field (fn [u] (slit-shader u {}))
   :post (fn [_] [{:type :bloom :radius 10 :strength 0.9}
                  {:type :vignette :strength 0.45}])})

;; ── Quantum carpet ────────────────────────────────────────────────────────

(defn packet-coefficients
  "Coefficients c_n, n = 1..`nmax`, of a Gaussian packet centred at `x0`
   with width `sigma` and momentum `p` in the unit well, by quadrature:
   [re im] pairs, flat."
  ^doubles [nmax x0 sigma p]
  (let [m 4096
        dx (/ 1.0 m)
        xs (map #(* (+ % 0.5) dx) (range m))
        env (fn [x] (Math/exp (- (/ (Math/pow (- x x0) 2.0) (* 2.0 sigma sigma)))))
        cs (for [nn (range 1 (inc nmax))]
             (reduce (fn [[re im] x]
                       (let [b (* (Math/sqrt 2.0) (Math/sin (* Math/PI nn x)) (env x) dx)]
                         [(+ re (* b (Math/cos (* p x)))) (+ im (* b (Math/sin (* p x))))]))
                     [0.0 0.0] xs))
        nrm (Math/sqrt (reduce + (map (fn [[a b]] (+ (* a a) (* b b))) cs)))]
    (double-array (mapcat (fn [[a b]] [(/ a nrm) (/ b nrm)]) cs))))

(defn carpet-grid
  "|psi(x, tau)|^2 on a `w` x `h` grid: x in [0, 1] across, tau from `tau0`
   over `span` revival times up the rows. tau = 1 is one full revival."
  ^doubles [^doubles cs w h tau0 span]
  (let [nmax (quot (alength cs) 2)
        w (long w) h (long h)
        modes (let [a (double-array (* nmax w))]
                (dotimes [nn nmax]
                  (dotimes [i w]
                    (aset a (+ (* nn w) i) (* (Math/sqrt 2.0) (Math/sin (* Math/PI (inc nn) (/ (+ i 0.5) w)))))))
                a)
        out (double-array (* w h))]
    (.forEach (.parallel (java.util.stream.IntStream/range 0 (int h)))
              (reify java.util.function.IntConsumer
                (accept [_ j]
                  (let [tau (+ (double tau0) (* (double span) (/ (+ j 0.5) h)))
                        ph (double-array (* 2 nmax))]
                    (dotimes [nn nmax]
                      (let [a (* -1.0 lp/tau (* (inc nn) (inc nn)) tau)
                            c (Math/cos a) s (Math/sin a)
                            re (aget cs (* 2 nn)) im (aget cs (inc (* 2 nn)))]
                        (aset ph (* 2 nn) (- (* re c) (* im s)))
                        (aset ph (inc (* 2 nn)) (+ (* re s) (* im c)))))
                    (dotimes [i w]
                      (loop [nn 0 re 0.0 im 0.0]
                        (if (< nn nmax)
                          (let [b (aget modes (+ (* nn w) i))]
                            (recur (inc nn) (+ re (* b (aget ph (* 2 nn)))) (+ im (* b (aget ph (inc (* 2 nn)))))))
                          (aset out (+ (* j w) i) (+ (* re re) (* im im))))))))))
    out))

(def ^:private carpet-coefficients (delay (packet-coefficients 40 0.5 0.05 0.0)))

(defn carpet-shader
  "Shader of the carpet at phase `u`: the window of `span` revivals starting
   at tau = u, sampled from a precomputed grid. |psi|^2 averages 1 over the
   well, so brightness rises steeply around that level: black at the nodes,
   `palette` through the body, white-hot at the revivals. `span` keeps the
   fastest mode below a cycle per few rows, or the carpet aliases."
  [u {:keys [palette span view knee] :or {span 0.15 view [(/ 128.0 9.0) 8.0] knee 1.6}}]
  (let [gw 1024 gh 1080
        ^doubles g (carpet-grid @carpet-coefficients gw gh (double u) span)
        [vw vh] view
        vw (double vw) vh (double vh) knee (double knee)]
    (fn ^long [^double x ^double y]
      (let [fx (+ 0.5 (/ x vw)) fy (+ 0.5 (/ y vh))
            i (max 0 (min (dec gw) (long (* fx gw))))
            j (max 0 (min (dec gh) (long (* fy gh))))
            v (aget g (+ (* j gw) i))
            t (/ v (+ v knee))
            [cr cg cb] (channels (pal/at palette t))
            b (Math/pow t 1.7)
            hot (Math/pow (max 0.0 (- t 0.75)) 2.0)]
        (r/rgb (+ (* b (double cr)) (* 6.0 hot))
               (+ (* b (double cg)) (* 6.0 hot))
               (+ (* b (double cb)) (* 6.0 hot)))))))

(def quantum-carpet
  (let [p (pal/scheme :blue-magenta-orange)]
    {:id :quantum-carpet
     :title "Quantum carpet: a packet in a box revives, the Talbot carpet scrolls"
     :seconds 24 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0]
     :field (fn [u] (carpet-shader u {:palette p}))
     :post (fn [_] [{:type :bloom :radius 10 :strength 0.9}
                    {:type :vignette :strength 0.4}])}))
