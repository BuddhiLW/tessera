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
   faint |psi| glow. At the screen (x = `screen`) the wall glows with the
   fringe intensity I(y), and an intensity graph stands out of it: a bar of
   length proportional to I(y), flashing as each crest arrives."
  [u {:keys [k bx d a per screen bar periods pos neg sharp]
      :or {k 7.0 bx -4.5 d 1.6 a 0.35 per 6 screen 4.6 bar 2.2 periods 2 sharp 4.0}}]
  (let [^doubles src (slit-sources bx d a per)
        n (quot (alength src) 2)
        k (double k) bx (double bx) d (double d) a (double a)
        screen (double screen) bar (double bar) sharp (double sharp)
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
        ;; the fringe profile at the screen, sampled once per frame
        ns 1024 y0 -4.5 y1 4.5
        ^doubles prof (double-array ns)
        _ (dotimes [i ns]
            (let [^doubles c (amp screen (+ y0 (* (- y1 y0) (/ (+ i 0.5) ns))))]
              (aset prof i (+ (* (aget c 0) (aget c 0)) (* (aget c 1) (aget c 1))))))
        imax (max 1e-9 (reduce max (seq prof)))
        intensity (fn ^double [^double y]
                    (let [i (long (* ns (/ (- y y0) (- y1 y0))))]
                      (/ (aget prof (max 0 (min (dec ns) i))) imax)))
        paint (fn ^long [^double re ^double mag]
                (let [s (* (min 1.0 mag) (Math/pow (min 1.0 (Math/abs re)) sharp))
                      glow (* 0.10 (min 1.0 mag))]
                  (if (pos? re)
                    (r/rgb (+ glow (* pr s)) (+ (* 0.6 glow) (* pg s)) (+ glow (* pb s)))
                    (r/rgb (+ (* 0.6 glow) (* nr s)) (+ (* 0.6 glow) (* ng s)) (+ glow (* nb s))))))]
    (fn ^long [^double x ^double y]
      (cond
        ;; the slit barrier
        (and (< (Math/abs (- x bx)) 0.05)
             (not (or (< (Math/abs (- y half)) ha) (< (Math/abs (+ y half)) ha))))
        (r/rgb 0.35 0.36 0.45)
        (< x bx)
        (paint (Math/cos (- (* k x) wt)) 0.8)
        ;; the screen and its intensity graph
        (>= x screen)
        (let [iy (intensity y)
              ^doubles c (amp screen y)
              cr (aget c 0) ci (aget c 1)
              re (+ (* cr cw) (* ci sw))
              mag2 (max 1e-12 (+ (* cr cr) (* ci ci)))
              hit (/ (* re re) mag2)
              flash (+ 0.45 (* 0.55 hit))
              rel (/ (- x screen) bar)]
          (cond
            (< rel 0.03) (let [g (* flash (Math/sqrt iy))] (r/rgb (* 1.0 g) (* 0.85 g) (* 0.6 g)))
            (< rel iy)   (let [g (* flash (+ 0.25 (* 0.75 iy)) (- 1.0 (* 0.5 (/ rel (max 1e-6 iy)))))
                               tip (Math/exp (* -400.0 (Math/pow (- rel iy) 2.0)))]
                           (r/rgb (+ (* g 1.0) tip) (+ (* g (+ 0.35 (* 0.45 iy))) tip) (+ (* g 0.25) (* 0.8 tip))))
            :else        (let [g (* 0.04 iy)] (r/rgb g g (* 1.4 g)))))
        :else
        (let [^doubles c (amp x y)
              cr (aget c 0) ci (aget c 1)
              re (+ (* cr cw) (* ci sw))
              mag (Math/sqrt (+ (* cr cr) (* ci ci)))]
          (paint (/ re (max 1e-9 mag)) (* 1.4 mag)))))))

(def double-slit
  {:id :double-slit
   :title "Double slit: a plane wave through two slits, the fringe intensity standing out of the screen"
   :seconds 12 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0]
   :field (fn [u] (slit-shader u {:k 10.0 :d 2.6 :a 0.3}))
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

(defn carpet-field
  "Complex psi(x, tau) on a `w` x `h` grid, flat [re im ..]: x in [0, 1]
   along the rows (down the screen), tau from `tau0` over `span` revivals
   across the columns."
  ^doubles [^doubles cs w h tau0 span]
  (let [nmax (quot (alength cs) 2)
        w (long w) h (long h)
        modes (let [a (double-array (* nmax h))]
                (dotimes [nn nmax]
                  (dotimes [j h]
                    (aset a (+ (* nn h) j) (* (Math/sqrt 2.0) (Math/sin (* Math/PI (inc nn) (/ (+ j 0.5) h)))))))
                a)
        out (double-array (* 2 w h))]
    (.forEach (.parallel (java.util.stream.IntStream/range 0 (int w)))
              (reify java.util.function.IntConsumer
                (accept [_ i]
                  (let [tau (+ (double tau0) (* (double span) (/ (+ i 0.5) w)))
                        ph (double-array (* 2 nmax))]
                    (dotimes [nn nmax]
                      (let [ang (* -1.0 lp/tau (* (inc nn) (inc nn)) tau)
                            c (Math/cos ang) s (Math/sin ang)
                            re (aget cs (* 2 nn)) im (aget cs (inc (* 2 nn)))]
                        (aset ph (* 2 nn) (- (* re c) (* im s)))
                        (aset ph (inc (* 2 nn)) (+ (* re s) (* im c)))))
                    (dotimes [j h]
                      (loop [nn 0 re 0.0 im 0.0]
                        (if (< nn nmax)
                          (let [b (aget modes (+ (* nn h) j))]
                            (recur (inc nn) (+ re (* b (aget ph (* 2 nn)))) (+ im (* b (aget ph (inc (* 2 nn)))))))
                          (let [o (* 2 (+ (* j w) i))]
                            (aset out o re)
                            (aset out (inc o) im)))))))))
    out))

(defn- hue->rgb
  "Channels of hue `h` (turns) at full saturation, smooth (cosine) wheel."
  [^double h]
  [(+ 0.5 (* 0.5 (Math/cos (* lp/tau h))))
   (+ 0.5 (* 0.5 (Math/cos (* lp/tau (- h (/ 1.0 3.0))))))
   (+ 0.5 (* 0.5 (Math/cos (* lp/tau (- h (/ 2.0 3.0))))))])

(defn phase-carpet-shader
  "Shader of the carpet in domain colouring at phase `u`: hue is the phase
   of psi, brightness rises with |psi|^2; the box runs down the screen and
   time scrolls across it through one revival per loop."
  [u {:keys [span view knee] :or {span 0.22 view [(/ 128.0 9.0) 8.0] knee 1.4}}]
  (let [gw 1600 gh 900
        ^doubles g (carpet-field @carpet-coefficients gw gh (double u) span)
        [vw vh] view
        vw (double vw) vh (double vh) knee (double knee)]
    (fn ^long [^double x ^double y]
      (let [fx (+ 0.5 (/ x vw)) fy (- 0.5 (/ y vh))
            i (max 0 (min (dec gw) (long (* fx gw))))
            j (max 0 (min (dec gh) (long (* fy gh))))
            o (* 2 (+ (* j gw) i))
            re (aget g o) im (aget g (inc o))
            v (+ (* re re) (* im im))
            t (/ v (+ v knee))
            b (Math/pow t 1.3)
            [cr cg cb] (hue->rgb (/ (Math/atan2 im re) lp/tau))
            hot (* 3.0 (Math/pow (max 0.0 (- t 0.8)) 2.0))]
        (r/rgb (+ (* b (double cr)) hot) (+ (* b (double cg)) hot) (+ (* b (double cb)) hot))))))

(def quantum-tapestry
  {:id :quantum-tapestry
   :title "Quantum tapestry: the Talbot carpet in phase colour, time flowing across"
   :seconds 24 :fps 30 :size [1920 1080] :view [(/ 128.0 9.0) 8.0]
   :field (fn [u] (phase-carpet-shader u {}))
   :post (fn [_] [{:type :bloom :radius 10 :strength 0.9}
                  {:type :chroma :shift 2}
                  {:type :vignette :strength 0.45}])})
