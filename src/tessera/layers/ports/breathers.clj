(ns tessera.layers.ports.breathers
  "Field port of :breathers (tessera.waves): twelve exact sine-Gordon
   breathers, their lit height field and analytic slope, as glowing level
   sets."
  (:require [tessera.layers.ports.forms :as f]
            [tessera.waves :as waves]))

(defn- breather-terms
  "Per breather: [bindings h-term dh/dx-term dh/dy-term], unrolled in the
   order of waves/breather-lattice."
  []
  (for [[i {:keys [n k x0 y0 phase]}] (map-indexed vector waves/breather-lattice)
        :let [s #(symbol (str % "_" i)) k (double k)]]
    [[(s "s") (list 'sin (list '+ (list '* (* f/tau n) 'u) phase))
      (s "dx") (list '- 'x x0) (s "dy") (list '- 'y y0)
      (s "env") (list 'exp (list '* -0.55 (list '* (s "dy") (s "dy"))))
      (s "denv") (list '* -1.1 (s "dy") (s "env"))
      (s "z") (list '* k (s "dx"))
      ;; cosh, sinh and atan are not in the DSL: exponentials, and
      ;; atan2(a, 1) (fdlibm returns atan(a) for x = 1). Parity is
      ;; measured at the 8-bit output, not assumed.
      (s "ez") (list 'exp (s "z")) (s "emz") (list 'exp (list '- (s "z")))
      (s "c") (list '* 0.5 (list '+ (s "ez") (s "emz")))
      (s "sh") (list '* 0.5 (list '- (s "ez") (s "emz")))
      (s "a") (list '/ (list '* k (s "s")) (s "c"))
      (s "denom") (list '+ 1.0 (list '* (s "a") (s "a")))
      (s "v") (list '* 4.0 (list 'atan2 (s "a") 1.0))
      (s "dv") (list '* -4.0 (list '/ (list '* k k (s "s") (s "sh"))
                                   (list '* (s "c") (s "c") (s "denom"))))]
     (list '* (s "v") (s "env")) (list '* (s "dv") (s "env")) (list '* (s "v") (s "denv"))]))

(def breathers
  (let [terms (breather-terms)]
    {:id :breathers
     :title "Sine-Gordon breathers: glowing level sets of an exact breather lattice, warm peaks and cool troughs"
     :period 12 :view [(/ 128.0 9.0) 8.0] :blend :lighten
     :params [['level_density {:default 2.2 :min 0.5 :max 8.0 :label "Level sets per unit height"}]
              ['line_sharpness {:default 500.0 :min 50.0 :max 3000.0 :label "Level-line sharpness"}]]
     :form {:params '[u level_density line_sharpness]
            :body (list 'let (vec (concat
                                   (mapcat first terms)
                                   ['h (f/sum-of (map second terms))
                                    'gx (f/sum-of (map #(nth % 2) terms))
                                    'gy (f/sum-of (map #(nth % 3) terms))
                                    'ah '(abs h)
                                    'warm (list '- 1.0 (f/ge '(- h)))]
                                   '[lv (* h level_density)
                                     fr (- lv (floor lv))
                                     dist (min fr (- 1.0 fr))
                                     line0 (exp (* (- line_sharpness) (* dist dist)))
                                     grad (sqrt (+ (* gx gx) (* gy gy)))
                                     ;; Width cap, as in waves/breather-shader:
                                     ;; no level line wider than
                                     ;; breather-line-width, so the flat field
                                     ;; at u = 0, 1/4, 1/2, 3/4 does not flash.
                                     dh (/ dist level_density)
                                     wq (* 0.05 grad)
                                     cap (exp (- (/ (* dh dh) (+ (* wq wq) 1e-12))))
                                     aa (/ 1.0 (+ 1.0 (* 0.04 grad)))
                                     line (* line0 aa cap)
                                     body (* 0.05 (pow (min 1.0 (/ ah 4.5)) 1.8))
                                     spec (pow (max 0.0 (/ (+ (* -0.5 gx) (* 0.866 gy)) (+ 1.0 grad))) 3.0)]))
                        '[(+ 0.001 (* body 0.95) (* line (+ 0.40 (* 0.55 warm))) (* 0.10 spec))
                          (+ 0.0015 (* body 0.80) (* line (+ 0.60 (* 0.18 warm))) (* 0.16 spec))
                          (+ 0.005 (* body 1.05) (* line (+ 0.95 (* -0.60 warm))) (* 0.26 spec))])}}))
