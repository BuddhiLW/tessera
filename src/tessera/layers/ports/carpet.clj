(ns tessera.layers.ports.carpet
  "Field ports of :quantum-carpet and :quantum-tapestry (tessera.quantum):
   a Gaussian packet in the unit well, 40 modes, revived exactly after one
   loop. The closures tabulate psi on a grid per frame; here each pixel
   evaluates psi at its grid cell's centre, so the per-pixel cost is 40
   modes. e^{-2 pi i n^2 tau} is reduced as -2 pi frac(n^2 tau) (n^2 is an
   integer, so the phase is the same) to keep GPU sin/cos in range."
  (:require [tessera.layers.ports.forms :as f]
            [tessera.quantum :as quantum]))

(def ^:private coefficients
  (vec @@#'quantum/carpet-coefficients))

(defn- psi-form
  "Bindings of psi = sum_n b_n(q) c_n e^{-2 pi i n^2 t} into psi_re, psi_im;
   b_n(q) = sqrt 2 sin(pi n q), q the cell centre across the box."
  [q t]
  (let [terms (for [nn (range (quot (count coefficients) 2))
                    :let [n (inc nn) s #(symbol (str % "_" n))
                          re (coefficients (* 2 nn)) im (coefficients (inc (* 2 nn)))]]
                [[(s "ang") (list '* (- f/tau) (list 'frac (list '* (double (* n n)) t)))
                  (s "c") (list 'cos (s "ang")) (s "s") (list 'sin (s "ang"))
                  (s "b") (list '* (Math/sqrt 2.0) (list 'sin (list '* (* Math/PI n) q)))]
                 (list '* (s "b") (list '- (list '* re (s "c")) (list '* im (s "s"))))
                 (list '* (s "b") (list '+ (list '* re (s "s")) (list '* im (s "c"))))])]
    (vec (concat (mapcat first terms)
                 ['psi_re (f/sum-of (map second terms)) 'psi_im (f/sum-of (map #(nth % 2) terms))]))))

(def quantum-carpet
  (let [[cr cg cb] (f/palette-channels :blue-magenta-orange 't)]
    {:id :quantum_carpet
     :title "Quantum carpet: a packet in a box revives, the Talbot carpet scrolls"
     :period 24 :view [(/ 128.0 9.0) 8.0] :blend :lighten
     :params [['time_span {:default 0.15 :min 0.02 :max 1.0 :label "Revival times up the frame"}]
              ['knee {:default 1.6 :min 0.1 :max 8.0 :label "Density knee"}]]
     ;; 1024 x 1080 grid: x across the box, time up the rows from tau = u
     :form {:params '[u time_span knee]
            :body (list 'let (vec (concat
                                   ['fx (list '+ 0.5 (list '/ 'x (/ 128.0 9.0))) 'fy (list '+ 0.5 (list '/ 'y 8.0))
                                    'ci '(clamp (floor (* fx 1024.0)) 0.0 1023.0)
                                    'cj '(clamp (floor (* fy 1080.0)) 0.0 1079.0)
                                    'cell_tau '(+ u (* time_span (/ (+ cj 0.5) 1080.0)))
                                    'cell_q '(/ (+ ci 0.5) 1024.0)]
                                   (psi-form 'cell_q 'cell_tau)
                                   ['v '(+ (* psi_re psi_re) (* psi_im psi_im))
                                    't '(/ v (+ v knee))
                                    'cr cr 'cg cg 'cb cb
                                    'b '(pow t 1.7)
                                    'hot '(pow (max 0.0 (- t 0.75)) 2.0)]))
                        '[(+ (* b cr) (* 6.0 hot)) (+ (* b cg) (* 6.0 hot)) (+ (* b cb) (* 6.0 hot))])}}))

(def quantum-tapestry
  (let [[hr hg hb] (f/hue-wheel 'h)]
    {:id :quantum_tapestry
     :title "Quantum tapestry: the Talbot carpet in phase colour, time flowing across"
     :period 24 :view [(/ 128.0 9.0) 8.0] :blend :lighten
     :params [['time_span {:default 0.22 :min 0.02 :max 1.0 :label "Revival times across the frame"}]
              ['knee {:default 1.4 :min 0.1 :max 8.0 :label "Density knee"}]]
     ;; 1600 x 900 grid: the box down the screen, time across from tau = u
     :form {:params '[u time_span knee]
            :body (list 'let (vec (concat
                                   ['fx (list '+ 0.5 (list '/ 'x (/ 128.0 9.0))) 'fy (list '- 0.5 (list '/ 'y 8.0))
                                    'ci '(clamp (floor (* fx 1600.0)) 0.0 1599.0)
                                    'cj '(clamp (floor (* fy 900.0)) 0.0 899.0)
                                    'cell_tau '(+ u (* time_span (/ (+ ci 0.5) 1600.0)))
                                    'cell_q '(/ (+ cj 0.5) 900.0)]
                                   (psi-form 'cell_q 'cell_tau)
                                   '[v (+ (* psi_re psi_re) (* psi_im psi_im))
                                     t (/ v (+ v knee))
                                     b (pow t 1.3)
                                     h (/ (atan2 psi_im psi_re) tau)
                                     hot (* 3.0 (pow (max 0.0 (- t 0.8)) 2.0))]))
                        [(list '+ (list '* 'b hr) 'hot) (list '+ (list '* 'b hg) 'hot) (list '+ (list '* 'b hb) 'hot)])}}))
