(ns tessera.layers.ports.forms
  "Field-form builders shared by the tessera.layers.ports families.

   The DSL has no if, loops or arrays, so:
   - fixed-length loops are unrolled at build time, in the closure's order
     (sum-of keeps the (loop [acc 0.0] ..) accumulation order);
   - branches are 0/1 masks: (ge d) = clamp(1 + floor d, 0, 1) is exactly
     d >= 0, and m*a + (1-m)*b is exactly a or b while both are finite;
   - lookup tables the closures precompute (sine tables, per-cell carpet
     grids, the slit's fringe profile) are re-evaluated per pixel at the
     very cell centre the table would have been sampled at.
   tessera.layers requires the ports, so the palette helpers are restated
   here rather than shared from there."
  (:require [thi.ng.color.gradients :as grad]))

(def tau (* 2.0 Math/PI))

(defn quantized-channel
  "floor(255 clamp(e) + 1/2)/255: the 8-bit round trip of pal/at, kept
   because the closures shade with the QUANTIZED palette colour."
  [e]
  (list '/ (list 'floor (list '+ (list '* 255.0 (list 'clamp e 0.0 1.0)) 0.5)) 255.0))

(defn palette-channels
  "[r g b] forms of thi.ng cosine scheme `scheme` at t, each quantized."
  [scheme t]
  (let [[a b c d] (get grad/cosine-schemes scheme)]
    (mapv (fn [aa bb cc dd] (quantized-channel (list 'cospal t aa bb cc dd))) a b c d)))

(defn sum-of
  "(+ (+ (+ 0.0 a) b) ..): the accumulation order of a (loop [acc 0.0] ..)."
  [terms]
  (reduce (fn [acc t] (list '+ acc t)) 0.0 terms))

(defn ge
  "1.0 iff d >= 0, else 0.0, exactly."
  [d]
  (list 'clamp (list '+ 1.0 (list 'floor d)) 0.0 1.0))

(defn lt
  "1.0 iff a < b (the sign of a - b is exact in IEEE)."
  [a b]
  (list '- 1.0 (ge (list '- a b))))

(defn select3
  "Per channel m*a + (1-m)*b, m a 0/1 mask."
  [m a b]
  (mapv (fn [x y] (list '+ (list '* m x) (list '* (list '- 1.0 m) y))) a b))

(defn table-sine
  "The entry sin(pi k i/(samples-1)) of tessera.orbitals' sine table at the
   cell i = clamp(trunc(q (samples-1))) the closure reads for q (floor and
   trunc agree after the clamp)."
  [k q samples]
  (let [n (double (dec samples))]
    (list 'sin (list '* (* Math/PI k) (list '/ (list 'clamp (list 'floor (list '* q n)) 0.0 n) n)))))

(defn hue-wheel
  "Full-saturation cosine colour wheel of hue h (turns)."
  [h]
  [(list '+ 0.5 (list '* 0.5 (list 'cos (list '* 'tau h))))
   (list '+ 0.5 (list '* 0.5 (list 'cos (list '* 'tau (list '- h (/ 1.0 3.0))))))
   (list '+ 0.5 (list '* 0.5 (list 'cos (list '* 'tau (list '- h (/ 2.0 3.0))))))])
