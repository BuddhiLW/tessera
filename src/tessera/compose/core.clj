(ns tessera.compose.core
  "The per-pixel dataflow under composed scenes.

   Every pixel owns a double[] context of named SLOTS (:x :y first). A NODE
   spec {:type k ...} compiles, with a slot allocator, to (fn [u] IStep):
   the u-dependent part is hoisted out of the pixel loop, the IStep runs per
   pixel, reading and writing slots. Charts (layout), fields and factors
   (modulate) and motifs are all nodes in one open registry, `node`.

   EXPRESSIONS (`expr`) are the closed arithmetic a scene writes its
   fields, modulators and weights in, so a scene stays EDN data; the
   :let and :phase nodes bind them to slots per pixel and per frame."
  (:require [desargues.wallpaper.loop :as lp]))

(set! *warn-on-reflection* true)

(defn glow
  "A thin Gaussian filament and a weaker, finite-support halo: the stroke
   profile every composed line is drawn with."
  ^double [^double distance ^double width]
  (let [q (/ distance width) q2 (* q q)]
    (if (> q2 36.0) 0.0
        (+ (* 0.88 (Math/exp (- q2)))
           (* 0.12 (Math/exp (* -0.16 q2)))))))


(definterface IStep
  (^void run [^doubles c]))

(defmulti node
  "Compile node `spec` with slot allocator `slot` (keyword -> long index)
   to (fn [u] IStep). Open: layouts, modulators and motifs extend it."
  (fn [spec _slot] (:type spec)))

(defn allocator
  "A slot allocator: (slot k) returns k's index, allocating on first use.
   :x and :y are always 0 and 1. (deref) returns the slot count."
  []
  (let [table (atom {:x 0 :y 1})]
    (reify
      clojure.lang.IFn
      (invoke [_ k]
        (or (get @table k)
            (get (swap! table #(if (contains? % k) % (assoc % k (count %)))) k)))
      clojure.lang.IDeref
      (deref [_] (count @table)))))

(definterface IFactor
  (^double at [^doubles c]))

(defn- binary [f a b]
  (case f
    :+ (reify IFactor (at [_ c] (+ (.at ^IFactor a c) (.at ^IFactor b c))))
    :- (reify IFactor (at [_ c] (- (.at ^IFactor a c) (.at ^IFactor b c))))
    :* (reify IFactor (at [_ c] (* (.at ^IFactor a c) (.at ^IFactor b c))))
    :/ (reify IFactor (at [_ c] (/ (.at ^IFactor a c) (.at ^IFactor b c))))
    :min (reify IFactor (at [_ c] (min (.at ^IFactor a c) (.at ^IFactor b c))))
    :max (reify IFactor (at [_ c] (max (.at ^IFactor a c) (.at ^IFactor b c))))
    :pow (reify IFactor (at [_ c] (Math/pow (.at ^IFactor a c) (.at ^IFactor b c))))
    :mod (reify IFactor (at [_ c] (mod (.at ^IFactor a c) (.at ^IFactor b c))))
    :hypot (reify IFactor (at [_ c] (Math/hypot (.at ^IFactor a c) (.at ^IFactor b c))))
    :atan2 (reify IFactor (at [_ c] (Math/atan2 (.at ^IFactor a c) (.at ^IFactor b c))))
    :glow (reify IFactor (at [_ c] (glow (.at ^IFactor a c) (.at ^IFactor b c))))))

(defn- unary [f a]
  (case f
    :neg (reify IFactor (at [_ c] (- (.at ^IFactor a c))))
    :abs (reify IFactor (at [_ c] (Math/abs (.at ^IFactor a c))))
    :exp (reify IFactor (at [_ c] (Math/exp (.at ^IFactor a c))))
    :sin (reify IFactor (at [_ c] (Math/sin (.at ^IFactor a c))))
    :cos (reify IFactor (at [_ c] (Math/cos (.at ^IFactor a c))))
    :sqrt (reify IFactor (at [_ c] (Math/sqrt (.at ^IFactor a c))))
    :floor (reify IFactor (at [_ c] (Math/floor (.at ^IFactor a c))))
    :frac (reify IFactor (at [_ c] (lp/frac (.at ^IFactor a c))))
    :wave (reify IFactor (at [_ c] (lp/wave (.at ^IFactor a c))))
    :clamp01 (reify IFactor (at [_ c] (max 0.0 (min 1.0 (.at ^IFactor a c)))))))

(defn expr
  "Compile an EDN expression to an IFactor over the slot context.
   A closed language, so a scene stays data:
     number | :slot | [op arg ..]
   ops: + - * / (n-ary, folded left exactly as Clojure does), min max pow
   hypot atan2 glow (binary), neg abs exp sin cos sqrt floor frac wave
   clamp01 (unary), [:if test then else] (test > 0), [:< a b] [:<= a b]
   (1.0 / 0.0)."
  [e slot]
  (cond
    (number? e) (let [v (double e)] (reify IFactor (at [_ _] v)))
    (keyword? e) (let [i (long (slot e))] (reify IFactor (at [_ c] (aget c i))))
    (vector? e)
    (let [[op & args] e
          xs (mapv #(expr % slot) args)]
      (case op
        (:+ :* :min :max)
        (reduce #(binary op %1 %2) xs)
        (:- :/)
        (if (= 1 (count xs))
          (if (= op :-) (unary :neg (first xs)) (binary :/ (expr 1.0 slot) (first xs)))
          (reduce #(binary op %1 %2) xs))
        (:pow :mod :hypot :atan2 :glow) (binary op (xs 0) (xs 1))
        :if (let [[t a b] xs]
              (reify IFactor (at [_ c] (if (> (.at ^IFactor t c) 0.0) (.at ^IFactor a c) (.at ^IFactor b c)))))
        :< (let [[a b] xs] (reify IFactor (at [_ c] (if (< (.at ^IFactor a c) (.at ^IFactor b c)) 1.0 0.0))))
        :<= (let [[a b] xs] (reify IFactor (at [_ c] (if (<= (.at ^IFactor a c) (.at ^IFactor b c)) 1.0 0.0))))
        (unary op (xs 0))))
    :else (throw (ex-info "Unknown expression" {:expr e}))))

(defmethod node :let
  ;; {:type :let :bind [[slot expr] ..]}: computed slots, in order.
  [{:keys [bind]} slot]
  (let [outs (long-array (map (comp slot first) bind))
        es (object-array (map #(expr (second %) slot) bind))]
    (fn [_]
      (reify IStep
        (run [_ c]
          (dotimes [i (alength outs)]
            (aset c (aget outs i) (.at ^IFactor (aget es i) c))))))))

(defmethod node :phase
  ;; {:type :phase :bind [[slot uexpr] ..]}: per-FRAME constants, uexpr an
  ;; expression over :u alone (hoisted out of the pixel loop). Periodic in u
  ;; by construction when every u term has an integer frequency.
  [{:keys [bind]} slot]
  (let [outs (long-array (map (comp slot first) bind))
        local (allocator)
        iu (long (local :u))
        es (mapv #(expr (second %) local) bind)
        n @local]
    (fn [u]
      (let [ctx (double-array (max n 3))
            _ (aset ctx iu (double u))
            vals (double-array (map #(.at ^IFactor % ctx) es))]
        (reify IStep
          (run [_ c]
            (dotimes [i (alength outs)] (aset c (aget outs i) (aget vals i)))))))))

(defn chain
  "Compile node specs in order to (fn [u] IStep) running all of them."
  [specs slot]
  (let [compiled (mapv #(node % slot) specs)]
    (fn [u]
      (let [steps (object-array (map #(% u) compiled))]
        (reify IStep
          (run [_ c]
            (dotimes [i (alength steps)] (.run ^IStep (aget steps i) c))))))))
