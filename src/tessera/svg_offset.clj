(ns tessera.svg-offset
  "Concentric offset outlines from any SVG (design memory 20261108205043-257c55ee).

   One fixed SVG. Every filled shape gets a signed distance field d
   (negative inside) and an id; rings are nested outlines inside each shape,
   spaced by the shape's own inradius, crawling inward or outward with the
   loop phase while a palette, indexed by an order key, cycles through them.

     shapes   SVG text -> filled shapes: closed rings in scene units (y up),
              fill colour, group index, centroid, bbox
     bake     shapes -> a grid of shape id and signed distance, plus each
              shape's inradius and order keys
     shader   a baked field at phase u -> a primitive fn x y -> 0xRRGGBB

   Seamless: the ring phase is speed*u + phase and the hue phase
   hue-speed*u; with integer speed and hue-speed every term returns at
   u = 1, so frame N is frame 0."
  (:require [clojure.string :as str]
            [clojure.xml :as xml]
            [desargues.player.svg-path :as svg-path])
  (:import (java.io ByteArrayInputStream)))

(set! *warn-on-reflection* true)

;; ── SVG reading ──────────────────────────────────────────────────────────

(defn- local-tag
  "The tag of an XML element without its namespace prefix, as a keyword."
  [el]
  (let [t (name (:tag el))] (keyword (subs t (inc (or (str/last-index-of t ":") -1))))))

(defn- numbers [s]
  (mapv parse-double (re-seq #"[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?" (str s))))

(defn- num-attr ^double [el k dflt]
  (if-let [v (get-in el [:attrs k])] (double (first (numbers v))) (double dflt)))

;; affine [a b c d e f]: x' = a x + c y + e, y' = b x + d y + f (SVG order)
(def identity-xf [1.0 0.0 0.0 1.0 0.0 0.0])

(defn compose
  "The affine map applying `inner` first, then `outer`."
  [[a b c d e f] [a2 b2 c2 d2 e2 f2]]
  [(+ (* a a2) (* c b2)) (+ (* b a2) (* d b2))
   (+ (* a c2) (* c d2)) (+ (* b c2) (* d d2))
   (+ (* a e2) (* c f2) e) (+ (* b e2) (* d f2) f)])

(defn apply-xf [[a b c d e f] x y]
  (let [x (double x) y (double y)]
    [(+ (* (double a) x) (* (double c) y) (double e))
     (+ (* (double b) x) (* (double d) y) (double f))]))

(defn parse-transform
  "The affine map of an SVG transform attribute (matrix, translate, scale,
   rotate, skewX, skewY), composed left to right."
  [s]
  (reduce
   (fn [xf [_ op args]]
     (let [n (numbers args)
           rad (fn [deg] (* (double deg) (/ Math/PI 180.0)))
           m (case op
               "matrix" (vec (take 6 (concat n (repeat 0.0))))
               "translate" [1.0 0.0 0.0 1.0 (nth n 0 0.0) (nth n 1 0.0)]
               "scale" (let [sx (nth n 0 1.0)] [sx 0.0 0.0 (nth n 1 sx) 0.0 0.0])
               "rotate" (let [t (rad (nth n 0 0.0)) c (Math/cos t) s (Math/sin t)
                              r [c s (- s) c 0.0 0.0]]
                          (if (>= (count n) 3)
                            (let [cx (nth n 1) cy (nth n 2)]
                              (compose (compose [1.0 0.0 0.0 1.0 cx cy] r) [1.0 0.0 0.0 1.0 (- cx) (- cy)]))
                            r))
               "skewX" [1.0 0.0 (Math/tan (rad (nth n 0 0.0))) 1.0 0.0 0.0]
               "skewY" [1.0 (Math/tan (rad (nth n 0 0.0))) 0.0 1.0 0.0 0.0]
               identity-xf)]
       (compose xf m)))
   identity-xf
   (re-seq #"(matrix|translate|scale|rotate|skewX|skewY)\s*\(([^)]*)\)" (str s))))

(def ^:private named-colours
  {"black" 0x000000 "white" 0xffffff "red" 0xff0000 "green" 0x008000 "blue" 0x0000ff
   "yellow" 0xffff00 "cyan" 0x00ffff "magenta" 0xff00ff "orange" 0xffa500
   "purple" 0x800080 "gold" 0xffd700 "gray" 0x808080 "grey" 0x808080 "navy" 0x000080
   "teal" 0x008080 "crimson" 0xdc143c})

(defn parse-colour
  "0xRRGGBB of an SVG paint (#rgb, #rrggbb, rgb(r,g,b), a basic name);
   :none for none or transparent; nil when unparseable (inherit)."
  [s]
  (when s
    (let [s (str/lower-case (str/trim (str s)))]
      (cond
        (contains? #{"none" "transparent"} s) :none
        (re-matches #"#[0-9a-f]{6}" s) (Long/parseLong (subs s 1) 16)
        (re-matches #"#[0-9a-f]{3}" s) (Long/parseLong (apply str (mapcat (fn [c] [c c]) (subs s 1))) 16)
        (str/starts-with? s "rgb") (let [[r g b] (map long (numbers s))]
                                     (bit-or (bit-shift-left r 16) (bit-shift-left g 8) b))
        :else (named-colours s)))))

(defn- style-fill
  "The fill an element declares, directly or in its style attribute."
  [el]
  (let [style (get-in el [:attrs :style])
        from-style (some->> style (re-find #"(?:^|;)\s*fill\s*:\s*([^;]+)") second)]
    (parse-colour (or from-style (get-in el [:attrs :fill])))))

(defn element-d
  "An SVG path string for a shape element (path, rect, circle, ellipse,
   polygon, polyline), nil for anything else."
  [el]
  (let [a (:attrs el)]
    (case (local-tag el)
      :path (:d a)
      :rect (let [x (num-attr el :x 0) y (num-attr el :y 0)
                  w (num-attr el :width 0) h (num-attr el :height 0)]
              (when (and (pos? w) (pos? h))
                (format "M %s %s H %s V %s H %s Z" x y (+ x w) (+ y h) x)))
      :circle (let [cx (num-attr el :cx 0) cy (num-attr el :cy 0) r (num-attr el :r 0)]
                (when (pos? r)
                  (format "M %s %s A %s %s 0 1 0 %s %s A %s %s 0 1 0 %s %s Z"
                          (- cx r) cy r r (+ cx r) cy r r (- cx r) cy)))
      :ellipse (let [cx (num-attr el :cx 0) cy (num-attr el :cy 0)
                     rx (num-attr el :rx 0) ry (num-attr el :ry 0)]
                 (when (and (pos? rx) (pos? ry))
                   (format "M %s %s A %s %s 0 1 0 %s %s A %s %s 0 1 0 %s %s Z"
                           (- cx rx) cy rx ry (+ cx rx) cy rx ry (- cx rx) cy)))
      (:polygon :polyline) (let [n (numbers (:points a))]
                             (when (>= (count n) 6)
                               (str "M " (str/join " " n) " Z")))
      nil)))

;; ── Flattening ───────────────────────────────────────────────────────────

(def ^:const curve-steps 24)

(defn flatten-segments
  "Closed rings [[x y] ...] of desargues.player.svg-path segments, each
   point mapped by the affine `xf`. Curves become `curve-steps` chords;
   every subpath is closed (a filled shape)."
  [segs xf]
  (let [done (fn [rings ring] (if (>= (count ring) 3) (conj rings ring) rings))]
    (loop [segs segs rings [] ring [] [px py] [0.0 0.0]]
      (if-let [[k & v] (first segs)]
        (case k
          :move (recur (rest segs) (done rings ring) [[(nth v 0) (nth v 1)]] [(nth v 0) (nth v 1)])
          :line (recur (rest segs) rings (conj ring [(nth v 0) (nth v 1)]) [(nth v 0) (nth v 1)])
          :quad (let [[x1 y1 x y] v
                      pts (for [i (range 1 (inc curve-steps))
                                :let [t (/ (double i) curve-steps) s (- 1.0 t)]]
                            [(+ (* s s px) (* 2 s t x1) (* t t x))
                             (+ (* s s py) (* 2 s t y1) (* t t y))])]
                  (recur (rest segs) rings (into ring pts) [x y]))
          :cubic (let [[x1 y1 x2 y2 x y] v
                       pts (for [i (range 1 (inc curve-steps))
                                 :let [t (/ (double i) curve-steps) s (- 1.0 t)]]
                             [(+ (* s s s px) (* 3 s s t x1) (* 3 s t t x2) (* t t t x))
                              (+ (* s s s py) (* 3 s s t y1) (* 3 s t t y2) (* t t t y))])]
                   (recur (rest segs) rings (into ring pts) [x y]))
          :close (let [start (first ring)]
                   (recur (rest segs) (done rings ring) [] (or start [px py]))))
        (mapv (fn [r] (mapv (fn [[x y]] (apply-xf xf x y)) r)) (done rings ring))))))

(defn- walk
  "Shape elements of the tree under `el`, with their inherited transform,
   fill and top-level group index: [{:d :xf :fill :group}]."
  [el xf fill group]
  (let [xf (if-let [t (get-in el [:attrs :transform])] (compose xf (parse-transform t)) xf)
        fill (or (style-fill el) fill)
        tag (local-tag el)]
    (cond
      (#{:defs :clipPath :mask :symbol :style :title :desc :metadata} tag) []
      (#{:svg :g :a} tag) (mapcat #(when (map? %) (walk % xf fill group)) (:content el))
      :else (if-let [d (element-d el)] [{:d d :xf xf :fill fill :group group}] []))))

(defn- ring-area ^double [ring]
  (let [n (count ring)]
    (* 0.5 (reduce + (for [i (range n)
                           :let [[x0 y0] (nth ring i) [x1 y1] (nth ring (mod (inc i) n))]]
                       (- (* x0 y1) (* x1 y0)))))))

(defn- centroid
  "Area-weighted centroid of rings (even-odd: signed areas of the rings as
   oriented, so holes drawn the other way subtract)."
  [rings]
  (let [acc (reduce (fn [[ax ay aa] ring]
                      (let [n (count ring)]
                        (reduce (fn [[ax ay aa] i]
                                  (let [[x0 y0] (nth ring i) [x1 y1] (nth ring (mod (inc i) n))
                                        c (- (* x0 y1) (* x1 y0))]
                                    [(+ ax (* c (+ x0 x1))) (+ ay (* c (+ y0 y1))) (+ aa c)]))
                                [ax ay aa] (range n))))
                    [0.0 0.0 0.0] rings)
        [ax ay aa] acc]
    (if (< (Math/abs (double aa)) 1.0e-12)
      (let [pts (apply concat rings) n (double (count pts))]
        [(/ (reduce + (map first pts)) n) (/ (reduce + (map second pts)) n)])
      [(/ ax (* 3.0 aa)) (/ ay (* 3.0 aa))])))

(defn read-svg
  "The parsed XML tree of SVG text."
  [^String text]
  (binding [*read-eval* false]
    (xml/parse (ByteArrayInputStream. (.getBytes text "UTF-8"))
               (fn [s ch]
                 (let [f (javax.xml.parsers.SAXParserFactory/newInstance)]
                   (.setFeature f "http://apache.org/xml/features/nonvalidating/load-external-dtd" false)
                   (.parse (.newSAXParser f) ^java.io.InputStream s ^org.xml.sax.helpers.DefaultHandler ch))))))

(defn shapes
  "The filled shapes of SVG `text`, in document order (later on top), in
   scene units: the art's viewBox (else its bounding box) centred on the
   origin, y up, scaled to `height` scene units tall (default 7.2).
   Each shape: {:id :group :fill :rings [[[x y] ...] ...] :centroid [x y]
   :bbox [x0 y0 x1 y1]}. Unfilled shapes (fill none) are dropped."
  ([text] (shapes text {}))
  ([text {:keys [height] :or {height 7.2}}]
   (let [root (read-svg text)
         tops (filter map? (:content root))
         raw (vec (apply concat
                         (map-indexed (fn [gi el]
                                        (walk el (if-let [t (get-in root [:attrs :transform])]
                                                   (parse-transform t) identity-xf)
                                              (style-fill root) gi))
                                      tops)))
         flat (vec (keep (fn [{:keys [d xf fill group]}]
                           (when-not (= :none fill)
                             (let [rings (flatten-segments (svg-path/segments d) xf)]
                               (when (seq rings)
                                 {:group group :fill (long (or fill 0)) :rings rings}))))
                         raw))
         vb (some-> (get-in root [:attrs :viewBox]) numbers)
         pts (mapcat #(apply concat (:rings %)) flat)
         [x0 y0 w h] (if (= 4 (count vb))
                       vb
                       (let [xs (map first pts) ys (map second pts)
                             x0 (reduce min xs) y0 (reduce min ys)]
                         [x0 y0 (- (reduce max xs) x0) (- (reduce max ys) y0)]))
         k (/ (double height) (max (double w) (double h) 1.0e-9))
         cx (+ x0 (/ w 2.0)) cy (+ y0 (/ h 2.0))
         to-scene (fn [[x y]] [(* k (- x cx)) (* k (- cy y))])]
     (vec (map-indexed
           (fn [i s]
             (let [rings (mapv #(mapv to-scene %) (:rings s))
                   xs (mapcat #(map first %) rings) ys (mapcat #(map second %) rings)]
               (assoc s :id i :rings rings
                      :area (Math/abs (double (reduce + (map ring-area rings))))
                      :centroid (centroid rings)
                      :bbox [(reduce min xs) (reduce min ys) (reduce max xs) (reduce max ys)])))
           flat)))))

;; ── The baked field ──────────────────────────────────────────────────────

(defn- edge-array
  "The edges of `rings` flat: x0 y0 x1 y1, four doubles per edge."
  ^doubles [rings]
  (double-array
   (mapcat (fn [ring]
             (let [n (count ring)]
               (mapcat (fn [i] (let [[x0 y0] (nth ring i) [x1 y1] (nth ring (mod (inc i) n))]
                                 [x0 y0 x1 y1]))
                       (range n))))
           rings)))

(defn signed-distance
  "Signed Euclidean distance from (x, y) to the boundary of the even-odd
   region bounded by `edges` (edge-array): negative inside."
  ^double [^doubles edges ^double x ^double y]
  (let [n (alength edges)]
    (loop [i 0 best Double/MAX_VALUE inside false]
      (if (>= i n)
        (let [d (Math/sqrt best)] (if inside (- d) d))
        (let [ax (aget edges i) ay (aget edges (+ i 1))
              bx (aget edges (+ i 2)) by (aget edges (+ i 3))
              ex (- bx ax) ey (- by ay)
              px (- x ax) py (- y ay)
              ee (+ (* ex ex) (* ey ey))
              h (if (> ee 0.0) (max 0.0 (min 1.0 (/ (+ (* px ex) (* py ey)) ee))) 0.0)
              qx (- px (* h ex)) qy (- py (* h ey))
              d2 (+ (* qx qx) (* qy qy))
              cross (and (not= (> ay y) (> by y))
                         (< x (+ ax (/ (* (- y ay) ex) ey))))]
          (recur (+ i 4) (min best d2) (if cross (not inside) inside)))))))

(defn- par-rows!
  "Runs (f row) for every row in [0, n), split over the processors."
  [^long n f]
  (let [k (.availableProcessors (Runtime/getRuntime))
        chunk (long (Math/ceil (/ (double n) k)))]
    (->> (range 0 n chunk)
         (mapv (fn [^long r0] (future (doseq [r (range r0 (min n (+ r0 chunk)))] (f r)))))
         (run! deref))))

(defn- unit-hash
  "A fixed pseudo-random number in [0, 1) for integer `i`."
  ^double [^long i]
  (.nextDouble (java.util.SplittableRandom. (+ 0x5DEECE66D (* 7919 i)))))

(defn order-keys
  "Per-shape order keys in [0, 1]: :radial (centroid distance from the art
   centre over the largest), :vertical (centroid height, bottom 0),
   :group (top-level <g> index over the count) and :random (hash of id)."
  [shapes]
  (let [cs (map :centroid shapes)
        r (map (fn [[x y]] (Math/hypot x y)) cs)
        rmax (max 1.0e-9 (reduce max 0.0 r))
        ys (map second cs)
        y0 (reduce min 0.0 ys) y1 (reduce max 0.0 ys)
        groups (distinct (map :group shapes))
        gi (zipmap groups (range))
        ng (max 1 (dec (count groups)))]
    {:radial (double-array (map #(/ % rmax) r))
     :vertical (double-array (map #(/ (- % y0) (max 1.0e-9 (- y1 y0))) ys))
     :group (double-array (map #(/ (double (gi (:group %))) ng) shapes))
     :random (double-array (map #(unit-hash (:id %)) shapes))}))

(defn bake
  "The baked field of `shapes`: a grid of `cell` scene units over the art's
   bounding box grown by `margin`, holding per cell the id of the topmost
   shape containing it and that shape's signed distance (negative inside).
   A cell inside no shape holds the nearest shape within `margin` and its
   (positive) distance, or id -1. Each shape's distance is to its own
   boundary, so a shape under another keeps its rings, cut by the one on
   top. Also each shape's inradius (deepest inside distance) and fill.
   Returns {:x0 :y0 :cell :w :h :ids ^ints :dist ^floats :inradius ^doubles
   :fill ^longs :keys (order-keys) :count}."
  ([shapes] (bake shapes {}))
  ([shapes {:keys [cell margin] :or {cell 0.012 margin 0.4}}]
   (let [cell (double cell) margin (double margin)
         [bx0 by0 bx1 by1] (reduce (fn [[a b c d] [e f g h]] [(min a e) (min b f) (max c g) (max d h)])
                                   [Double/MAX_VALUE Double/MAX_VALUE (- Double/MAX_VALUE) (- Double/MAX_VALUE)]
                                   (map :bbox shapes))
         x0 (- (double bx0) margin) y0 (- (double by0) margin)
         w (long (Math/ceil (/ (- (+ (double bx1) margin) x0) cell)))
         h (long (Math/ceil (/ (- (+ (double by1) margin) y0) cell)))
         ids (int-array (* w h) -1)
         dist (float-array (* w h) Float/MAX_VALUE)
         inradius (double-array (count shapes))]
     (doseq [{:keys [id rings bbox]} shapes]
       (let [id (long id)
             edges (edge-array rings)
             [sx0 sy0 sx1 sy1] bbox
             c0 (max 0 (long (Math/floor (/ (- (- (double sx0) margin) x0) cell))))
             c1 (min w (long (Math/ceil (/ (- (+ (double sx1) margin) x0) cell))))
             r0 (max 0 (long (Math/floor (/ (- (- (double sy0) margin) y0) cell))))
             r1 (min h (long (Math/ceil (/ (- (+ (double sy1) margin) y0) cell))))
             deepest (double-array (- r1 r0))]
         (par-rows!
          (- r1 r0)
          (fn [^long dr]
            (let [r (+ r0 dr) y (+ y0 (* cell (+ r 0.5))) row (* r w)]
              (loop [c c0 deep 0.0]
                (if (< c c1)
                  (let [x (+ x0 (* cell (+ c 0.5)))
                        d (signed-distance edges x y)
                        k (+ row c)]
                    (cond
                      (neg? d) (do (aset ids k (int id)) (aset dist k (float d)))
                      (and (< d margin)
                           (or (neg? (aget ids k)) (not (neg? (aget dist k))))
                           (< d (aget dist k)))
                      (do (aset ids k (int id)) (aset dist k (float d))))
                    (recur (inc c) (max deep (- d))))
                  (aset deepest dr deep))))))
         (aset inradius id (max cell (double (reduce max 0.0 deepest))))))
     {:x0 x0 :y0 y0 :cell cell :w w :h h :ids ids :dist dist
      :inradius inradius :fill (long-array (map :fill shapes))
      :keys (order-keys shapes) :count (count shapes)})))

;; ── The ring field ───────────────────────────────────────────────────────

(def defaults
  "The panel: every parameter of the model, at its default.
     :amount      rings per shape, spaced by the shape's own inradius
     :shrink      0..1, the outer fraction of the depth left empty
     :line        ring width as a fraction of the ring spacing
     :fill        brightness between the rings (0 = lines only)
     :motion      :continuous (rings crawl forever) or :bounce (back and forth)
     :speed       ring periods per loop; an integer for a seamless loop
     :phase       ring phase offset
     :palette     :greyscale :rainbow :white or :original (the SVG fill)
     :order       palette order key: :radial :vertical :group or :random
     :key-spread  how much of the palette the order key spans
     :ring-spread how far the palette steps from one ring to the next
     :hue-speed   palette cycles per loop; an integer
     :falloff     0..1, fades the rings toward the centre
     :boost       gain (soft-clipped, so it saturates instead of clipping)
     :keyer       nil or {:width :speed :phase :soft :level :repeat}: an
                  accent band over the order key, :speed an integer
     :scale :offset [x y] :rotate (radians) :spin (turns per loop)
     :background  :black :white or :alpha (offline: black)"
  {:amount 6 :shrink 0.0 :line 0.22 :fill 0.10
   :motion :continuous :speed 1 :phase 0.0
   :palette :rainbow :order :radial :key-spread 0.6 :ring-spread 0.12 :hue-speed 1
   :falloff 0.35 :boost 0.6
   :keyer nil
   :scale 1.0 :offset [0.0 0.0] :rotate 0.0 :spin 0
   :background :black})

(def ^:private ^:const lp-tau (* 2.0 Math/PI))

(defn- frac ^double [^double x] (- x (Math/floor x)))

(defn- smooth ^double [^double e0 ^double e1 ^double x]
  (let [t (max 0.0 (min 1.0 (/ (- x e0) (- e1 e0))))]
    (* t t (- 3.0 (* 2.0 t)))))

(defn- channel ^double [^long c ^long shift]
  (/ (double (bit-and 0xFF (bit-shift-right c shift))) 255.0))

(defn- pack ^long [^double r ^double g ^double b]
  (let [q (fn ^long [^double v] (Math/round (* 255.0 (max 0.0 (min 1.0 v)))))]
    (bit-or (bit-shift-left (q r) 16) (bit-shift-left (q g) 8) (q b))))

(defn- lookup
  "Shape id and signed distance at (x, y) in art units, into `out`
   [id d]: bilinear in d where the four cells share an id, else the
   nearest cell. id -1 off the art."
  [baked ^double x ^double y ^doubles out]
  (let [{:keys [^double x0 ^double y0 ^double cell ^long w ^long h ^ints ids ^floats dist]} baked
        fx (- (/ (- x x0) cell) 0.5) fy (- (/ (- y y0) cell) 0.5)
        c (long (Math/floor fx)) r (long (Math/floor fy))]
    (if (or (< c 0) (< r 0) (>= (inc c) w) (>= (inc r) h))
      (do (aset out 0 -1.0) (aset out 1 1.0e9) out)
      (let [k (+ c (* r w))
            i00 (aget ids k) i10 (aget ids (inc k))
            i01 (aget ids (+ k w)) i11 (aget ids (+ k w 1))
            tx (- fx c) ty (- fy r)]
        (if (and (== i00 i10) (== i00 i01) (== i00 i11))
          (let [d (+ (* (- 1.0 ty) (+ (* (- 1.0 tx) (aget dist k)) (* tx (aget dist (inc k)))))
                     (* ty (+ (* (- 1.0 tx) (aget dist (+ k w))) (* tx (aget dist (+ k w 1))))))]
            (aset out 0 (double i00)) (aset out 1 d) out)
          (let [kk (+ k (if (> tx 0.5) 1 0) (if (> ty 0.5) w 0))]
            (aset out 0 (double (aget ids kk))) (aset out 1 (double (aget dist kk))) out))))))

(defn shader
  "The ring field of `baked` at phase `u` under `params` (merged over
   `defaults`): a primitive fn x y -> 0xRRGGBB in scene units. Every time
   term is speed*u, hue-speed*u, keyer speed*u or spin*u, so with integer
   rates the frame at u = 1 is the frame at u = 0."
  [baked params u]
  (let [p (merge defaults params)
        u (double u)
        amount (double (:amount p))
        shrink (double (:shrink p))
        half-line (* 0.5 (double (:line p)))
        fill (double (:fill p))
        raw-t (+ (* (double (:speed p)) u) (double (:phase p)))
        t (if (= :bounce (:motion p)) (- 1.0 (Math/abs (- (* 2.0 (frac raw-t)) 1.0))) raw-t)
        pal (case (:palette p) :greyscale 0 :rainbow 1 :white 2 :original 3)
        ^doubles okey (get (:keys baked) (:order p))
        key-spread (double (:key-spread p))
        ring-spread (double (:ring-spread p))
        hue (* (double (:hue-speed p)) u)
        falloff (double (:falloff p))
        gain (+ 1.0 (double (:boost p)))
        {kw :width ks :speed kph :phase ksoft :soft klevel :level krep :repeat
         :or {kw 0.15 ks 1 kph 0.0 ksoft 0.08 klevel 0.8 krep 1}} (:keyer p)
        keyer? (some? (:keyer p))
        kc (frac (+ (* (double ks) u) (double kph)))
        kw (* 0.5 (double kw)) ksoft (double ksoft) klevel (double klevel) krep (double krep)
        ^doubles inr (:inradius baked)
        ^longs fills (:fill baked)
        scale (double (:scale p))
        [ox oy] (:offset p) ox (double ox) oy (double oy)
        th (- (+ (double (:rotate p)) (* lp-tau (double (:spin p)) u)))
        ct (Math/cos th) st (Math/sin th)
        bg (case (:background p) :white 1.0 0.0)
        tl (ThreadLocal/withInitial (reify java.util.function.Supplier (get [_] (double-array 2))))]
    (fn ^long [^double x ^double y]
      (let [^doubles out (.get ^ThreadLocal tl)
            qx (/ (- x ox) scale) qy (/ (- y oy) scale)
            ax (- (* ct qx) (* st qy)) ay (+ (* st qx) (* ct qy))
            _ (lookup baked ax ay out)
            id (long (aget out 0))]
        (if (neg? id)
          (pack bg bg bg)
          (let [d (* scale (aget out 1))
                r (* scale (aget inr id))
                spacing (/ (* r (- 1.0 shrink)) amount)
                f (/ (- d) r)
                fs (/ (- f shrink) (- 1.0 shrink))
                s (- (* amount fs) t)
                k (Math/rint s)
                ;; distance to the nearest ring (number k), in scene units
                dr (* spacing (Math/abs (- s k)))
                dr (if (neg? fs) (max dr (* (- shrink f) r)) dr)
                aa (* 0.6 (/ spacing amount))
                line (- 1.0 (smooth (* half-line spacing) (+ (* half-line spacing) (max aa 0.004)) dr))
                inside (smooth 0.0 0.006 (- d))
                body (* inside (if (neg? fs) 0.0 fill))
                v (max line body)
                v (* v (- 1.0 (* falloff (max 0.0 (min 1.0 f)))))
                key (aget okey id)
                arg (+ (* key-spread key) hue (* ring-spread (+ k t)))
                fc (aget fills id)
                cr (case pal
                     0 (+ 0.55 (* 0.45 (Math/cos (* lp-tau arg))))
                     1 (+ 0.5 (* 0.5 (Math/cos (* lp-tau arg))))
                     2 1.0
                     (* (channel fc 16) (+ 0.7 (* 0.3 (Math/cos (* lp-tau arg))))))
                cg (case pal
                     0 cr
                     1 (+ 0.5 (* 0.5 (Math/cos (* lp-tau (+ arg (/ 1.0 3.0))))))
                     2 1.0
                     (* (channel fc 8) (+ 0.7 (* 0.3 (Math/cos (* lp-tau arg))))))
                cb (case pal
                     0 cr
                     1 (+ 0.5 (* 0.5 (Math/cos (* lp-tau (+ arg (/ 2.0 3.0))))))
                     2 1.0
                     (* (channel fc 0) (+ 0.7 (* 0.3 (Math/cos (* lp-tau arg))))))
                accent (if keyer?
                         (let [dk (Math/abs (- (frac (+ (- (* krep key) kc) 0.5)) 0.5))]
                           (* klevel (- 1.0 (smooth kw (+ kw ksoft) dk))))
                         0.0)
                lift (fn ^double [^double c]
                       (let [c (+ (* (- 1.0 accent) c) accent)
                             e (* gain v c)]
                         (- 1.0 (Math/exp (* -1.6 e)))))]
            (if (== bg 1.0)
              (pack (- 1.0 (lift (- 1.0 cr))) (- 1.0 (lift (- 1.0 cg))) (- 1.0 (lift (- 1.0 cb))))
              (pack (lift cr) (lift cg) (lift cb)))))))))
