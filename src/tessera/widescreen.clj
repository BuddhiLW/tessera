(ns tessera.widescreen
  "How each loop fills a frame wider than 16:9 (fosfora's 2560 x 1080).

   A field loop needs nothing: the renderer keeps the vertical scale and
   shows more of the same plane. The loops here lay their content out for
   the 16:9 window (a particle domain, a lattice of objects, a plot axis, a
   box), so each gets a reframe (fn [spec view] spec'): the same loop, its
   layout extended over the wider window `view`. The rules: show more world,
   never stretch or crop the subject; extend by the loop's own symmetry or
   periodicity (a torus tiles, a well mirrors, a lattice gains columns);
   keep the 16:9 content where it was, so the wide frame contains it."
  (:require [desargues.wallpaper.loop :as lp]
            [tessera.chaos :as chaos]
            [tessera.compose.layout :as layout]
            [tessera.compose.scene :as scene]
            [tessera.curves :as curves]
            [tessera.delays :as delays]
            [tessera.flows :as flows]
            [tessera.frame :as frame]
            [tessera.lattice :as lattice]
            [tessera.neon :as neon]
            [tessera.orbitals :as orb]
            [tessera.palette :as pal]
            [tessera.quantum :as quantum]
            [tessera.waves :as waves]))

(set! *warn-on-reflection* true)

(defn- even-floor ^long [^double x]
  (let [n (long (Math/floor x))] (if (odd? n) (dec n) n)))

(defn mirror-x
  "x folded into the window [-w/2, w/2] by reflection at its edges (the
   method of images: a box with walls at the edges continues mirrored)."
  ^double [^double w ^double x]
  (let [t (- (+ 0.5 (/ x w)) (* 2.0 (Math/floor (/ (+ 0.5 (/ x w)) 2.0))))
        t (if (> t 1.0) (- 2.0 t) t)]
    (* w (- t 0.5))))

;; ── Tiling a torus or a periodic street ─────────────────────────────────

(defn cat-map
  "The torus tiles across the whole width (16:9 keeps 7.3: its half-width
   7.11 plus a bead)."
  [spec view]
  (let [hw (+ (frame/half-width view) 0.19)]
    (assoc spec :splat (fn [u] (chaos/cat-rows u {:half-width hw})))))

(defn curl-flow
  "The flow is periodic on the 16:9 torus; its images tile on."
  [spec view]
  (assoc spec :splat (fn [u] (flows/streak-rows @flows/curl-particles u
                                                {:tile flows/view :tail 0.037 :samples 14
                                                 :power 0.42 :window view}))))

(defn vortex-street
  "The street runs on, one more cell each side."
  [spec view]
  (assoc spec :splat (fn [u] (flows/street-splats u view))))

(defn hofstadter
  "The spectrum is periodic in flux with period one: neighbouring
   butterflies continue at the sides."
  [spec _view]
  (assoc spec :field (fn [u] (lattice/hofstadter-shader u true))))

;; ── More of a lattice or a particle domain ──────────────────────────────

(defn delay-pendulums
  "More columns of pivots 0.4 apart, kept 0.2 inside the edge as at 16:9;
   amplitudes normalised by the 16:9 grid's peak, so the old pendulums
   swing exactly as before."
  [spec view]
  (let [hw (frame/half-width view)
        cols (even-floor (+ 1.0 (/ (- hw 0.2) 0.2)))
        peak (reduce max (map (fn [{[x y] :pivot}]
                                (let [[re im] (delays/huygens x y)] (Math/hypot re im)))
                              (delays/pendulum-grid 20 nil)))
        pends (delay (delays/pendulum-grid cols peak))]
    (assoc spec :splat (fn [u] (delays/pendulum-splats u @pends)))))

(defn noise-flow-lines
  "Seeds and the tabulated flow over the wider window, at the 16:9 density."
  [spec view]
  (let [cols (long (Math/round (* 64.0 (/ (double (first view)) 8.0))))
        parts (delay (delays/flow-seeds view cols 36))]
    (assoc spec :splat (fn [u] (delays/flow-line-splats u @parts view)))))

(defn delay-mesh
  "The water-parcel mesh gains columns on both sides; the 16:9 vertices keep
   their rest positions."
  [spec view]
  (let [extra (long (Math/ceil (/ (- (frame/half-width view) 4.0) 0.2)))
        m (delay (delays/mesh-of (+ 48 (* 2 extra)) 30))]
    (assoc spec :splat (fn [u] (delays/mesh-splats u @m)))))

(defn breathers
  "Columns of breathers 3.6 apart out to the edges, the checkerboard of
   phases anchored at the centre."
  [spec view]
  (let [half (Math/round (/ (frame/half-width view) 3.6))
        lattice (waves/breather-lattice-of (* 2 half))]
    (assoc spec :field (fn [u] (waves/breather-shader u lattice)))))

(defn neon-khatam
  "More cells of the tilted plane, beads kept to the wider edge."
  [spec view]
  (let [xmax (+ (frame/half-width view) 1.39)]
    (assoc spec :frame (fn [u] (neon/frame u {:i0 -8 :i1 10 :xmax xmax})))))

(defn neon-pulse
  "More cells of the closer plane, beads kept to the wider edge."
  [spec view]
  (let [xmax (+ (frame/half-width view) 0.744)]
    (assoc spec :frame (fn [u] (neon/pulse-frame u {:i0 -5 :i1 7 :xmax xmax})))))

(defn moore-comets
  "Two more Moore curves outside the pair, 7.2 apart as the pair is; the
   pair keeps its comet phases (offsets 0 and 1/2)."
  [spec _view]
  (let [segs (curves/tiled :moore 5 3.55 [[-3.6 0.0] [-10.8 0.0] [3.6 0.0] [10.8 0.0]])
        p (pal/around (pal/scheme :blue-magenta-orange))]
    (assoc spec :frame (fn [u] (curves/flow-frame segs p u {})))))

(defn kepler-resonance
  "The star field continues into the side strips at its 16:9 density
   (260 stars over 14.4 x 8.2); the system itself stays centred."
  [spec view]
  (let [w (- (+ (frame/half-width view) 0.2) 7.2)
        n (long (Math/round (/ (* 260.0 2.0 w) 14.4)))
        rng (java.util.Random. 8)
        extra (vec (repeatedly n (fn [] [(* (if (.nextBoolean rng) 1.0 -1.0) (+ 7.2 (* w (.nextDouble rng))))
                                         (* 8.2 (- (.nextDouble rng) 0.5))
                                         (+ 0.006 (* 0.016 (Math/pow (.nextDouble rng) 3.0)))
                                         (inc (.nextInt rng 3)) (.nextDouble rng)])))
        frame-fn (:frame spec)]
    (assoc spec :frame
           (fn [u]
             (let [sky (for [[x y r k ph] extra]
                         {:node :dot :at [x y] :opacity (+ 0.25 (* 0.5 (lp/wave (+ (* (long k) (double u)) (double ph)))))
                          :opts {:radius r} :fill {:color "#c9d6ff" :opacity 1.0}})]
               (update (frame-fn u) :shapes #(into (vec sky) %)))))))

;; ── Boxes, plots and set-ups ────────────────────────────────────────────

(defn box-modes
  "The well is the frame: a wider frame is a wider well, the same five modes
   in the box's own coordinates, so the energies and the loop are unchanged."
  [spec view]
  (let [width (double (first view))]
    (assoc spec :field (fn [u] (orb/box-shader u width)))))

(defn quantum-carpet
  "Beyond the walls the carpet continues by the method of images (|psi|^2
   reflected at each wall), so the well keeps its width."
  [spec _view]
  (let [p (pal/scheme :blue-magenta-orange)
        w (/ 128.0 9.0)]
    (assoc spec :field
           (fn [u]
             (let [^clojure.lang.IFn$DDL f (quantum/carpet-shader u {:palette p})]
               (fn ^long [^double x ^double y] (.invokePrim f (mirror-x w x) y)))))))

(defn quantum-tapestry
  "Time runs across: a wider frame shows more of the revival at the same
   time per unit width."
  [spec view]
  (let [span (* 0.22 (/ (double (first view)) (/ 128.0 9.0)))]
    (assoc spec :field (fn [u] (quantum/phase-carpet-shader u {:view view :span span})))))

(defn double-slit
  "The set-up spans the frame: plane-wave run-in, barrier and screen move
   out with the edges (16:9: barrier at -4.5, screen at 4.6), so the
   bar graph ends at the right edge as before."
  [spec view]
  (let [hw (frame/half-width view)]
    (assoc spec :field (fn [u] (quantum/slit-shader u {:k 10.0 :d 2.6 :a 0.3
                                                       :bx (- 2.611 hw) :screen (- hw 2.511)})))))

;; ── Composed scenes ─────────────────────────────────────────────────────

(defn- strip-darts
  "`sites` (the authored set) plus darts thrown over [-w/2, w/2] x [-h/2, h/2]
   outside |x| < hw0, at the same minimum distance from every site, until
   `target` sites in all or the attempts run out."
  [sites seed target distance [w h] hw0]
  (let [rng (java.util.Random. (long seed)) d2 (* (double distance) (double distance))
        w (double w) h (double h) hw0 (double hw0)]
    (loop [pts (vec sites) attempts 0]
      (if (or (>= (count pts) (long target)) (= attempts 40000)) pts
          (let [x (- (* w (.nextDouble rng)) (/ w 2.0))
                y (- (* h (.nextDouble rng)) (/ h 2.0))
                okay? (and (>= (Math/abs x) hw0)
                           (every? (fn [[a b]]
                                     (let [dx (- x (double a)) dy (- y (double b))]
                                       (>= (+ (* dx dx) (* dy dy)) d2))) pts))]
            (recur (if okay? (conj pts [x y]) pts) (inc attempts)))))))

(defmethod layout/sites ::poisson-strips
  ;; The authored Poisson layout, its sites (and their indices) unchanged,
  ;; continued into the side strips of the wider extent at its density.
  [{:keys [base extent count]}]
  (let [authored (layout/sites base)
        hw0 (/ (double (first (:extent base))) 2.0)
        pts (strip-darts (map (juxt :x :y) authored) (inc (long (:seed base)))
                         count (:distance base) extent hw0)]
    (into authored
          (map-indexed (fn [j [x y]] {:i (+ (clojure.core/count authored) j) :x (double x) :y (double y)}))
          (drop (clojure.core/count authored) pts))))

(defn- widen-layout
  "A full-frame site layout (extent at least the 16:9 width) stretched to
   the wider window, its site count scaled with the area; a local layout
   is left alone. A Poisson layout keeps its authored sites where they were
   (re-throwing the darts over the wider extent would move every one, and
   with them the moments their sprites appear) and gains sites only in the
   side strips."
  [{:keys [extent] :as layout} k]
  (if (and extent (>= (double (first extent)) 7.2))
    (let [[w h] extent
          wide-extent [(* k (double w)) h]]
      (if (= :poisson (:type layout))
        {:type ::poisson-strips :base layout :extent wide-extent
         :count (long (Math/round (* k (double (:count layout)))))}
        (cond-> (assoc layout :extent wide-extent)
          (:count layout) (update :count #(long (Math/round (* k (double %))))))))
    layout))

(defn composed
  "A composed scene recompiled with its full-frame layouts widened."
  [spec view]
  (let [k (/ (double (first view)) (double (first (frame/view-of spec))))
        scene' (update (:scene-spec spec) :layers
                       (partial mapv #(cond-> % (:layout %) (update :layout widen-layout k))))]
    (merge spec (scene/->spec scene'))))

(defn- full-frame-layout? [spec]
  (some #(some-> % :layout :extent first double (>= 7.2)) (:layers (:scene-spec spec))))

(def reframes
  "Loop id -> reframe, for every loop laid out for the 16:9 window."
  {:cat-map cat-map :curl-flow curl-flow :vortex-street vortex-street
   :hofstadter hofstadter :delay-pendulums delay-pendulums
   :noise-flow-lines noise-flow-lines :delay-mesh delay-mesh
   :breathers breathers :neon-khatam neon-khatam :neon-pulse neon-pulse
   :moore-comets moore-comets :kepler-resonance kepler-resonance
   :box-modes box-modes :quantum-carpet quantum-carpet
   :quantum-tapestry quantum-tapestry :double-slit double-slit})

(defn attach
  "`loops` (id -> spec) with each listed loop's reframe attached, and every
   composed scene with a full-frame layout reframed by `composed`."
  [loops]
  (into (empty loops)
        (map (fn [[id spec]]
               [id (cond
                     (reframes id) (frame/with-reframe spec (reframes id))
                     (full-frame-layout? spec) (frame/with-reframe spec composed)
                     :else spec)]))
        loops))
