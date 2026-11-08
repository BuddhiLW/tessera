(ns tessera.glass
  "Path-traced glass and metal ornament loops (LuxCore PATHOCL + OIDN).

     :girih-glass  the 4.8.8 khatam star pattern as solid dispersive glass
                   tiles -- cobalt-blue stars, amber squares -- standing on a
                   black polished floor. A single grazing SUN of small angular
                   size swings its azimuth by +/-`swing` and returns exactly at
                   u = 1, so sharp dispersive caustics cover the whole floor,
                   crawl, and close the loop. Camera fixed, low grazing angle,
                   thin-lens depth of field; the only light is the sun.

   This namespace only builds scene maps; rendering stays in
   desargues.wallpaper.luxcore. `scene-props`/`render-scene!` below wrap the
   upstream ones to add :volumes (LuxCore scene.volumes.<id>), so the glass
   carries a clear absorption volume and reads as dark sapphire, not frosted
   plastic. They can be dropped once the upstream writer knows :volumes.

   Pure layer: `tiles` (polygons from desargues.wallpaper.girih), `sun-dir`
   (exactly periodic light). Promote: `bevel-prism`, `tile-objects`.
   Boundary: `girih-glass-scene`, `render-scene!`."
  (:require [clojure.java.io :as io]
            [desargues.wallpaper.girih :as g]
            [desargues.wallpaper.luxcore :as lx])
  (:import (java.awt.image BufferedImage)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (javax.imageio ImageIO)))

(set! *warn-on-reflection* true)

;; -- Collect: the khatam tiling --------------------------------------------

(def period
  "The 4.8.8 lattice period (1 + sqrt 2) in scene units (unit edge = 1)."
  (first (first (:lattice (g/tiling :4.8.8)))))

(defn- region-pieces
  "The regions of ONE polygon's Hankin construction, as coloured glass
   pieces. `vs` counter-clockwise, `hk` its g/hankin. Returns
   [{:colour :star|:kite|:accent :centre c :points [..]} ..].

   With D the edge midpoint, E the polygon vertex, S the rosette tip, the
   far corner of the gap quadrilateral is Q = (D+E)/2 + (E-S)/2: at the
   khatam angle the rosette circumradius is exactly half the polygon's, so
   the gap is a KITE with two right angles (this is why the pattern tiles).
   At angles where a polygon's rosette degenerates (the square at 3 pi/8)
   the pieces fall back to the polygon's corners, still a partition."
  [vs hk colour-star colour-kite colour-accent]
  (let [n (count vs)
        m (fn [i] (g/mid (vs (mod i n)) (vs (mod (inc i) n))))
        tip (fn [i] (second ((:strands hk) (mod i n))))
        star (mapcat (fn [[p x _]] [p x]) (:strands hk))
        star-c (g/centroid star)
        degenerate? (or (every? #(< (g/vlen %) 1e-9) (map (fn [[p x _]] (g/v- x p)) (:strands hk)))
                        (< (apply + (map #(g/vlen (g/v- % star-c)) star)) 1e-6))]
    (if degenerate?
      [{:colour colour-accent :centre (g/centroid vs) :points (vec vs)}]
      (into [{:colour colour-star :centre star-c :points (vec star)}]
            (for [i (range n)
                  :let [d (m i) e (vs (mod (inc i) n)) s (tip i) sp (tip (dec i))
                        q (g/v+ (g/v* 0.5 (g/v+ d e)) (g/v* 0.5 (g/v- e s)))
                        qp (g/v+ (g/v* 0.5 (g/v+ d e)) (g/v* 0.5 (g/v- e sp)))]]
              {:colour colour-kite
               :centre (g/centroid [d q e qp])
               :points [d q e qp]})))))

(defn tiles
  "The stained-glass pieces of the 4.8.8 khatam pattern over the square of
   half-side `half`: a partition of the panel into the regions bounded by
   Hankin's strands (8-point stars, the kites between them, the small square
   accents), each tagged with a jewel colour and carrying the strands as the
   gold came. Pieces are shrunk by `s` about their own centre so the came
   shows between them."
  ([half] (tiles half 0.90))
  ([half s]
   (let [polys (g/pattern :4.8.8 [(- half) half (- half) half] (g/khatam-angle) 0.0)]
     (vec
      (for [{:keys [kind vertices] :as p} polys
            :let [hk (g/hankin vertices (g/khatam-angle) 0.0)
                  centre (g/centroid vertices)]
            piece (region-pieces vertices hk
                                 (if (= kind :octagon) :star :accent) :kite :accent)]
        (let [c (:centre piece)
              keep? (and (<= (Math/abs (first centre)) half)
                         (<= (Math/abs (second centre)) half))]
          (assoc piece
                 :keep? keep?
                 :points (mapv (fn [v] (g/v+ c (g/v* s (g/v- v c)))) (:points piece))
                 :strands (when (= :star (:colour piece)) (:strands hk))
                 :cell-centre centre)))))))

;; -- Promote: meshes -------------------------------------------------------

(defn bevel-prism
  "Polygon `pts` extruded from z0 to z1 with the top face inset by fraction
   `b` towards the centroid (a chamfer), so the top edge catches the light.
   Closed mesh {:vertices :faces}; every cap is fanned from a vertex, so
   star-shaped polygons close correctly."
  [pts z0 z1 b]
  (let [n (count pts)
        c (g/centroid pts)
        top (mapv (fn [v] (g/v+ c (g/v* (- 1.0 b) (g/v- v c)))) pts)
        ring (fn [vs z] (mapv (fn [[x y]] [x y z]) vs))
        vs (vec (concat (ring pts z0) (ring pts z1) (ring top z1)))]
    {:vertices vs
     :faces (vec (concat
                  ;; bottom cap (fanned from vertex 0, reversed)
                  (for [i (range 1 (dec n))] [0 (inc i) i])
                  ;; side walls
                  (for [i (range n) :let [j (mod (inc i) n)]
                        f [[i j (+ n j)] [i (+ n j) (+ n i)]]] f)
                  ;; chamfer, z1 -> z1
                  (for [i (range n) :let [j (mod (inc i) n)]
                        f [[(+ n i) (+ n j) (+ (* 2 n) j)]
                           [(+ n i) (+ (* 2 n) j) (+ (* 2 n) i)]]] f)
                  ;; top cap, fanned from the first inset vertex
                  (for [i (range 1 (dec n))]
                    [(* 2 n) (+ (* 2 n) i) (+ (* 2 n) (inc i))])))}))

(defn tile-objects
  "One merged mesh per material for `ts` tiles resting on the floor, each
   lifted `lift`, extruded `height` and scaled by `s` about its own centre,
   so black floor shows in the joints and catches the caustics."
  [ts lift height s]
  (let [shrink (fn [{:keys [centre points]}]
                 (mapv (fn [v] (g/v+ centre (g/v* s (g/v- v centre)))) points))]
    {:stars (lx/merge-meshes
             (for [{:keys [kind] :as t} ts :when (= kind :octagon)]
               (bevel-prism (shrink t) lift (+ lift height) 0.08)))
     :squares (lx/merge-meshes
               (for [{:keys [kind] :as t} ts :when (= kind :square)]
                 (bevel-prism (shrink t) lift (+ lift height) 0.10)))}))

;; -- Promote: the stained-glass panel --------------------------------------

(def tilt
  "The panel leans back by this angle from vertical (radians)."
  (/ Math/PI 6.0))

(def panel-z
  "World z of the panel's mid-height."
  3.4)

(defn panel-point
  "Pattern point [x h] (h = height up the panel) in world coordinates: the
   panel stands at y = 0, its foot at z = 0, leaning back by `tilt`."
  [[x h]]
  [x (* (Math/sin tilt) h) (* (Math/cos tilt) h)])

(defn panel-mesh
  "Extruded 2D mesh `m` (vertices [x y z], the polygon in x-y, thickness in
   z) mapped onto the panel: pattern y becomes height, pattern x stays x,
   thickness goes along the panel normal. The panel leans back by `tilt`
   from vertical, its mid-height at z = `panel-z`."
  [m]
  (let [st (Math/sin tilt) ct (Math/cos tilt)]
    {:vertices (mapv (fn [[x y z]]
                       [x (+ (* st y) (* ct z)) (+ (* ct y) (* st z) panel-z)])
                     (:vertices m))
     :faces (:faces m)}))

(defn panel-objects
  "The stained-glass panel: one merged glass solid per jewel colour plus the
   gold came, in world coordinates. `th` glass thickness (along the panel
   normal), `w` came width. Every piece is a separate closed solid; pieces
   partition the panel, so no two overlap."
  [ts th w]
  (let [ts (filter :keep? ts)
        glass (fn [c] (lx/merge-meshes
                       (for [t ts :when (= c (:colour t))]
                         (lx/prism (:points t) (- (/ th 2.0)) (/ th 2.0)))))]
    {:star (panel-mesh (glass :star))
     :kite (panel-mesh (glass :kite))
     :accent (panel-mesh (glass :accent))
     :came (panel-mesh (lx/ribbons (distinct (mapcat :strands ts)) w (- (/ th 2.0)) (+ (/ th 2.0) 0.01)))}))

;; -- Collect: the emissive backdrop ----------------------------------------

(def ^:private backdrop-warm
  "Emission of the dim parts of the backdrop, warm white."
  [1.0 0.90 0.75])

(defn backdrop
  "The emissive backdrop BEHIND the panel: this is what the glass transmits,
   so it is what makes the jewels glow. `xs` strip centres, `width` strip
   width, `height` total height, `y0` depth behind the panel. Strips stand
   vertically, leaning back with the panel, and face the camera (-y)."
  [xs width height y0 emissions]
  (let [st (Math/sin tilt) ct (Math/cos tilt)
        z0 (- panel-z (* ct (/ height 2.0)))]
    (into {}
          (map (fn [i x em]
                 [(keyword (str "back" i))
                  {:material (keyword (str "backmat" i))
                   :vertices (mapv (fn [[x h]] [x (+ y0 (* st h)) (+ z0 (* ct h))])
                                   [[(- x (/ width 2.0)) 0.0] [(+ x (/ width 2.0)) 0.0]
                                    [(+ x (/ width 2.0)) height] [(- x (/ width 2.0)) height]])
                   :faces [[0 1 2] [0 2 3]]}])
               (range) xs emissions))))

(defn backdrop-materials
  "The material of every backdrop strip."
  [emissions]
  (into {} (map-indexed (fn [i em]
                          [(keyword (str "backmat" i)) {:type "matte" :kd [0 0 0] :emission em}])
                        emissions)))

(defn sweep-emissions
  "Emissions of the `n` backdrop strips at phase `u`: a bright warm band
   `width` strips wide crossing the panel once per loop, superimposed on a
   dim base. A raised-cosine profile, periodic in strip index, so frame N
   equals frame 0 and the sweep reads as light travelling across the glass."
  [u n base peak width]
  (let [centre (* (double u) n)]
    (vec (for [i (range n)]
           (let [d (Math/abs (- i centre))
                 d (min d (- n d))
                 w (Math/exp (- (/ (* d d) (* 2.0 width width))))]
             (mapv (fn [b p] (+ (* base b) (* w (- (* peak p) (* base b)))))
                   backdrop-warm backdrop-warm))))))

;; -- Collect: the periodic lamp path ---------------------------------------

(defn sun-dir
  "The direction TOWARDS the sun at phase `u`: elevation `el`, azimuth
   `az0` + `swing`*sin(2 pi u). Exactly periodic, so the caustics crawl one
   way and return with no seam."
  [u el az0 swing]
  (let [az (+ az0 (* swing (Math/sin (* 2.0 Math/PI (double u)))))]
    [(* (Math/cos el) (Math/cos az)) (* (Math/cos el) (Math/sin az)) (Math/sin el)]))

;; -- Boundary: the scene ---------------------------------------------------

(defn gpu
  "PATHOCL on the GPU only, hybrid back/forward paths for the caustics, OIDN
   denoising and a FIXED linear exposure so frames do not flicker."
  [scale]
  {:engine "PATHOCL"
   :config {"opencl.cpu.use" 0 "opencl.gpu.use" 1 "opencl.native.threads.count" 0
            "path.hybridbackforward.enable" 1
            "path.clamping.variance.maxvalue" 8.0
            "film.imagepipelines.0.0.type" "INTEL_OIDN"
            "film.imagepipelines.0.1.type" "TONEMAP_LINEAR"
            "film.imagepipelines.0.1.scale" scale
            "film.imagepipelines.0.2.type" "GAMMA_CORRECTION"
            "film.imagepipelines.0.2.value" 2.2}})

(defn- quad [[x y] half z]
  {:vertices [[(- x half) (- y half) z] [(+ x half) (- y half) z]
              [(+ x half) (+ y half) z] [(- x half) (+ y half) z]]
   :faces [[0 2 1] [0 3 2]]})

(defn girih-glass-scene
  "STAINED GLASS, backlit. The 4.8.8 khatam panel leans back 30 deg at y = 0,
   its regions coloured glass with gold came. An EMISSIVE BACKDROP just
   behind the panel supplies what the glass transmits -- a warm band
   sweeping across it once per loop -- so the jewels glow and the sweep is
   unmistakable. A small sun behind the panel adds the coloured projection
   on the dark floor in front; its direction swings with integer frequency,
   exactly periodic. The camera sits in front, slightly below the panel's
   mid-height and off axis.

   Options: :spp (default 32), :exposure (linear tonemap scale), :gain (sun
   gain), :half (panel half-width in pattern units), :base/:peak (backdrop
   emission), :strips (backdrop resolution)."
  ([u] (girih-glass-scene u {}))
  ([u {:keys [spp exposure gain half base peak strips th]
       :or {spp 32 exposure 0.02 gain 0.5 half 4.8 base 0.35 peak 3.0
            strips 48 th 0.05}}]
   (let [az (+ Math/PI (* 0.25 (Math/sin (* 2.0 Math/PI (double u)))))
         el (+ 0.42 (* 0.10 (Math/sin (* 4.0 Math/PI (double u)))))
         dir [(* (Math/cos el) (Math/cos az)) (* (Math/cos el) (Math/sin az)) (Math/sin el)]
         ts (tiles half)
         objs (panel-objects ts th 0.045)
         span (+ 4.0 (* 2.0 half))
         width (/ span strips)
         xs (vec (for [i (range strips)] (+ (- (/ span 2.0)) (* width (+ i 0.5)))))
         ems (sweep-emissions u strips base peak 3.0)]
     (merge (gpu exposure)
            {:camera {:orig [1.2 -9.8 3.4] :target [0.0 0.0 3.4] :fov 50
                      :lensradius 0.02 :focaldistance 10.0}
             :materials (merge {:star {:type "glass" :kr [1 1 1] :kt [0.10 0.30 0.95]
                                       :interiorior 1.50 :cauchyb 0.008}
                                :kite {:type "glass" :kr [1 1 1] :kt [0.10 0.70 0.25]
                                       :interiorior 1.50 :cauchyb 0.008}
                                :accent {:type "glass" :kr [1 1 1] :kt [0.95 0.55 0.08]
                                         :interiorior 1.50 :cauchyb 0.008}
                                :gold {:type "metal2" :preset "gold" :roughness 0.18}
                                :floor {:type "glossy2" :kd [0.03 0.03 0.035]
                                        :ks [0.04 0.04 0.04]
                                        :uroughness 0.10 :vroughness 0.10}}
                               (backdrop-materials ems))
             :lights {:key {:type "sun" :dir dir :turbidity 2.2 :relsize 2.0
                            :color [1.0 0.94 0.85] :gain [gain gain gain]}}
             :objects (merge {:floor (merge {:material :floor} (quad [0 0] 60.0 0.0))
                              :stars (assoc (:star objs) :material :star)
                              :kites (assoc (:kite objs) :material :kite)
                              :accents (assoc (:accent objs) :material :accent)
                              :came (assoc (:came objs) :material :gold)}
                             (backdrop xs width (+ 2.0 (* 2.0 half)) 0.35 ems))
             :spp spp}))))

(defn scene-props
  "Upstream lx/scene-props plus the scene's :volumes map, written as
   `scene.volumes.<id>.<key>` (LuxCore clear/heterogeneous volumes)."
  [{:keys [volumes] :as s}]
  (concat (lx/scene-props (dissoc s :volumes))
          (for [[id props] (sort-by (comp str key) volumes)
                [k v] (sort-by (comp str key) props)]
            [(str "scene.volumes." (name id) "." (name k)) v])))

(defn render-scene!
  "Render scene map `s` at `w` x `h` through luxcoreconsole, volumes included;
   the BufferedImage. Same contract as lx/render-scene!."
  ^BufferedImage [s w h]
  (let [dir (.toFile (Files/createTempDirectory "tessera-glass" (make-array FileAttribute 0)))
        png (io/file dir "frame.png")]
    (try
      (spit (io/file dir "scene.scn") (lx/props->text (scene-props s)))
      (spit (io/file dir "render.cfg") (lx/props->text (lx/config-props s w h (.getPath png))))
      (let [^"[Ljava.lang.String;" argv (into-array String [(lx/console) "render.cfg"])
            proc (.start (doto (ProcessBuilder. argv)
                           (.directory dir)
                           (.redirectErrorStream true)))
            log (slurp (.getInputStream proc))
            code (.waitFor proc)]
        (when-not (and (zero? code) (.exists png))
          (throw (ex-info (str "luxcoreconsole failed (" code ")")
                          {:exit code :dir (.getPath dir)
                           :log (subs log (max 0 (- (count log) 2500)))})))
        (ImageIO/read png))
      (finally
        (doseq [^java.io.File f (reverse (file-seq dir))] (.delete f))))))

(def girih-glass
  {:id :girih-glass
   :title "Khatam stars in dispersive glass, caustics crawling (LuxCore, GPU)"
   :seconds 10 :fps 24 :size [640 360]
   :scene #(girih-glass-scene % {:spp 32 :exposure 4.0})})

(def loops
  "The path-traced glass ornament loops, by id."
  (array-map :girih-glass girih-glass))
