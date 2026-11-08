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

(defn tiles
  "The star/square polygons of the 4.8.8 khatam pattern covering the square
   of half-side `half` about the origin. Stars (:octagon) carry the full
   8-point khatam rosette; squares are inset towards their centre by `gap`
   (fraction of the half-edge) so each tile is a separate solid with a dark
   joint around it."
  [half gap]
  (let [polys (g/pattern :4.8.8 [(- half) half (- half) half] (g/khatam-angle) 0.0)]
    (mapv (fn [{:keys [kind centre] :as p}]
            {:kind kind
             :centre centre
             :points (if (= kind :octagon)
                       (:rosette p)
                       (mapv (fn [v] (g/v+ centre (g/v* (- 1.0 gap) (g/v- v centre))))
                             (:vertices p)))})
          polys)))

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
  "The khatam tiles in dispersive glass at phase `u`. A single grazing SUN
   (small angular size, so the caustics are sharp) swings its azimuth through
   +/-`swing` radians and returns exactly at u = 1; nothing else lights the
   scene, so the tiles read as dark sapphire and amber against a black floor
   and the coloured caustics carry the picture. The camera looks down at
   about 45 deg from far enough back that 5-7 stars span the frame and the
   4.8.8 pattern stays legible. Options: :spp (default 32), :exposure (linear
   tonemap scale), :elevation/:azimuth/:swing (radians), :gain (sun gain)."
  ([u] (girih-glass-scene u {}))
  ([u {:keys [spp exposure elevation azimuth swing half lift height gain]
       :or {spp 32 exposure 1.0 elevation 0.42 azimuth 2.30 swing 0.44
            half 6.5 lift 0.02 height 0.20 gain 0.5}}]
   (let [dir (sun-dir u elevation azimuth swing)
         objs (tile-objects (tiles half 0.06) lift height 0.86)]
     (merge (gpu exposure)
            {:camera {:orig [0.0 -8.6 8.2] :target [0.0 0.6 0.0] :fov 46
                      :lensradius 0.04 :focaldistance 12.2}
             :materials {:lapis {:type "glass" :kr [1 1 1] :kt [0.55 0.72 1.0]
                                 :interiorior 1.52 :cauchyb 0.020
                                 :volume.interior :cobalt}
                         :amber {:type "glass" :kr [1 1 1] :kt [1.0 0.80 0.45]
                                 :interiorior 1.51 :cauchyb 0.020
                                 :volume.interior :honey}
                         :floor {:type "glossy2" :kd [0.020 0.022 0.028]
                                 :ks [0.04 0.04 0.04]
                                 :uroughness 0.08 :vroughness 0.08}}
             :volumes {:cobalt {:type "clear" :absorption [3.0 1.2 0.15]}
                       :honey {:type "clear" :absorption [0.10 0.9 2.4]}}
             :lights {:key {:type "sun" :dir dir :turbidity 2.2 :relsize 1.4
                            :color [1.0 0.93 0.82] :gain [gain gain gain]}}
             :objects {:floor (merge {:material :floor} (quad [0 0] 60.0 0.0))
                       :stars (assoc (:stars objs) :material :lapis)
                       :squares (assoc (:squares objs) :material :amber)}
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
