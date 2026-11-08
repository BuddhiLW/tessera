(ns tessera.bz3d
  "First Brillouin zones in 3D, path traced by LuxCore.

     :bz-glass  the first zones of the simple cubic (cube), body-centred cubic
                (rhombic dodecahedron), face-centred cubic (truncated
                octahedron) and hexagonal (hexagonal prism) lattices as glass
                on a dark glossy floor. Edges are thin gold beams, the
                high-symmetry points (Gamma, X, L, K, W, U, ...) small glowing
                beads, and an area lamp orbits once per loop. Solids and camera
                are fixed: only the light moves, and the caustics with it.

   A zone is built from the Bragg planes of its reciprocal lattice: the points
   k with G.k <= |G|^2/2 for every reciprocal-lattice vector G, a half-space
   intersection. Vertices are the feasible triple intersections of planes,
   faces the planes that carry at least three vertices.
   Collect: `bragg-vectors`, `relevant`. Promote: `zone`, `triangles`,
   `edges`, `beam`, `bead`, `orbit`. Boundary: the scene maps, rendered by
   desargues.wallpaper.luxcore."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.luxcore :as lx]))

(set! *warn-on-reflection* true)

(def ^:private eps 1e-7)

;; ── vectors ─────────────────────────────────────────────────────────────

(defn- v+ [a b] (mapv + a b))
(defn- v- [a b] (mapv - a b))
(defn- v* [s a] (mapv #(* (double s) %) a))
(defn- dot ^double [a b] (double (reduce + (map * a b))))
(defn- norm ^double [a] (Math/sqrt (dot a a)))
(defn- unit [a] (v* (/ 1.0 (norm a)) a))

(defn- cross [[a b c] [d e f]]
  [(- (* b f) (* c e)) (- (* c d) (* a f)) (- (* a e) (* b d))])

(defn- centroid [pts] (v* (/ 1.0 (count pts)) (reduce v+ pts)))

(defn- point-key [p] (mapv #(Math/round (* 1e6 (double %))) p))

(defn- dedupe-points [pts] (vals (into (sorted-map) (map (juxt point-key identity)) pts)))

;; ── Collect: the reciprocal lattices ───────────────────────────────────────

(def ^:private s3 (Math/sqrt 3.0))

(def ^:private hex-c
  "Hexagonal c/a, the ideal close-packed ratio sqrt(8/3)."
  (Math/sqrt (/ 8.0 3.0)))

(def lattices
  "Each Bravais lattice by id: its name, the primitive vectors of its
   reciprocal lattice in units of 2pi/a, and its high-symmetry points
   (Setyawan-Curtarolo labels) in the same units."
  {:sc  {:name "simple cubic"
         :reciprocal [[1 0 0] [0 1 0] [0 0 1]]
         :points {:Gamma [0 0 0] :X [0 0.5 0] :M [0.5 0.5 0] :R [0.5 0.5 0.5]}}
   :bcc {:name "body-centred cubic"
         :reciprocal [[0 1 1] [1 0 1] [1 1 0]]
         :points {:Gamma [0 0 0] :H [0 0 1] :N [0.5 0.5 0] :P [0.5 0.5 0.5]}}
   :fcc {:name "face-centred cubic"
         :reciprocal [[-1 1 1] [1 -1 1] [1 1 -1]]
         :points {:Gamma [0 0 0] :X [0 1 0] :L [0.5 0.5 0.5] :W [0.5 1 0]
                  :K [0.75 0.75 0] :U [0.25 1 0.25]}}
   :hex {:name "hexagonal"
         :reciprocal [[1 (/ -1.0 s3) 0] [0 (/ 2.0 s3) 0] [0 0 (/ 1.0 hex-c)]]
         :points (let [a (/ 0.5 hex-c) m [0 (/ 1.0 s3) 0] k [(/ 1.0 3.0) (/ 1.0 s3) 0]]
                   {:Gamma [0 0 0] :A [0 0 a] :M m :K k :L (assoc m 2 a) :H (assoc k 2 a)})}})

(defn bragg-vectors
  "The nonzero reciprocal-lattice vectors n1 b1 + n2 b2 + n3 b3, |ni| <= `r`."
  [basis r]
  (let [ns (range (- r) (inc r))]
    (for [i ns j ns k ns :when (not= 0 i j k)
          :let [[b1 b2 b3] basis]]
      (reduce v+ [(v* i b1) (v* j b2) (v* k b3)]))))

;; ── Promote: half-space intersection ─────────────────────────────────────

(defn- solve3
  "The point k with n_i . k = d_i for three planes [n d], or nil when they
   do not meet in one point (Cramer's rule)."
  [[[n1 d1] [n2 d2] [n3 d3]]]
  (let [c23 (cross n2 n3) det (dot n1 c23)]
    (when (> (Math/abs det) eps)
      (v* (/ 1.0 det) (reduce v+ [(v* d1 c23) (v* d2 (cross n3 n1)) (v* d3 (cross n1 n2))])))))

(defn- inside? [planes k]
  (every? (fn [[n d]] (<= (dot n k) (+ d 1e-6))) planes))

(defn- on-plane? [[n d] k] (< (Math/abs (- (dot n k) d)) 1e-6))

(defn- ordered-face
  "The vertices `vs` of a convex face with outward normal `n`, ordered
   counter-clockwise seen from outside."
  [n vs]
  (let [c (centroid vs)
        e1 (unit (v- (first vs) c))
        e2 (cross (unit n) e1)]
    (vec (sort-by (fn [v] (let [w (v- v c)] (Math/atan2 (dot w e2) (dot w e1)))) vs))))

(defn zone
  "The first Brillouin zone of reciprocal basis `basis`: {:vertices [k ..]
   :faces [{:g G :vertices [i ..]} ..]}, faces counter-clockwise seen from
   outside, each lying on the Bragg plane G.k = |G|^2/2 of its vector G.
   `r` bounds the lattice vectors tried (default 1, enough for these four)."
  ([basis] (zone basis 1))
  ([basis r]
   (let [gs (bragg-vectors basis r)
         planes (mapv (fn [g] [g (/ (dot g g) 2.0)]) gs)
         n (count planes)
         verts (vec (dedupe-points
                     (for [i (range n) j (range (inc i) n) l (range (inc j) n)
                           :let [k (solve3 [(planes i) (planes j) (planes l)])]
                           :when (and k (inside? planes k))]
                       k)))
         index (into {} (map-indexed (fn [i v] [(point-key v) i])) verts)
         faces (for [[g _ :as p] planes
                     :let [on (filter #(on-plane? p %) verts)]
                     :when (>= (count on) 3)]
                 {:g g :vertices (mapv #(index (point-key %)) (ordered-face g on))})]
     {:vertices verts :faces (vec faces)})))

(defn edges
  "The edges of zone `z` as sorted vertex-index pairs, each once."
  [z]
  (vec (sort (into #{} (for [{vs :vertices} (:faces z)
                             [a b] (map vector vs (conj (subvec vs 1) (first vs)))]
                         [(min a b) (max a b)])))))

;; ── Promote: meshes ──────────────────────────────────────────────────────

(defn- place
  "Points `pts` scaled by `s` and moved to `at`."
  [pts s at]
  (mapv #(v+ at (v* s %)) pts))

(defn triangles
  "Zone `z` as a closed triangle mesh {:vertices :faces}, each face fanned
   from its first vertex, scaled by `s` and moved to `at`."
  [z s at]
  {:vertices (place (:vertices z) s at)
   :faces (vec (for [{vs :vertices} (:faces z)
                     i (range 1 (dec (count vs)))]
                 [(vs 0) (vs i) (vs (inc i))]))})

(defn beam
  "A square beam of width `w` from `a` to `b`, lengthened by w/2 at both ends."
  [a b w]
  (let [d (unit (v- b a))
        h (/ w 2.0)
        p (unit (cross d (if (< (Math/abs (double (d 2))) 0.9) [0 0 1] [1 0 0])))
        q (cross d p)
        a (v- a (v* h d)) b (v+ b (v* h d))
        corners (for [[sp sq] [[-1 -1] [1 -1] [1 1] [-1 1]]] (v+ (v* (* sp h) p) (v* (* sq h) q)))]
    {:vertices (vec (concat (map #(v+ a %) corners) (map #(v+ b %) corners)))
     :faces (vec (concat [[0 2 1] [0 3 2] [4 5 6] [4 6 7]]
                         (for [i (range 4) :let [j (mod (inc i) 4)]
                               f [[i j (+ 4 j)] [i (+ 4 j) (+ 4 i)]]]
                           f)))}))

(defn bead
  "An octahedron of radius `r` about `c`."
  [c r]
  {:vertices (vec (for [s [1 -1] i (range 3)] (v+ c (assoc [0 0 0] i (* s r)))))
   :faces [[0 1 2] [1 3 2] [3 4 2] [4 0 2] [1 0 5] [3 1 5] [4 3 5] [0 4 5]]})

;; ── Boundary: scenes ────────────────────────────────────────────────────

(defn gpu
  "Render options: LuxCore PATHOCL on the GPU only (no CPU render threads,
   so the machine stays usable), hybrid back/forward paths for the caustics,
   Intel OIDN denoising and a FIXED linear exposure `scale`, so the frames
   of a loop do not flicker the way per-frame auto exposure would."
  [scale]
  {:engine "PATHOCL"
   :config {"opencl.cpu.use" 0 "opencl.gpu.use" 1 "opencl.native.threads.count" 0
            "path.hybridbackforward.enable" 1
            "film.outputs.2.type" "ALBEDO" "film.outputs.2.filename" "albedo.exr"
            "film.outputs.3.type" "AVG_SHADING_NORMAL" "film.outputs.3.filename" "normal.exr"
            "film.imagepipelines.0.0.type" "INTEL_OIDN"
            "film.imagepipelines.0.1.type" "TONEMAP_LINEAR"
            "film.imagepipelines.0.1.scale" scale
            "film.imagepipelines.0.2.type" "GAMMA_CORRECTION"
            "film.imagepipelines.0.2.value" 2.2}})

(def ^:private point-colours
  "Emission of each high-symmetry label."
  {:Gamma [9 9 9] :X [2 6 14] :L [14 4 2] :W [3 12 4] :K [12 3 10] :U [12 10 2]
   :M [2 6 14] :R [14 4 2] :H [12 3 10] :N [3 12 4] :P [14 4 2] :A [12 10 2]})

(defn- quad [[x y] half z]
  {:vertices [[(- x half) (- y half) z] [(+ x half) (- y half) z]
              [(+ x half) (+ y half) z] [(- x half) (+ y half) z]]
   :faces [[0 2 1] [0 3 2]]})

(defn- radius ^double [z] (reduce max (map norm (:vertices z))))

(defn zone-objects
  "The scene objects of lattice `id`'s zone, scaled to unit circumradius,
   centred over floor point [x y] with its lowest vertex `lift` above the
   floor: a glass solid, gold edge beams and one glowing bead per
   high-symmetry point, every key prefixed by the lattice id."
  [id [x y] lift]
  (let [{:keys [reciprocal points]} (lattices id)
        z (zone reciprocal)
        s (/ 1.0 (radius z))
        at [x y (- lift (* s (reduce min (map #(% 2) (:vertices z)))))]
        vs (place (:vertices z) s at)
        p #(keyword (str (name id) "-" %))]
    (into {(p "glass") (merge {:material :glass} (triangles z s at))
           (p "edges") (merge {:material :gold}
                              (lx/merge-meshes (for [[a b] (edges z)] (beam (vs a) (vs b) 0.022))))}
          (for [[label k] points]
            [(p (name label)) (merge {:material (keyword (str "glow-" (name label)))}
                                     (bead (v+ at (v* s k)) 0.045))]))))

(defn orbit
  "The lamp's centre at phase `u`: once round a circle of radius 4.2, 5.5 up."
  [u]
  (conj (lp/circle u 4.2) 5.5))

(defn bz-glass-scene
  "Four first Brillouin zones in a row (sc, bcc, fcc, hexagonal) as glass at
   phase `u`; only the lamp moves, once round per loop. Options: :spp
   (default 64, denoised) and :exposure (linear tonemap scale)."
  ([u] (bz-glass-scene u {}))
  ([u {:keys [spp exposure] :or {spp 64 exposure 1.0}}]
   (let [ids [:sc :bcc :fcc :hex]
         [lx ly lz] (orbit u)]
     (merge (gpu exposure)
            {:camera {:orig [0 -8.6 3.6] :target [0 0 1.0] :fov 38}
             :materials (merge {:glass {:type "glass" :kr [1 1 1] :kt [1 1 1]
                                        :interiorior 1.52 :cauchyb 0.012}
                                :gold {:type "metal2" :preset "gold" :roughness 0.08}
                                :floor {:type "glossy2" :kd [0.035 0.04 0.055] :ks [0.10 0.10 0.10]
                                        :uroughness 0.015 :vroughness 0.015}
                                :lamp {:type "matte" :kd [0 0 0] :emission [60 56 50]}}
                               (into {} (for [[label c] point-colours]
                                          [(keyword (str "glow-" (name label)))
                                           {:type "matte" :kd [0 0 0] :emission c}])))
             :lights {:amb {:type "constantinfinite" :color [0.012 0.015 0.025]}}
             :objects (merge {:floor (merge {:material :floor} (quad [0 0] 30.0 0.0))
                              :lamp (merge {:material :lamp :camerainvisible true}
                                           (quad [lx ly] 0.5 lz))}
                             (apply merge (map-indexed (fn [i id] (zone-objects id [(* 2.5 (- i 1.5)) 0] 0.15))
                                                       ids)))
             :spp spp}))))

(def bz-glass
  {:id :bz-glass
   :title "First Brillouin zones in glass under an orbiting lamp (LuxCore, GPU)"
   :seconds 10 :fps 24 :size [640 360]
   ;; RTX 4070 laptop GPU, OIDN on: 640x360 ~6 s/frame; 1920x1080 ~24 s/frame
   ;; at 32 spp, so the 240-frame loop takes ~1.6 h at 1080p.
   :scene #(bz-glass-scene % {:spp 32 :exposure 8.0})})

(def loops
  "The path-traced Brillouin-zone loops, by id."
  (array-map :bz-glass bz-glass))
