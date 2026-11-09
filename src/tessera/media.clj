(ns tessera.media
  "The media pack: every tessera loop as a browsable clip for fosfora's
   Catalog. The format is the hive decision 20261108184723-2a511904 and
   docs/media.md:

     out/media/media.json, media.edn   one manifest, {:version 1 :items [...]}
     out/media/<id>/clip-hd.mp4        1920x1080, hardlinked from out/hd
     out/media/<id>/clip-uw.mp4        2560x1080, hardlinked from out/uw
     out/media/<id>/thumb.webp         480x270 still
     out/media/<id>/preview.webp       ~3 s animated, 320x180, 12 fps

   `catalog` is pure: the registry plus the files present give the items.
   `emit-pack!` does the I/O, skipping every output newer than its source,
   so re-running it picks up clips rendered since."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [tessera.loops :as loops])
  (:import (java.io File)
           (java.nio.file Files LinkOption StandardCopyOption)))

(set! *warn-on-reflection* true)

(def version 1)

(def family-set
  "The allowed families, in the order the Catalog should show them."
  ["Orbits" "Curves" "Knots" "Fractals" "Lattices" "Waves" "Quantum" "Tilings" "Ornament" "Flows" "Scenes"])

(def families
  "Hand-curated: loop id -> [family tags]. Must cover every loop in
   tessera.loops/catalog (the tests check it)."
  {;; Orbits: celestial and mechanical periodic motion
   :choreographies ["Orbits" ["three-body" "figure-eight" "n-body" "choreography"]]
   :figure-eight ["Orbits" ["three-body" "figure-eight" "n-body" "choreography"]]
   :phase-flow ["Orbits" ["pendulum" "phase-space" "action-angle" "hamiltonian"]]
   :kepler-resonance ["Orbits" ["kepler" "resonance" "ellipse" "celestial"]]
   :pendulum-wave ["Orbits" ["pendulum" "elliptic" "resonance" "mechanics"]]
   :delay-pendulums ["Orbits" ["pendulum" "delay" "phase" "mechanics"]]
   :elliptic-billiard ["Orbits" ["billiard" "poncelet" "caustic" "ellipse"]]
   :standard-map ["Orbits" ["chaos" "kam" "standard-map" "hamiltonian"]]
   :cat-map ["Orbits" ["chaos" "torus" "arnold" "recurrence"]]

   ;; Curves: parametric figures and space-filling curves
   :lissajous-3d ["Curves" ["lissajous" "3d" "comets" "harmonic"]]
   :lissajous-oscillators ["Curves" ["lissajous" "3d" "oscillator" "harmonic"]]
   :moore-comets ["Curves" ["space-filling" "moore" "comets" "hilbert"]]
   :harmonograph ["Curves" ["harmonograph" "pendulum" "damped" "lissajous" "comets"]]
   :spirograph ["Curves" ["spirograph" "hypotrochoid" "roulette" "symmetry" "comets"]]

   ;; Knots: topology in 3D
   :torus-knots ["Knots" ["torus-knot" "3d" "neon" "topology"]]
   :hopf-fibration ["Knots" ["hopf" "fibration" "torus" "topology" "3d"]]
   :knot-tubes ["Knots" ["trefoil" "figure-eight-knot" "tube" "neon" "symmetry" "3d"]]

   ;; Fractals: self-similar sets and zooms
   :julia-orbit ["Fractals" ["julia" "complex" "escape-time"]]
   :sierpinski-zoom ["Fractals" ["sierpinski" "zoom" "self-similar"]]
   :hofstadter ["Fractals" ["hofstadter" "butterfly" "magnetic-flux" "spectrum"]]
   :flower-mandala ["Fractals" ["dyadic" "labyrinth" "tunnel" "mandala"]]
   :hyperspace ["Fractals" ["dyadic" "log-polar" "warp" "kaleidoscope"]]
   :mandelbulb ["Fractals" ["mandelbulb" "3d" "raymarching" "distance-estimator" "seven-fold"]]
   :quaternion-julia ["Fractals" ["julia" "quaternion" "3d" "raymarching" "distance-estimator"]]
   :menger-flight ["Fractals" ["menger" "sponge" "fly-through" "3d" "raymarching" "tunnel"]]
   :cardioid-julia ["Fractals" ["julia" "mandelbrot" "cardioid" "complex" "escape-time"]]
   :apollonian ["Fractals" ["apollonian" "gasket" "inversion" "moebius" "circles"]]

   ;; Lattices: band structure, Fermi surfaces, Brillouin zones
   :graphene-fermi ["Lattices" ["graphene" "fermi-surface" "dirac" "hexagonal"]]
   :graphene-zones ["Lattices" ["graphene" "brillouin" "fermi-surface" "van-hove"]]
   :kagome-fermi ["Lattices" ["kagome" "fermi-surface" "triangular"]]
   :twisted-bilayer ["Lattices" ["graphene" "moire" "twisted-bilayer"]]
   :square-fermi ["Lattices" ["square" "fermi-surface" "four-fold"]]
   :lieb-fermi ["Lattices" ["lieb" "fermi-surface" "flat-band"]]
   :bloch-oscillation ["Lattices" ["bloch" "wave-packet" "square" "transport"]]
   :brillouin-zones ["Lattices" ["brillouin" "hexagonal" "stained-glass"]]
   :brillouin-square ["Lattices" ["brillouin" "square" "stained-glass"]]
   :brillouin-oblique ["Lattices" ["brillouin" "oblique" "stained-glass"]]
   :brillouin-rect ["Lattices" ["brillouin" "rectangular" "stained-glass"]]
   :bz-glass ["Lattices" ["brillouin" "glass" "path-traced" "luxcore" "3d"]]
   :sprite-lattice ["Lattices" ["graphene" "glyph" "uv" "band"]]

   ;; Waves: interference, membranes, solitons, water
   :caustics ["Waves" ["caustics" "water" "snell" "refraction"]]
   :caustics-gold ["Waves" ["caustics" "gold" "snell" "refraction"]]
   :breathers ["Waves" ["sine-gordon" "breather" "soliton"]]
   :drum ["Waves" ["membrane" "chladni" "modes" "resonance"]]
   :delay-mesh ["Waves" ["gerstner" "ocean" "mesh" "delay"]]
   :quasicrystal ["Waves" ["plane-waves" "quasicrystal" "interference"]]

   ;; Quantum: wavefunctions and orbitals
   :quantum-tapestry ["Quantum" ["talbot" "carpet" "phase-colour"]]
   :quantum-carpet ["Quantum" ["talbot" "carpet" "revival" "particle-in-a-box"]]
   :double-slit ["Quantum" ["double-slit" "interference" "diffraction"]]
   :hydrogen-beat ["Quantum" ["hydrogen" "orbitals" "superposition"]]
   :box-modes ["Quantum" ["eigenstates" "particle-in-a-box" "phase-colour"]]

   ;; Tilings: girih, quasiperiodic tilings, kaleidoscopes
   :girih-light ["Tilings" ["girih" "khatam" "4.8.8" "islamic"]]
   :girih-drift ["Tilings" ["girih" "3.6.3.6" "islamic"]]
   :girih-assembly ["Tilings" ["girih" "khatam" "4.8.8" "assembly"]]
   :girih-gold ["Tilings" ["girih" "gold" "path-traced" "luxcore" "3d"]]
   :neon-pulse ["Tilings" ["girih" "neon" "rosette" "bloom"]]
   :neon-khatam ["Tilings" ["girih" "khatam" "neon" "tilt-shift"]]
   :penrose-fermi ["Tilings" ["penrose" "quasicrystal" "five-fold"]]
   :heptagonal-fermi ["Tilings" ["quasicrystal" "seven-fold"]]
   :octagonal-fermi ["Tilings" ["ammann-beenker" "quasicrystal" "eight-fold"]]
   :fold-9 ["Tilings" ["quasiperiodic" "nine-fold" "contours"]]
   :fold-10 ["Tilings" ["quasiperiodic" "ten-fold" "contours"]]
   :fold-11 ["Tilings" ["quasiperiodic" "eleven-fold" "contours"]]
   :fold-12 ["Tilings" ["quasiperiodic" "twelve-fold" "contours"]]
   :kaleido-quasicrystal ["Tilings" ["kaleidoscope" "quasicrystal" "uv" "eight-fold"]]

   ;; Ornament: SVG art drawn as concentric offset outlines (tessera.svg-offset)
   :offset-rosette ["Ornament" ["svg-offset" "rosette" "rings" "rainbow" "eight-fold"]]
   :offset-keyer ["Ornament" ["svg-offset" "outline" "keyer" "greyscale"]]
   :offset-bounce ["Ornament" ["svg-offset" "rings" "bounce" "original-colours"]]

   ;; Flows: fluids and vector fields
   :curl-flow ["Flows" ["curl-noise" "dye" "eddies"]]
   :vortex-street ["Flows" ["von-karman" "vortex" "dye"]]
   :noise-flow-lines ["Flows" ["streamlines" "noise" "vorticity"]]

   ;; Scenes: composed VJ scenes
   :textile-tunnel ["Scenes" ["tunnel" "uv" "textile"]]
   :fabric ["Scenes" ["uv" "relief" "ribbons" "droplets"]]
   :temple ["Scenes" ["kaleidoscope" "pyramids" "quasicrystal" "droplets"]]
   :serpent-garden ["Scenes" ["serpents" "glyphs" "graphene" "droplets"]]
   :mandala-tunnel ["Scenes" ["tunnel" "mandala" "beads"]]
   :generated-2 ["Scenes" ["generated" "kaleidoscope" "eight-fold" "ice"]]
   :generated-3 ["Scenes" ["generated" "tunnel" "twelve-fold" "ember"]]
   :fourier-trefoil ["Curves" ["fourier" "epicycles" "trefoil" "harmonic"]]
   :fourier-space ["Curves" ["fourier" "trefoil" "3d" "spheres" "rods"]]
   :fourier-formula ["Curves" ["fourier" "tex" "gauss-law" "epicycles"]]
   :fourier-live ["Curves" ["fourier" "trefoil" "sdf" "live" "analytic"]]})

(def titles
  "Short human titles for the Catalog tile. The loop's own :title (often a
   sentence) becomes the item's :description."
  {:choreographies "Three-Body Choreographies"
   :figure-eight "Figure-Eight Three-Body Orbit"
   :phase-flow "Pendulum Phase Flow"
   :kepler-resonance "Kepler Resonance"
   :pendulum-wave "Pendulum Wave"
   :delay-pendulums "Delay Pendulums"
   :elliptic-billiard "Elliptic Billiard"
   :standard-map "Chirikov Standard Map"
   :cat-map "Arnold's Cat Map"
   :lissajous-3d "Lissajous Lattice"
   :lissajous-oscillators "Lissajous Oscillators"
   :moore-comets "Moore Curve Comets"
   :harmonograph "Harmonograph"
   :spirograph "Spirograph"
   :torus-knots "Torus Knot Garden"
   :hopf-fibration "Hopf Fibration"
   :knot-tubes "Trefoil and Figure-Eight Knots"
   :julia-orbit "Julia Set Orbit"
   :sierpinski-zoom "Sierpinski Zoom"
   :hofstadter "Hofstadter Butterfly"
   :flower-mandala "Flower Mandala"
   :hyperspace "Hyperspace"
   :mandelbulb "Mandelbulb"
   :quaternion-julia "Quaternion Julia Set"
   :menger-flight "Menger Sponge Flight"
   :cardioid-julia "Julia Sets Along the Cardioid"
   :apollonian "Apollonian Gasket"
   :graphene-fermi "Graphene Fermi Contours"
   :graphene-zones "Graphene Brillouin Zones"
   :kagome-fermi "Kagome Fermi Contours"
   :twisted-bilayer "Twisted Bilayer Graphene"
   :square-fermi "Square Lattice Fermi Contours"
   :lieb-fermi "Lieb Lattice Fermi Contours"
   :bloch-oscillation "Bloch Oscillations"
   :brillouin-zones "Brillouin Zones: Hexagonal"
   :brillouin-square "Brillouin Zones: Square"
   :brillouin-oblique "Brillouin Zones: Oblique"
   :brillouin-rect "Brillouin Zones: Rectangular"
   :bz-glass "Brillouin Zones in Glass"
   :sprite-lattice "Sprite Lattice"
   :caustics "Water Caustics"
   :caustics-gold "Gold Caustics"
   :breathers "Sine-Gordon Breathers"
   :drum "Chladni Drum"
   :delay-mesh "Gerstner Sea"
   :quasicrystal "Seven-Wave Quasicrystal"
   :quantum-tapestry "Quantum Tapestry"
   :quantum-carpet "Quantum Carpet"
   :double-slit "Double Slit"
   :hydrogen-beat "Hydrogen Orbital Beat"
   :box-modes "Particle-in-a-Box Modes"
   :girih-light "Khatam Stars Under Light"
   :girih-drift "Drifting Girih"
   :girih-assembly "Girih Assembly"
   :girih-gold "Gold Girih"
   :neon-pulse "Neon Pulse"
   :neon-khatam "Neon Khatam"
   :penrose-fermi "Five-Fold Quasicrystal"
   :heptagonal-fermi "Seven-Fold Quasicrystal"
   :octagonal-fermi "Eight-Fold Quasicrystal"
   :fold-9 "Nine-Fold Contours"
   :fold-10 "Ten-Fold Contours"
   :fold-11 "Eleven-Fold Contours"
   :fold-12 "Twelve-Fold Contours"
   :kaleido-quasicrystal "Quasicrystal Kaleidoscope"
   :offset-rosette "Offset Rosette"
   :offset-keyer "Offset Outlines, Keyed"
   :offset-bounce "Breathing Offset Rings"
   :curl-flow "Curl Flow"
   :vortex-street "Von Kármán Vortex Street"
   :noise-flow-lines "Noise Flow Lines"
   :textile-tunnel "Textile Tunnel"
   :fabric "UV Fabric"
   :temple "Temple"
   :serpent-garden "Serpent Garden"
   :mandala-tunnel "Mandala Tunnel"
   :generated-2 "Generated Scene: Ice Fold"
   :generated-3 "Generated Scene: Ember Tunnel"
   :fourier-trefoil "Fourier Trefoil"
   :fourier-space "Fourier Spheres on Rods"
   :fourier-formula "Fourier Draws Gauss's Law"
   :fourier-live "Analytic Fourier Epicycles"})

(def clip-dirs
  "Clip kind -> render directory under the repo root."
  {:hd "out/hd" :uw "out/uw"})

(defn- rel [id file] (str (name id) "/" file))

(defn item
  "The manifest item of `loop`, given `present`: the set of clip kinds
   (:hd, :uw) and still kinds (:thumb, :preview) that exist for it."
  [loop present]
  (let [id (:id loop)
        [family tags] (get families id ["Scenes" []])]
    (cond-> {:id (name id)
             :title (get titles id (:title loop))
             :description (:title loop)
             :family family
             :tags tags
             :seconds (:seconds loop)
             :fps (:fps loop)
             :loop true
             :clips (into (sorted-map)
                          (for [k [:hd :uw] :when (present k)]
                            [k (rel id (str "clip-" (name k) ".mp4"))]))
             :source (str "tessera.loops/" (name id))
             :author "tessera"}
      (present :thumb) (assoc :thumb (rel id "thumb.webp"))
      (present :preview) (assoc :preview (rel id "preview.webp")))))

(defn catalog
  "Pure: the manifest `{:version 1 :items [...]}` of `registry` (id -> loop,
   default tessera.loops/catalog). `present-fn` maps an id to the set of
   media kinds present; by default every loop is assumed to have
   everything (use emit-pack! for the real, file-backed manifest)."
  ([] (catalog loops/catalog))
  ([registry] (catalog registry (constantly #{:hd :uw :thumb :preview})))
  ([registry present-fn]
   {:version version
    :items (vec (for [[id loop] registry] (item loop (present-fn id))))}))

;; ---------------------------------------------------------------- I/O

(defn- file ^File [root & parts] (apply io/file root parts))

(defn- fresh?
  "True when `out` exists and is at least as new as `src`."
  [^File src ^File out]
  (and (.exists out) (>= (.lastModified out) (.lastModified src))))

(defn- ffmpeg! [& args]
  (let [{:keys [exit err]} (apply sh/sh "timeout" "300" "ffmpeg" "-v" "error" "-y" (map str args))]
    (when-not (zero? exit)
      (throw (ex-info (str "ffmpeg failed: " (str/trim (str err))) {:args args :exit exit})))
    true))

(defn- duration
  "Seconds of the video `f`, via ffprobe; nil when unknown."
  [^File f]
  (let [{:keys [exit out]} (sh/sh "timeout" "60" "ffprobe" "-v" "error" "-show_entries" "format=duration"
                                  "-of" "default=nw=1:nk=1" (str f))]
    (when (zero? exit) (parse-double (str/trim out)))))

(defn- link-or-copy!
  "Hardlink `src` to `out` (fresh copy on another filesystem). Replaces a
   stale `out`. Returns :linked, :copied or :fresh."
  [^File src ^File out]
  (if (fresh? src out)
    :fresh
    (let [sp (.toPath src) op (.toPath out)]
      (io/make-parents out)
      (Files/deleteIfExists op)
      (try (Files/createLink op sp) :linked
           (catch Exception _
             (Files/copy sp op ^"[Ljava.nio.file.CopyOption;"
                         (into-array java.nio.file.CopyOption [StandardCopyOption/COPY_ATTRIBUTES]))
             :copied)))))

(def ^:private fill-16x9
  (fn [w h] (format "scale=%d:%d:force_original_aspect_ratio=increase,crop=%d:%d,setsar=1" w h w h)))

(defn- tmp-of ^File [^File out]
  (File. (.getParentFile out) (str "." (.getName out) ".tmp.webp")))

(defn- commit! [^File tmp ^File out]
  (Files/move (.toPath tmp) (.toPath out)
              ^"[Ljava.nio.file.CopyOption;" (into-array java.nio.file.CopyOption [StandardCopyOption/REPLACE_EXISTING]))
  out)

(defn- thumb-from-clip! [^File clip ^File out]
  (let [mid (/ (or (duration clip) 2.0) 2.0) tmp (tmp-of out)]
    (ffmpeg! "-ss" (format "%.3f" mid) "-i" clip "-frames:v" "1" "-vf" (fill-16x9 480 270)
             "-c:v" "libwebp" "-quality" "85" "-f" "webp" tmp)
    (commit! tmp out)))

(defn- thumb-from-still! [^File still ^File out]
  (let [tmp (tmp-of out)]
    (ffmpeg! "-i" still "-frames:v" "1" "-vf" (fill-16x9 480 270)
             "-c:v" "libwebp" "-quality" "85" "-f" "webp" tmp)
    (commit! tmp out)))

(defn- preview! [^File clip ^File out]
  (let [d (or (duration clip) 3.0)
        start (max 0.0 (- (/ d 2.0) 1.5))
        tmp (tmp-of out)]
    (ffmpeg! "-ss" (format "%.3f" start) "-t" "3" "-i" clip "-an"
             "-vf" (str "fps=12," (fill-16x9 320 180))
             "-c:v" "libwebp_anim" "-loop" "0" "-quality" "60" "-f" "webp" tmp)
    (commit! tmp out)))

(defn- emit-item!
  "Brings out/media/<id>/ up to date for one loop. Returns
   {:present #{...} :wrote [...] :errors [...]}."
  [root media id]
  (let [dir (file media (name id))
        clips (into {} (for [[k d] clip-dirs
                             :let [src (file root d (str (name id) ".mp4"))]
                             ;; a render still being written has no moov atom yet:
                             ;; ffprobe cannot read it, so it waits for the next run
                             :when (and (.isFile src) (duration src))]
                         [k src]))
        log (atom {:present #{} :wrote [] :errors []})
        step! (fn [kind ^File out thunk]
                (try (let [r (thunk)]
                       (swap! log update :present conj kind)
                       (when-not (= r :fresh) (swap! log update :wrote conj (str (name id) "/" (.getName out)))))
                     (catch Exception e
                       (swap! log update :errors conj {:id id :kind kind :error (ex-message e)}))))]
    ;; clips
    (doseq [[k src] clips
            :let [out (file dir (str "clip-" (name k) ".mp4"))]]
      (step! k out #(link-or-copy! src out)))
    ;; drop clip links whose source has gone
    (doseq [k (keys clip-dirs)
            :let [out (file dir (str "clip-" (name k) ".mp4"))]
            :when (and (not (clips k)) (.exists out))]
      (.delete out))
    (let [clip (or (:hd clips) (:uw clips))
          still (file root "docs" "gallery" (str (name id) ".webp"))
          thumb (file dir "thumb.webp")
          prev (file dir "preview.webp")]
      (io/make-parents thumb)
      (cond
        clip (step! :thumb thumb #(if (fresh? clip thumb) :fresh (thumb-from-clip! clip thumb)))
        (.isFile still) (step! :thumb thumb #(if (fresh? still thumb) :fresh (thumb-from-still! still thumb)))
        (.isFile thumb) (swap! log update :present conj :thumb))
      (if clip
        (step! :preview prev #(if (fresh? clip prev) :fresh (preview! clip prev)))
        (when (.exists prev) (.delete prev))))
    @log))

(defn- write-manifest! [^File media manifest]
  (let [json-out (file media "media.json")
        edn-out (file media "media.edn")
        js (json/write-str manifest :indent true :escape-slash false :escape-unicode false)
        ed (with-out-str (binding [*print-namespace-maps* false] (pp/pprint manifest)))]
    ;; rewrite only on change, so an idle run leaves mtimes alone
    (doseq [[^File f s] [[json-out (str js "\n")] [edn-out ed]]]
      (when-not (and (.exists f) (= s (slurp f)))
        (spit f s)))))

(defn emit-pack!
  "Writes out/media/ under `:root` (default: the current directory) for
   every loop in `:registry` (default tessera.loops/catalog). Idempotent:
   outputs newer than their sources are skipped. Returns the manifest and
   counts {:items :hd :uw :thumbs :previews :wrote :errors}."
  ([] (emit-pack! {}))
  ([{:keys [root registry] :or {root "." registry loops/catalog}}]
   (let [root (.getCanonicalFile (io/file root))
         media (file root "out" "media")
         _ (.mkdirs media)
         results (into {} (for [id (keys registry)] [id (emit-item! root media id)]))
         manifest (catalog registry #(get-in results [% :present]))
         items (:items manifest)]
     (write-manifest! media manifest)
     {:manifest (str (file media "media.json"))
      :counts {:items (count items)
               :hd (count (filter #(get-in % [:clips :hd]) items))
               :uw (count (filter #(get-in % [:clips :uw]) items))
               :thumbs (count (filter :thumb items))
               :previews (count (filter :preview items))
               :wrote (reduce + (map (comp count :wrote) (vals results)))}
      :errors (vec (mapcat :errors (vals results)))})))

(defn -main
  "clojure -M:media [root]"
  [& [root]]
  (let [r (emit-pack! (cond-> {} root (assoc :root root)))]
    (pp/pprint (select-keys r [:manifest :counts :errors]))
    (shutdown-agents)
    (System/exit (if (seq (:errors r)) 1 0))))
