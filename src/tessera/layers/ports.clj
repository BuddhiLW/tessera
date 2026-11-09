(ns tessera.layers.ports
  "The second wave of live field layers: closure loops ported to desargues
   field forms, each pixel-exact against its original shader at the 8-bit
   output (tessera.layers.ports-test). tessera.layers/layers appends
   `layers` after its first ten. Same contract as there: :params is an
   ordered, append-only vector of [symbol spec] pairs (the slot ABI),
   loop_rate is slot 0, and `effects` names are fosfora preset keys, never
   renamed. The forms live in tessera.layers.ports.* by family; the
   builders and the DSL workarounds are in tessera.layers.ports.forms."
  (:require [tessera.layers.ports.breathers :as breathers]
            [tessera.layers.ports.brillouin :as bz]
            [tessera.layers.ports.carpet :as carpet]
            [tessera.layers.ports.modes :as modes]
            [tessera.layers.ports.reciprocal :as recip]
            [tessera.layers.ports.slit :as slit]
            [tessera.layers.ports.uv :as uv]
            [tessera.projective :as projective]))

(def layers
  "Ported layers in stable registry order (best visual payoff first)."
  (into [recip/twisted-bilayer slit/double-slit carpet/quantum-tapestry carpet/quantum-carpet
         modes/box-modes modes/drum breathers/breathers
         recip/fold-9 recip/fold-10 recip/fold-11 recip/fold-12
         bz/brillouin-square bz/brillouin-oblique bz/brillouin-rect
         uv/textile-tunnel uv/flower-mandala uv/hyperspace uv/sprite-lattice]
        projective/layers))

(def source-loops
  "Layer id -> the tessera.loops/all id of the closure loop it reproduces."
  (merge
   {:twisted_bilayer :twisted-bilayer :double_slit :double-slit
    :quantum_tapestry :quantum-tapestry :quantum_carpet :quantum-carpet
    :box_modes :box-modes :drum_modes :drum :breathers :breathers
    :fold_9 :fold-9 :fold_10 :fold-10 :fold_11 :fold-11 :fold_12 :fold-12
    :brillouin_square :brillouin-square :brillouin_oblique :brillouin-oblique
    :brillouin_rect :brillouin-rect
    :textile_tunnel :textile-tunnel :flower_mandala :flower-mandala
    :hyperspace :hyperspace :sprite_lattice :sprite-lattice}
   (into {} (map (fn [{:keys [id]}] [id (keyword (clojure.string/replace (name id) "_" "-"))]))
         projective/layers)))

(def effects
  "fosfora layer-effect catalog name (a preset key: STABLE) and description
   per layer id."
  (merge
   {:twisted_bilayer {:name "Twisted Bilayer" :description "Twisted bilayer graphene: the moire superlattice with light pulsing outward."}
    :double_slit {:name "Double Slit" :description "A plane wave through two slits; the fringe intensity stands out of the screen."}
    :quantum_tapestry {:name "Quantum Tapestry" :description "The Talbot carpet of a packet in a box, in phase colour, time flowing across."}
    :quantum_carpet {:name "Quantum Carpet" :description "A packet in a box revives exactly: the Talbot carpet scrolling once per loop."}
    :box_modes {:name "Box Modes" :description "Five commensurate eigenstates of a 16 by 9 well, phase colour and iso-density contours."}
    :drum_modes {:name "Drum Modes" :description "Pythagorean square-membrane modes: luminous Chladni nodal lines in a 1:2:3:4 resonance."}
    :breathers {:name "Sine-Gordon Breathers" :description "Glowing level sets of an exact sine-Gordon breather lattice."}
    :fold_9 {:name "Nine-Fold Contours" :description "Nine-fold quasiperiodic energy contours, rising."}
    :fold_10 {:name "Ten-Fold Contours" :description "Ten-fold quasiperiodic energy contours, rising."}
    :fold_11 {:name "Eleven-Fold Contours" :description "Eleven-fold quasiperiodic energy contours, rising."}
    :fold_12 {:name "Twelve-Fold Contours" :description "Twelve-fold quasiperiodic energy contours, rising."}
    :brillouin_square {:name "Brillouin Square" :description "Brillouin zones of the square lattice as four-fold stained glass."}
    :brillouin_oblique {:name "Brillouin Oblique" :description "Brillouin zones of an oblique lattice: only inversion survives."}
    :brillouin_rect {:name "Brillouin Rectangular" :description "Brillouin zones of a rectangular lattice as stained glass."}
    :textile_tunnel {:name "Textile Tunnel" :description "A UV textile tunnel: stepped crosses on an exact cylinder, travelling endlessly inward."}
    :flower_mandala {:name "Flower Mandala" :description "A dyadic triangular labyrinth flowering outward around a ring of dashed circles."}
    :hyperspace {:name "Hyperspace Warp" :description "D12-folded hex-lattice stars streaking outward through a dyadic log-radial warp."}
    :sprite_lattice {:name "Sprite Lattice" :description "UV glyphs sized by the graphene hopping band, sliding one cell per loop."}}
   {:dandelin_conics {:name "Dandelin Conics" :description "A cone cut through circle, ellipse, parabola and hyperbola, with Dandelin spheres."}
    :desargues {:name "Desargues" :description "Perspective triangles and their invariant axis of perspectivity."}
    :pascal_brianchon {:name "Pascal and Brianchon" :description "A hexagon on a conic and its tangent-line projective dual."}
    :pole_polar {:name "Pole and Polar" :description "An orbiting pole, its polar and two exact tangent contacts."}
    :conic_pencil {:name "Conic Pencil" :description "The projective pencil of conics through four invariant points."}}))

(def skipped
  "Loops of tessera.loops/all audited for a field port and not ported, with
   the reason. A painter or raster approximation is never presented as a port."
  {:julia_orbit "Escape-time iteration (up to 300 steps) with a data-dependent exit and smooth escape count; the DSL has no loops or branches, and unrolling 300 masked steps would have to freeze z after escape without inf*0."
   :sierpinski_zoom "26-level data-dependent IFS descent (a branch per level on the sub-triangle); no loops or branches in the DSL."
   :hydrogen_beat "Brightness is normalized by a per-frame peak over a 96x54 sample scan of the density; a pixel would need all 5184 evaluations. Needs a reduction pass or a precomputed peak(u) table."
   :caustics "Photons are refracted and bilinearly SPLATTED into a per-frame density buffer (a scatter); a per-pixel gather field cannot reproduce it."
   :caustics_gold "Same splat buffer as :caustics, with eighteen waves."
   :standard_map "Samples a precomputed phase portrait (iterated standard-map orbits binned into W/R/G/B/C/S rasters)."
   :delay_pendulums "Uses :splat and :exposure (particle splats) on top of the Huygens field; not a pure field."
   :fabric "Three composed closure layers reading spatial bins (int-array lookups per cell); needs arrays."
   :generated_2 "tessera.compose scene compiled to a closure over painter layers (iso pyramids, serpents); needs SDF field versions of the painters."
   :generated_3 "tessera.compose painter scene, as :generated_2."
   :mandala_tunnel "tessera.compose painter scene, as :generated_2."
   :serpent_garden "tessera.compose painter scene, as :generated_2."
   :temple "tessera.compose painter scene, as :generated_2."
   :phase_flow "A :frame loop (vector strokes), not a per-pixel field."
   :curl_flow "A :frame loop with :splat, :blur and :exposure; particles, not a field."
   :noise_flow_lines "A :frame loop with :splat; particle lines, not a field."
   :vortex_street "A :frame loop with :splat; particles, not a field."
   :cat_map "Splat loop (:splat/:exposure), no :field."
   :elliptic_billiard "Splat loop (:splat/:exposure), no :field."
   :hopf_fibration "A :frame loop with :splat (3D curves), not a field."
   :torus_knots "A :frame loop with :splat (3D curves), not a field."
   :lissajous_3d "A :frame loop with :splat (3D curves), not a field."
   :delay_mesh "A :frame loop with :splat, not a field."
   :moore_comets "A :frame painter (space-filling curve strokes)."
   :choreographies "A :frame painter (n-body orbits as strokes)."
   :figure_eight "A :frame painter (three-body orbit strokes)."
   :pendulum_wave "A :frame painter."
   :kepler_resonance "A :frame painter."
   :lissajous_oscillators "A :frame painter."
   :neon_pulse "A :frame painter (girih beads) with post stages."
   :neon_khatam "A :frame painter (girih beads) with post stages."
   :girih_light "A :frame vector painter with a :light pass."
   :girih_drift "A :frame vector painter with a :light pass."
   :girih_assembly "A :frame vector painter with a :light pass."
   :mandelbulb "Sphere-traced distance estimator (110 march steps, 5-step triplex iteration, both with data-dependent exits) plus normal and occlusion taps; needs loop-n (desargues PR #46)."
   :quaternion_julia "Sphere-traced quaternion Julia DE (80 march steps over a 7-step escape iteration) plus normal and occlusion taps; needs loop-n (desargues PR #46)."
   :menger_flight "Sphere-traced Menger sponge (90 march steps over a 4-level fold) plus normal and occlusion taps; needs loop-n (desargues PR #46)."
   :cardioid_julia "Escape-time iteration (up to 160 steps) with two data-dependent exits (escape, capture by the attracting fixed point), as :julia_orbit; needs loop-n (desargues PR #46)."
   :apollonian "Up to 30 data-dependent inversion folds, each choosing its dual circle by a branch and stopping on landing in a gasket circle; needs loop-n (desargues PR #46)."
   :harmonograph "A :frame painter: a 2400-segment polyline of translucent glow strokes. A field would need the distance to every segment per pixel and the painter's stroke compositing."
   :spirograph "A :frame painter: six hypotrochoids (more on a wider window) of about 900 glow-stroke segments each, composited in order; not a straight-line field."
   :knot_tubes "A :frame painter (sp/frame): 3D knots projected to depth-sorted line strokes; not a per-pixel field."})

(def follow-ups
  "Live-host considerations for the ported layers."
  {:brillouin_square "292 unrolled Bragg planes, about 56 KB of WGSL; same compile-hitch and per-pixel-cost caveat as brillouin_zones."
   :brillouin_oblique "344 unrolled Bragg planes, about 72 KB of WGSL."
   :brillouin_rect "456 unrolled Bragg planes, about 93 KB of WGSL: the heaviest layer; measure it live before relying on it at 2560x1080."
   :quantum_tapestry "40 modes per pixel (about 160 sin/cos). The f32 phase frac(n^2 tau) carries about 1e-3 rad of error at n = 40; check it live."
   :quantum_carpet "40 modes per pixel, as quantum_tapestry."
   :double_slit "Wave number and slit geometry are not params: the fringe normalization is a constant of them (36 Huygens terms per pixel)."
   :breathers "cosh/sinh as exponentials: exp(k dx) reaches about 1e13 at the frame edge, within f32 range."
   :sprite_lattice "Nine neighbour cells unrolled, each re-deriving its graphene-band glyph size and hue per pixel (the closure built a 24x16 table once per frame): about nine times the closure's per-pixel trig. Measure it live."
   :flower_mandala "blend mixes PACKED colours, so the form quantizes to 8-bit levels before the ring blend; on the GPU an f32 level that lands within an ulp of an integer can floor one level differently."
   :hyperspace "The 20 stars are evaluated unconditionally and masked by the closure's |da| > 4 width skip. log2 radius over the frame is small, so f32 frac(q - q_i) keeps its precision."})
