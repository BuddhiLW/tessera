# tessera

**Seamless mathematical wallpaper loops.** Islamic star patterns, the Fermi
surfaces of graphene, quantum revivals, three-body choreographies, Lagrangian
pendulums, the Hopf fibration: each one a video whose last frame flows into its
first.

<p align="center">
  <img src="docs/hero/graphene-fermi.webp" width="49%" alt="graphene Fermi contours">
  <img src="docs/hero/choreographies.webp" width="49%" alt="three-body choreographies">
  <img src="docs/hero/neon-pulse.webp" width="49%" alt="neon khatam stars">
  <img src="docs/hero/kepler-resonance.webp" width="49%" alt="Kepler resonance">
  <img src="docs/hero/quantum-tapestry.webp" width="49%" alt="quantum tapestry">
  <img src="docs/hero/hopf-fibration.webp" width="49%" alt="Hopf fibration">
</p>

Every loop is a function of a phase `u ∈ [0, 1)`. An N-frame video samples
`u = i/N`, so frame N is frame 0 again. Nothing is blended at the seam: the
mathematics itself repeats. Where physics does not repeat on its own, the
parameters are chosen so that it does:

- pendulum lengths come from the exact elliptic period, not the small-angle one;
- hydrogen energies are rational, so a superposition shares one common period;
- membrane modes are picked with Pythagorean frequency ratios;
- three-body orbits are closed by Newton shooting to 1e-9.

The camera never turns.

The generic machinery lives in
[desargues](https://github.com/mentat-collective/desargues) under
`desargues.wallpaper.*`: phase helpers, rasterisers, a 3D camera, glow splats,
bloom, motion blur, LuxCore, GPU field shaders and video encoders. This
repository holds the loops.

## Gallery

### Mechanics: Lagrangians and Hamiltonians

| | | |
|---|---|---|
| ![](docs/gallery/pendulum-wave.webp) **pendulum-wave**<br>24 nonlinear pendulums. L = ½ml²θ̇² + mgl cos θ through Emmy; lengths from 4√(l/g)·K(sin θ₀/2), so pendulum k swings exactly 18+k times | ![](docs/gallery/kepler-resonance.webp) **kepler-resonance**<br>six orbits with periods 12:6:4:3:2:1, Kepler's equation by Newton iteration, inclined planes | ![](docs/gallery/lissajous-oscillators.webp) **lissajous-oscillators**<br>3D harmonic oscillators with whole frequencies, checked against the Euler-Lagrange equations |
| ![](docs/gallery/figure-eight.webp) **figure-eight**<br>the Chenciner-Montgomery three-body choreography, Yoshida 4th-order symplectic, energy drift 2e-7 | ![](docs/gallery/choreographies.webp) **choreographies**<br>figure-eight, Butterfly I, Moth I and Yin-yang 1a (Šuvakov-Dmitrašinović), shot closed to 1e-9 | ![](docs/gallery/phase-flow.webp) **phase-flow**<br>pendulum phase space H = p²/2 − cos q; each particle turns its action-angle variable a whole number of times |

### Quantum

| | | |
|---|---|---|
| ![](docs/gallery/hydrogen-beat.webp) **hydrogen-beat**<br>a 3s/4d/4f/5g superposition; Eₙ = −1/2n² is rational, so ψ returns exactly | ![](docs/gallery/drum.webp) **drum**<br>square membrane modes with Pythagorean frequency ratios, mirrored to fill the frame | ![](docs/gallery/box-modes.webp) **box-modes**<br>a 16:9 infinite well, E ∝ m²/16² + n²/9², 48 commensurate modes |
| ![](docs/gallery/quantum-tapestry.webp) **quantum-tapestry**<br>the Talbot carpet in domain colouring: hue is the phase of ψ(x, t) | ![](docs/gallery/quantum-carpet.webp) **quantum-carpet**<br>a wave packet in a box; n² energies make the revival exact | ![](docs/gallery/double-slit.webp) **double-slit**<br>Huygens sources, crests in neon, the screen's cos² intensity as a bar graph |

### 3D

| | | |
|---|---|---|
| ![](docs/gallery/hopf-fibration.webp) **hopf-fibration**<br>Villarceau circles of S³ → S² on nested tori, stereographically projected, beads flowing along the fibres | ![](docs/gallery/torus-knots.webp) **torus-knots**<br>(p, q) torus knots at fixed poses, light pulses travelling along them, depth of field | ![](docs/gallery/lissajous-3d.webp) **lissajous-3d**<br>a lattice of 3D Lissajous curves with integer frequency triples, one comet each |

### Band structures and reciprocal space

| | | |
|---|---|---|
| ![](docs/gallery/graphene-fermi.webp) **graphene-fermi**<br>tight-binding \|f(k)\| contours of graphene, Dirac cones at K | ![](docs/gallery/kagome-fermi.webp) **kagome-fermi**<br>kagome lattice bands | ![](docs/gallery/twisted-bilayer.webp) **twisted-bilayer**<br>moiré of two honeycomb layers twisted by 0.11 rad |
| ![](docs/gallery/brillouin-zones.webp) **brillouin-zones**<br>Brillouin zones of the hexagonal lattice, by counting Bragg planes | ![](docs/gallery/brillouin-square.webp) **brillouin-square** | ![](docs/gallery/brillouin-oblique.webp) **brillouin-oblique** |
| ![](docs/gallery/penrose-fermi.webp) **penrose-fermi**<br>5-fold quasiperiodic band Σ cos(k·eⱼ) | ![](docs/gallery/heptagonal-fermi.webp) **heptagonal-fermi**<br>7-fold | ![](docs/gallery/octagonal-fermi.webp) **octagonal-fermi**<br>8-fold |
| ![](docs/gallery/fold-9.webp) **fold-9** | ![](docs/gallery/fold-11.webp) **fold-11** | ![](docs/gallery/fold-12.webp) **fold-12** |

### Ornament, curves and fractals

| | | |
|---|---|---|
| ![](docs/gallery/neon-pulse.webp) **neon-pulse**<br>khatam stars as glowing beads on a breathing beat, bloom and red/blue fringe | ![](docs/gallery/neon-khatam.webp) **neon-khatam**<br>eight-pointed stars and octagons on a tilted plane | ![](docs/gallery/girih-light.webp) **girih-light**<br>Hankin's polygons-in-contact on 4.8.8, the contact angle breathing |
| ![](docs/gallery/girih-assembly.webp) **girih-assembly**<br>the ceiling comes apart in a radial wave and settles back | ![](docs/gallery/moore-comets.webp) **moore-comets**<br>two closed Moore curves with comets | ![](docs/gallery/julia-orbit.webp) **julia-orbit**<br>the Julia set of c = 0.7885·e^{2πiu} |
| ![](docs/gallery/quasicrystal.webp) **quasicrystal**<br>seven plane waves, phases once around | ![](docs/gallery/sierpinski-zoom.webp) **sierpinski-zoom**<br>a zoom by its own similarity ratio, 2 | ![](docs/gallery/girih-drift.webp) **girih-drift**<br>six-fold stars drifting one lattice period |

Also `girih-gold`, path traced by LuxCore: gold strands over glazed lapis stars,
an area lamp orbiting once per loop.

## Render

Needs a JDK and `ffmpeg`.

```sh
clojure -M:render --list
clojure -M:render --preview                          # out/<id>.png + a 480x270 GIF each
clojure -M:render --only graphene-fermi --format mp4 # 1920x1080, the loop's own length
clojure -M:render --only kepler-resonance,drum --format webm --size 2560x1440 --out out/4k
clojure -M:test                                      # every loop's seam, plus the physics checks
```

Formats: `mp4` (H.264, yuv420p), `webm` (VP9), `gif`, `png` (a numbered
sequence). To use one as a desktop background, play it with
`mpv --loop-file=inf` under `xwinwrap`.

Against a local desargues checkout instead of the pinned git sha:

```sh
clojure -Sdeps '{:deps {io.github.mentat-collective/desargues {:local/root "../desargues"}}}' -M:render --preview
```

## Live preview

From a REPL, `desargues.wallpaper.preview` keeps one mpv window looping the
current loop. `show!` renders at 640x360 in the background and swaps it in.

```clojure
(require '[desargues.wallpaper.preview :as pv] '[tessera.loops :as loops])
(pv/show! (loops/all :kepler-resonance))   ; edit, re-eval, show! again
```

## Adding a loop

A loop is a map:

```clojure
{:id :my-loop :seconds 12 :fps 30 :size [1920 1080] :view [14.22 8.0]
 :field (fn [u] (fn ^long [^double x ^double y] 0xRRGGBB))   ; per-pixel layer
 :frame (fn [u] {:background "#0b1f3a" :shapes [...]})        ; vector layer
 :splat (fn [u] rows)                                         ; additive 3D glow points
 :light (fn [u] (fn ^double [^double x ^double y] k))         ; brightness
 :post  (fn [u] [[:bloom {...}] [:vignette {...}]])           ; post effects
 :blur  {:samples 3 :shutter 1}                               ; motion blur, seam-exact
 :ss 2}                                                       ; supersampling
```

- Build every motion from `desargues.wallpaper.loop` (`wave`, `osc`, `window`,
  `pulse`, `circle`, `shift`) so it is periodic in `u`.
- A palette cycled by phase needs integer frequencies (`tessera.palette`, on
  thi.ng/color), or the colours jump at the wrap.
- 3D loops use `desargues.wallpaper.space`: a fixed pinhole camera, depth cue
  and near clipping.
- A per-pixel shader written as a field form (`desargues.wallpaper.field`) also
  runs on the GPU through raster and OpenCL: about 16 ms per 1080p frame on an
  RTX 4070, against 40 ms on the JVM.
- `clojure -M:test` compares every loop's wrap with its ordinary
  frame-to-frame motion.

## Path-traced loops (LuxCore)

A loop with `:scene (fn [u] scene-map)` is rendered by the `:luxcore` engine of
`desargues.wallpaper.luxcore`. The scene map becomes LuxCore `.scn`/`.cfg` text,
and `luxcoreconsole` renders each frame (`DESARGUES_LUXCORE` picks another
binary).

```sh
clojure -M:render --only girih-gold --format mp4
```

## How it was made

The early loops were built in conversation with Claude. The physics loops were
then built by a swarm of agents (Claude, GPT and Kimi) working in parallel
branches. Each loop was checked for an exact seam and reviewed frame by frame
before it was merged.
