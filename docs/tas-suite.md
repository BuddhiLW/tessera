# TAS-style input transform recipes

TAS Visuals is a **style reference, not a source of artwork**. Use original
ornaments, your own SVGs, or Tessera lattices. This implements card TAS-SUITE
`20261108205634-50856b50`; SVG Offset's model is `20261108205043-257c55ee`.
Saving/importing fosfora presets and scene-walker registration are a separate
card. The recipes below are assembly instructions, not saved presets or a
claim of GPU/UI visual approval.

## Audit before adding anything

Read-only audit of fosfora `assets/effects/*.pfx`, the corresponding shaders,
and `assets/trama/effects` (2026-11-08):

| Existing node/effect | What it already covers | Decision here |
| --- | --- | --- |
| Tunnel | Log-polar procedural wall, integrated travel/twist, feedback phase | Do not clone its wall generator. `tas_torus` textures a tunnel with **input0**, mirrors arbitrary image edges, and locks travel/rotation to a loop. |
| Prism | N-fold FBM/geometric **source**, audio-driven zoom/rotation | No duplicate source; fold the incoming image instead. |
| Iris | Orbiting dot + feedback trails | Skip: not an input warp and not stateless/loop-locked. |
| Cleave | HPSS shard/ribbon particles, radial feedback echo | Skip particles/feedback; hyperspace is a stateless input warp. |
| Shards | Animated Voronoi source | Skip: no substitute Voronoi generator. |
| Panorama | Stereo/pitch particle display, horizontal afterglow | Not a 360 image transform despite the name. Equirect mirror is separate. |
| trama Tile | Square repeat, mirror, freely integrated scrolling | Keep for simple tiling. Add rectangular n×m, row offset and periodic sway. |
| trama Kaleidoscope / Mirror | N-fold free spin / planar quadrant fold | Keep for static/free-running work. Add loop-count rotation and equirect longitude/equator mirror with collapsed poles. |
| trama Zoom Blur | Static centered radial/directional blur, 32 taps | Keep for kick punches. Hyperspace adds a travelling radial compression wave, z cycles and 12 one-sided streak taps. |

## Emit and install

In a project REPL (desargues pinned by `deps.edn`, `naga` on PATH):

```clojure
(require '[tessera.layers :as layers]
         '[tessera.transforms :as transforms])
(layers/emit-trama! "out/layers") ; all source layers + seven input effects
;; Fast path: only the transforms, all naga-validated before any write:
(transforms/emit-trama! "out/layers")
(layers/emit-pfx! "out/effects")  ; existing source effects, unchanged
```

Copy the **seven `tas_*.wgsl` files** to fosfora's user trama search directory
(the same configured location used for Tessera source nodes). Add a source,
then the desired Tessera effect in that layer's chain editor. `inputs=1` means
the immediately connected image, not a generated replacement. Do not install
these files in the `.pfx` effects folder. No automatic fosfora install occurs.

### Why there is no transform `.pfx`

The pinned desargues `:pfx` path emits layer **sources**. fosfora `.pfx` `inputs`
are scalar controls; shader `input0` there is a named **internal pass** (Tunnel
uses it for its phase texture), not the preceding layer's image. Packaging a
warp as a standalone `.pfx` would silently sample the wrong thing. All seven
are useful as trama effects; `emit-pfx!` continues to emit the lattice sources
into `out/effects`. A future external-input `.pfx` adapter can reuse these UV
fields if the host adds that contract.

## Loop and sampling contract

- Every effect has `period=16` seconds. Slot 0 is `loop_rate`'s running
  integral, exactly as for Tessera trama sources: `u=fract(param(0u)/16)`.
  All motion is `sin(tau*k*u)`, `cos(tau*k*u)`, or a mirrored integer scroll.
- Set `loop_rate=1` for 16 seconds, `2` for 8, `0.5` for 32. `0` freezes the
  clock at its current phase (not necessarily phase zero). All effects in a
  16-second chain can use 1. For a source with period P, set its rate to P/16
  to share the same 16-second loop. Common period is what matters, not equal
  raw slider values. Reset/restart together if phase alignment is desired.
- Cycle counts and grid/fold counts round in the shader **and** have integer
  UI metadata. Static fractional modulation values still close. Changing
  parameters/audio or a nonperiodic input during capture invalidates the
  seamless-chain guarantee. Rate changes preserve integrated-clock continuity
  but change wall-clock duration.
- Aspect correction keeps circles round on 2560×1080. Sampling uses mirrored
  extension with a half-texel inset, no feedback or history. Row offsets may
  intentionally form brick-like joins; mirroring does not hide those joins.
- Trama input samples are premultiplied RGBA. Sprites multiply RGB **and** alpha
  by coverage; streaks average RGBA. Depth uses premultiplied luma (transparent
  pixels contribute zero height), before bloom. No colour palette is baked in.
- Sprite cell phase offsets **spatial pulse/rock**, not upstream video time.
  One input texture cannot re-evaluate SVG Offset at a different time per cell.
- Depth is a single height lookup + displaced sample (2 reads), not raymarched
  geometry/occlusion. Hyperspace costs 12 reads; other effects cost 1.

## Published node controls

Names, ids and parameter order are binding ABI: append only, never rename.
Slot 0 is always `loop_rate`; columns list slots 1 onward. Values below are
defaults. Counts/cycles are integers; `angle`, `rotate`, `rock` use turns.

| Node id / display name | Ordered parameters and defaults |
| --- | --- |
| `tas_torus` / Tessera Torus | `twist=.25, speed=1, depth_scale=.7, rotation=1, core=.02` |
| `tas_hyperspace` / Tessera Hyperspace | `z_speed=2, warp=.18, density=4, streak_length=.2` |
| `tas_tile` / Tessera Stagger Tile | `columns=4, rows=3, row_offset=.5, sway=.15, cycles=1` |
| `tas_ripple` / Tessera Ripple | `amplitude=.035, frequency=6, cycles=1, planar=0, angle=0` |
| `tas_kaleido` / Tessera Loop Kaleido 360 | `folds=6, zoom=1, rotate=0, rotation=1, equirect=0` |
| `tas_sprite_grid` / Tessera Sprite Grid | `columns=5, rows=3, fill=.65, pulse=.2, phase_step=.17, cycles=1, rock=.08` |
| `tas_depth` / Tessera Luma Depth | `parallax=.07, wave_depth=.25, frequency=3, cycles=1, angle=0, orbit=1, height_bias=.5` |

Torus `speed` is whole mirrored depth cycles per loop, not units/second.
`core` caps the log singularity. Ripple `planar=0` is radial, `1` planar.
Kaleido `equirect=1` interprets UV as longitude/latitude: x wraps, y mirrors
about the equator, longitude variation fades at the poles. In that mode zoom
is unused. It is an equirect image fold, not a perspective/sphere projector.

## Recipe 1 — Woven log tunnel

**Foreground layer chain**, in this order:

1. SVG Offset source: original rosette SVG (own artwork), `amount=12`,
   `shrink=.15`, `falloff=.7`, `boost=1.2`, background alpha. Palette: original
   cyan/violet/gold fills, order Radial. For a 16s loop choose ring speed
   `1/16` cycle/s, hue cycle `1/16` (or freeze hue), key repeat 1, key travel
   `1/16` cycle/s, key width `.15`, key level `.4`. These are design-model
   units from the SVG Offset spec; map to its final UI when that card lands.
2. `tas_tile`: columns 4, rows 3, row_offset .5, sway .1, cycles 1.
3. `tas_torus`: twist .35, speed 1, depth_scale .65, rotation 1, core .025.
4. Optional static host bloom: start intensity .15, threshold .7. Keep
   highlights coloured, not a white disk.

**Background layer**: `graphene_fermi` source, lattice_scale `4.1887902`,
contour_step `.25`, sharpness `24`; loop_rate `1` (period 16). Opacity `.22`,
normal on black; foreground screen/lighten. Do not stack two dense full-gain
rainbows. All transform loop_rate values are 1.

## Recipe 2 — Constellation depth wave

**Foreground chain**:

1. SVG Offset source: original isolated droplets/pins; amount 8, shrink .1,
   falloff .6, boost 1, alpha background, order Group. Ring/key travel each
   1 cycle / 16 seconds. Prefer original two-colour fills; no arbitrary hue
   sweep. Keep source ornament coverage near half the image.
2. `tas_sprite_grid`: columns 6, rows 3, fill .6, pulse .22,
   phase_step .17, cycles 1, rock .06.
3. `tas_depth`: parallax .045, wave_depth .3, frequency 3, cycles 1,
   angle .125, orbit 1, height_bias .35.
4. `tas_ripple`: amplitude .012, frequency 5, cycles 2, planar 1, angle .25.
5. Optional low static bloom; no feedback trails if exporting an exact loop.

**Background**: `kagome_fermi` source, contour_step `.25`, sharpness `24`,
lattice_scale `3.2`, opacity `.18`, loop_rate `1` (period 16). Sprite alpha
gaps reveal it. Tessera transform loop_rate values are 1. Bright marks shift
more than dark ones because depth reads luma; do not bloom before height.

## Recipe 3 — Mirrored 360 aurora rush

**Foreground chain**:

1. SVG Offset source: original horizontal ornament band, amount 10,
   shrink .2, falloff .8, boost 1, order Vertical, alpha background. Ring
   travel 2 cycles / 16s, key and cyclic palette each 1 cycle / 16s.
2. `tas_kaleido`: folds 4, rotate .1, rotation 1, equirect 1. UV is an
   equirectangular canvas; longitude edges agree and poles collapse.
3. Optional `tas_hyperspace`: z_speed 2, warp .12, density 3,
   streak_length .12 **only for a flat-screen rush variant**.
4. Optional static bloom and a dim lattice background as below.

For the actual 360 variant, **stop after step 2**: a later planar warp or
ordinary planar bloom can break the longitude/pole invariant. If bloom is
needed, put it before the final equirect fold. A plane is not a sphere.
For the flat variant use hyperspace for the rush. Background `penrose_fermi`,
lattice_scale `3.6`, contour_step `.25`, sharpness `24`, opacity `.15`;
period 16, loop_rate 1. To make that background 360-safe too, give it its own
`tas_kaleido equirect=1` as its final spatial node. Composite with a
conservative screen/lighten blend, not full dodge.

## Verification and acceptance

Run `tessera.transforms-test` with the test alias in a project REPL. Naga is
required (or set `NAGA` to its executable); missing/rejecting validators fail
closed. `validate-wgsl!` prepends a minimal resolution/param/real texture
binding fixture; it does not ship that fixture to fosfora. Tests check the
strict manifest, slot order, ranges, integer metadata, float32 finiteness at
parameter extremes, shared JVM/WGSL-IR parity, alpha gaps, luma response,
longitude/poles and emitter integration. These are CPU/compiler checks, not
GPU performance or an application screenshot.

Default 960-frame CPU reference results (RGBA mean absolute difference):

| Effect | last→first seam | maximum adjacent step |
| --- | ---: | ---: |
| torus | .00138629 | .00145740 |
| hyperspace | .00021822 | .00023923 |
| tile | .00025183 | .00027848 |
| ripple | .00004109 | .00004153 |
| kaleido | .00204974 | .00205832 |
| sprite grid | .00082067 | .00148925 |
| depth | .00005552 | .00005748 |

All endpoint u=0/u=1 errors are below `1.3e-16`. These are transform-kernel
results on a 16×9 asymmetric reference texture, not a native-size GPU render
or a measurement of SVG Offset recipe output. `naga 30.0.1` accepted all seven.

Seam checks use **960 frames (16s × 60fps)** with an asymmetric non-periodic
RGBA test image. They compare final→first with the maximum ordinary adjacent
step and separately compare u=0 with u=1; no seam crossfade. For recipes,
freeze all non-loop modulation and remeasure at the target frame count after
changing integer frequencies (at least 8 samples per highest harmonic).
Do not call a seam larger than the adjacent-step maximum a pass.

Before saving presets on the separate card: inspect u=0, .3, .7 and the seam
at the native output size in fosfora; check the black-space balance, no white
cores, readable 2–3 colour hierarchy and acceptable GPU frame cost. The SVG
Offset source remains that card's dependency. No TAS artwork is included.
