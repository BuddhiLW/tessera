# Fourier epicycles

`tessera.fourier` turns a closed curve or SVG path into a chain of integer
harmonics. All four loops are in `tessera.loops/all`, in media family **Curves**:

| Loop | Source | Terms | Period |
|---|---|---:|---:|
| `fourier-trefoil` | XY projection of `tessera.figures/trefoil`, equal arc length | 24 | 20 s |
| `fourier-space` | The same trefoil in 3D, spheres on rods and a fading tube | 24 real-vector harmonics | 20 s |
| `fourier-formula` | Gauss's law, `∇·E = ρ/ε₀`, through LaTeX/dvisvgm | 96 | 32 s |
| `fourier-live` | Native-parameter analytic trefoil projection, SDF layer | 2 nonzero harmonics (≤24 budget) | 20 s |

```sh
timeout 900 clojure -J-Xmx2g -M:render --only fourier-trefoil,fourier-space,fourier-formula,fourier-live --preview
```

## Sampling and reconstruction

```clojure
(require '[tessera.fourier :as f]
         '[tessera.figures :as figures]
         '[desargues.player.svg-path :as svg])

;; Any tessera curve f(turns) -> [x y] or [x y z].
(def samples (f/resample-curve figures/trefoil 512))
(def spectrum (f/top-terms (f/dft samples) 24))
(f/point spectrum 0.3)      ; drawing head
(f/chain spectrum 0.3)      ; origin, then every rod endpoint

;; Absolute segments from the shared desargues parser (arcs normalize to cubics).
(def samples-2d
  (f/resample-segments (svg/segments "M0 0C0 2 2 2 2 0Z") 256))

;; Import path/rect/use artwork; defs/use placement and nested affine
;; transforms are supported. This is not a general SVG renderer.
(f/resample-segments (f/svg->segments (slurp "drawing.svg")) 512)
```

`resample-points` samples a closed polyline uniformly in arc length, without
repeating the endpoint. Constant, empty, non-finite and inconsistent paths are
rejected. `resample-curve` approximates arc length on at least 2048 points (or
16M); SVG curves flatten at default flatness 0.01 SVG units, configurable.

Each disconnected SVG/glyph subpath is closed first, **then joined in document
order by straight bridges**. The last-to-first bridge closes the whole walk.
These bridges are deliberate parts of the drawing, not pen-up jumps.

The normalized complex DFT uses `z=x+iy`; frequency bins above the positive
half become negative integers. For even M, Nyquist is `-M/2`. All M terms
reconstruct the samples. `top-terms` ranks by magnitude with deterministic
ties. DC participates in that ranking (it is a stationary offset, not a ring).

For 3D, conjugate pairs become real-vector harmonics
`A cos(k τu) + B sin(k τu)`; DC and Nyquist are not doubled. A spatial rod tip
usually traces an **ellipse**, sometimes a line, rather than a complex-plane
circle. Magnitudes are RMS vector amplitudes. The fixed camera projects the
orbits, rods, depth-sorted shaded spheres and tube segments.

Every temporal angle is an integer multiple of `τu`. Trails are reconstructed
from `u-age`, oldest first, with fixed age weights; there is no history buffer
that empties at the seam. Every frame can be rendered out of order.

## TeX provenance and optional binaries

Both `latex` and `dvisvgm` were installed and successfully typeset
`f/gauss-law` through `desargues.tex.typeset`. The resulting SVG is checked in at
`resources/tessera/fourier/gauss-law.svg`. Registry loading never spawns a
process, and rendering needs neither binary.

To regenerate explicitly:

```clojure
(when-let [svg (:svg (f/typeset-formula))]
  (spit "resources/tessera/fourier/gauss-law.svg" svg))
```

`typeset-formula` checks both tools and returns `:skipped` if either is absent;
otherwise it returns SVG/segments or the upstream typesetting `:error`.
The checked-in asset lets CI test formula geometry without a TeX installation.
The upstream adapter runs external processes; invoke regeneration with a
bounded evaluation/process timeout. Missing assets omit the formula loop.

## Native live field

`tessera.layers.ports.fourier/fourier-live` is published through
`tessera.layers/layers`, `by-id`, and the ISF/GLSL, trama WGSL and fosfora PFX
emitters. Effect name **Fourier Epicycles** and parameter order are stable:

1. host `loop_rate` (slot 0);
2. `trail_width`;
3. `ring_width`;
4. `tail_length`;
5. `ring_gain`.

The PFX host also supplies the running-rate integral in its usual extra slot.
Offline bloom/vignette are not baked into the live source.

The trefoil's native parameterization has exactly two nonzero complex DFT
terms (frequencies 2 and −1); numerical noise is discarded. This respects the
≤24 decision and avoids unrolling an expensive solver for 24 almost-zero
harmonics. Its locus matches the curve, but pen speed differs from the
arc-length-resampled painter. It is a **native field, not a pixel-exact port
of that painter**. `fourier-live` offline renders this same field AST.

- Circles are exact ring SDFs; rods are capsule SDFs.
- The trail is the analytic Fourier curve as a tube distance estimator:
  twelve fixed angular seeds, four bounded Gauss–Newton refinements, all
  unrolled at AST construction. Each evaluation uses curve values and
  derivatives, **not polyline segments, samples, texture lookups or history**.
- This is not an exact global signed-distance oracle. The solver can miss a
  closest stationary point far from the tube or at crossings. Tests recover
  240 on-curve probes to <1e−8 and compare an 81-point grid against 8192 dense
  reference samples to <0.005 scene units (under one native 1080p pixel).
- The AST has 11,905 tree nodes and emits about 49 KB of trama WGSL. It has no
  runtime loops or arrays. Both trama and PFX shaders pass Naga with a host
  uniform/param preamble. That validates shader structure, **not measured
  live GPU frame rate**. Measure in fosfora before assuming 2560×1080/60 fps.

## Verification

Use a worktree-owned warm REPL with `-Xmx2g`, never the coordinator ports.
The task was verified on port 7938, with heap max 2,147,483,648 bytes.

```clojure
(require '[clojure.test :as t]
         'tessera.fourier-test 'tessera.fourier-live-test)
(t/run-tests 'tessera.fourier-test 'tessera.fourier-live-test)
;; 15 tests, 2399 assertions, zero failures/errors.

;; Slow review helper: every native adjacent frame, streaming two images.
(tessera.fourier-live-test/native-seam-report)
```

All native adjacent frames were rendered at 160×90, including postprocessing
(and SS=2 for the field). Mean absolute RGB-channel difference, 0..255:

| Loop | Native frames | Wrap | Maximum ordinary step |
|---|---:|---:|---:|
| `fourier-trefoil` | 600 | 0.267847 | 0.306690 |
| `fourier-space` | 600 | 0.101829 | 0.143194 |
| `fourier-formula` | 960 | 0.861319 | 0.917778 |
| `fourier-live` | 600 | 0.160880 | 0.193380 |

**Every wrap ≤ ordinary maximum**, with no shortened/aliased preview period.
The native counts exceed 8× the highest temporal harmonic (41, 26, 67, 2).
Regular seam tests also run at the actual spec frame count using distributed
adjacent steps. JVM/GLSL/WGSL IR, independent scalar references, slot ordering,
formula absence handling, registry and Curves media wiring are tested.

Regression run: native layers (14 tests / 1138 assertions), media (7 / 1277),
frame (8 / 640) and loop registry (3 / 87) pass. The port audit has 8 existing
failures for `apollonian`, `mandelbulb`, `cardioid-julia`, `spirograph`,
`menger-flight`, `quaternion-julia`, `harmonograph`, and `knot-tubes`, which
lack an audit entry on the base branch. Removing all Fourier additions at
runtime reproduces exactly those same 8 failures. No Fourier audit fails.
