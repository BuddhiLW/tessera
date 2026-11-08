# Periodic dye flows

`tessera.flows/loops` exposes `:curl-flow` and `:vortex-street` independently
of the central registry (`loops.clj` is intentionally unchanged). The camera
and coordinate axes stay fixed; nothing rotates as a whole.

## Curl flow

2,560 deterministically seeded particles follow an analytic incompressible
velocity `(∂ψ/∂y, −∂ψ/∂x)`. The stream function is a finite Fourier sum with
integer temporal frequencies on a 128/9 × 8 torus. Eight asymmetric integer wavevectors
of unequal amplitudes/phases give differently sized eddies rather than a
paired-product cellular grid or a rigidly moving picture.
The tracer velocity per unit loop phase is four times `curl-velocity` (a
constant speed scale, equivalently the curl of `4ψ`). Paths use RK4 and are
stored at 513 ages, not reintegrated at every render.

## Von Kármán street

Two vortices per x-periodic cell represent two infinite staggered rows. For
cell length `L = (128/9)/3 ≈ 4.7407407407`, circulation `±Γ = ±4` and row separation `h`,

```
cosh(πh/L) = √2
U = Γ tanh(πh/L) / (2L)
T = L/U
```

Here `h ≈ 1.3300144648`, `U ≈ 0.2983106733`, `T ≈ 15.8919581659` in
physical units. The displayed loop lasts 20 seconds. Laboratory coordinates
advance exactly `L` during one loop; the fixed view contains several tiled
cells. This is translation of a relative equilibrium, not rotation or a
numerically forced closure.

For displacement `(dx,dy)`, let `D = cosh(2πdy/L) − cos(2πdx/L)`. The velocity
per unit circulation is

```
(−sinh(2πdy/L), sin(2πdx/L)) / (2 L D).
```

This differentiates the periodic Green's function `log|sin(πz/L)|`, rather
than truncating an image sum. The implementation evaluates `D` as a sum of
squares to avoid cancellation near a core. The corresponding Kirchhoff
Hamiltonian is `−Σ(i<j) Γi Γj log(2Dij)/(4π)`, up to an irrelevant constant.
The noncanonical equations are `Γi ẋi = ∂H/∂yi`, `Γi ẏi = −∂H/∂xi`.

The street's motion is analytic. An independent implicit-midpoint
integrator provides a symplectic physics check, including a genuinely
evolving four-vortex case (not only the equilibrium). No core softening or
linear endpoint correction is used. 1,536 passive tracers per cell, coloured
by their seed origin, are integrated in the translating frame with adaptive
RK4 microsteps resolving core angular speed, then displayed in the lab frame.
They have no back-reaction on the vortices. Stratified seeds cover the entire
vertical domain in each cell, then tile across the full view. Alternating
warm/cool horizontal origin bands show their deformation around each vortex.

## Why the dye closes

For each particle choose a fixed offset `d` and set its age
`p = frac(u + d)`. Store a whole open trajectory from the corresponding birth
phase. At `u + 1`, the field, birth phase, path, colour and age are identical.
The street's additional translation changes by one full lattice period.
A quintic C2 lifetime envelope is zero at both ends, and multiplies **every
sample of the entire streak**, not just its head. The tail has its own age
visibility too. Thus an old tail cannot flash when the particle is replaced.
There is no crossfade between endpoint images and no false claim that
advected particles return. Subframe averaging uses the renderer's `:blur`.

## Warm-REPL use

Use an existing REPL started with `:test`, bounded to at most 4 GiB. Do not
start a second JVM for tests or previews.

```clojure
(require '[tessera.flows :as flows]
         '[tessera.flows-test]
         '[desargues.wallpaper.render :as r]
         '[clojure.test :as test])
(test/run-tests 'tessera.flows-test)
(r/still! flows/curl-flow 0.25 "out/preview/curl-flow.png" {:size [640 360]})
(r/still! flows/vortex-street 0.25 "out/preview/vortex-street.png" {:size [640 360]})
(load-file "dev/flows_preview.clj")
(def progress (atom nil))
(def job (future (flows-preview/render! 5 progress)))
@progress
```

The preview helper streams complete 18/20-second loops at 5 fps, one
640×360 frame at a time, with only two encoder threads. Set fps to 30 for
smooth review at greater rendering cost. It deliberately does not call the
generic encoder's parallel frame batcher. PNG stills are separate calls;
`out/` is ignored by git. Specs retain 1920×1080 / 30 fps production defaults.

Tests cover the gallery-style wrap-vs-ordinary-motion comparison, identical
rendered phases one period apart, negative subframe phases, divergence and
stream-function derivatives, replacement-tail invisibility, finite path
tables, Hamiltonian equations/Green-function limits, equilibrium translation,
conservation in a non-equilibrium problem, and midpoint reversibility.
