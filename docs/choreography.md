# Hamiltonian light

`tessera.choreography/loops` exposes three fixed-camera, seamless loops. The
coordinator adds these to the gallery; this change does not modify `loops.clj`.

- **figure-eight** — Chenciner–Montgomery equal-mass three-body choreography,
  G=m=1, period 6.32591398. Twelve thousand fourth-order Yoshida steps, a linear
  endpoint correction, periodic interpolation, three neon suns and long tails.
- **choreographies** — a stationary atlas of four distinct periodic three-body
  trajectories: figure-eight, Butterfly I, Moth I, Yin-yang 1a. The latter three
  start from the collinear initial conditions published by Šuvakov and
  Dmitrašinović, *Three Classes of Newtonian Three-Body Planar Periodic Orbits*,
  Physical Review Letters 110, 114301 (2013), DOI: 10.1103/PhysRevLett.110.114301.
  The atlas uses individual palettes and fixed, aspect-preserving fits. It does
  not rotate the configurations. Each cell makes an integer number of periods.
- **phase-flow** — 3,520 particles on 88 pendulum energy components, H=p²/2−cos q.
  Midpoint quadrature of dt=dq/p builds uniform-time angle tables; q=A sin θ
  removes the libration turning-point singularity. Inverting the tables and
  evaluating p on H=E gives both libration and oppositely directed rotation.
  Integer winding numbers (1, 2, 3) quantise playback to an exact loop. The
  infinite-period separatrix stays stationary and gold. Display q is periodic:
  long segments crossing the cylinder boundary are omitted, not drawn as chords.

## Numerical closure, not an approximate final frame

Velocity Verlet is symplectic; the symmetric Yoshida composition is fourth
order. The last stored state is retained and corrected to equal the first.
The tiny raw end–start mismatch is distributed linearly in phase across all
12 state coordinates. Sampling uses `frac(u)`, including negative trail times.
The clamp at a rounded `frac(-epsilon)=1` prevents an out-of-range endpoint.

`shoot` implements finite-difference, damped Newton least squares in p1,p2,T,
using every coordinate of the period map and a pivoted 3×3 linear solve.
Starting from the paper's rounded guesses requires only a few Newton updates
at sufficiently high integration resolution. Near encounters make coarse
solutions misleading: always double the integration resolution before accepting
a small residual. The refined seeds are stored in source to avoid a nonlinear
solve on gallery startup; `verified-trajectory` checks their raw residual before
applying the tiny cosmetic closure. Integration and dense table generation are
lazy and happen once per JVM, never per frame. No gravitational softening is used.

## Measured numerical checks

The three atlas seeds were refined at 1,600,000 Yoshida microsteps, then
independently integrated at 3,200,000 (not merely replayed at the shooting
resolution). Raw endpoint L2 errors and sampled energy ranges:

| orbit | doubled-resolution return error | raw energy range |
|---|---:|---:|
| Butterfly I | 3.27e-9 | 3.05e-10 |
| Moth I | 4.95e-10 | 2.06e-12 |
| Yin-yang 1a | 1.24e-7 | 4.74e-9 |

The explicitly closed figure-eight has energy range 2.00e-7. The closed atlas
has a larger worst-case energy error (Butterfly I: 2.00e-5), because its close
encounters amplify even the 3e-9 endpoint correction; this is under 1e-5 of
its binding energy. The regression test covers the **corrected** tables, not
only the symplectic integrator's raw trajectories.

## Warm development

Use the worktree's own nREPL with the local `desargues` dependency and `:test`
alias. Require `tessera.choreography-test`, then call `clojure.test/run-tests`.
The suite tests the same rendered wrap criterion as `loops_test.clj`, endpoint
closure, energy conservation, the two Yoshida implementations, pendulum level
sets and Hamilton's equations. Rendering is via `desargues.wallpaper.render`:

```clojure
(r/still! c/choreographies 0.187 "out/choreographies.png" {:size [960 540]})
(r/encode! :mp4 c/choreographies "out/preview/choreographies.mp4"
           {:size [640 360] :fps 20})
```

`out/` is intentionally ignored by git. Previews contain the full phase cycle,
not a truncated excerpt.
