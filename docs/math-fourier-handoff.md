# MATH-FOURIER checkpoint

Card: `20261108205553-16315d61`. Branch: `math-fourier`, based on `eff3544`.
Original bootstrap commit: `b7b9fd9`.

## Completed after restart

- Resumed the original Carto scan; inspected tessera curves/loops/media/layers
  and pinned desargues SVG/TeX/space/field APIs through Carto only.
- Owned CIDER REPL `tessera-fourier`, port **7938**, PID **205109**, heap verified
  at 2,147,483,648 bytes. **Stop it before exit/restart.** No coordinator REPL.
- Implemented and tested `tessera.fourier`: closed equal-arc-length paths,
  SVG contours and bridges (including defs/use/rect/transforms), signed 2D
  DFT, 3D real-vector harmonics, top-N selection and periodic reconstruction.
- Both latex and dvisvgm installed. Successfully generated Gauss-law SVG via
  desargues.tex.typeset; asset checked in, no runtime tool dependency.
- Implemented 2D circles/trail and 3D spheres/rods/tube, registered with formula
  and live field in loops/all and Curves media metadata.
- Published native live field in `tessera.layers.ports.fourier`: TWO exact
  native-parameter harmonics (within <=24 decision), analytic curve tube
  estimator (12 seeds x 4 refinements, unrolled), exact ring/rod SDFs. Native
  field, not a false pixel-parity claim against the arc-length painter.
- Checkpoints: `7006417` core; `47a5419` offline loops/TeX asset; `c27b650`
  native field/registry/media. Commit current docs/test hardening next.

## Verification

- Latest new suites: **15 tests / 2399 assertions, 0 failures/errors**.
- Native layers 14/1138, media 7/1277, frame 8/640, loop registry 3/87 all pass.
- Port audit has 8 unrelated base failures: apollonian, mandelbulb,
  cardioid-julia, spirograph, menger-flight, quaternion-julia, harmonograph,
  knot-tubes have no audit entries. Reproduced the same 8 after removing
  Fourier additions at runtime. Do not claim full regression green.
- ALL native consecutive steps at 160x90 + post pass seam<=step-max:
  trefoil 0.267847<=0.306690 (600); space 0.101829<=0.143194 (600);
  formula 0.861319<=0.917778 (960); live 0.160880<=0.193380 (600).
- GLSL emitter, WGSL IR/reference checks pass. Naga validates both emitted
  trama/PFX shaders with the required uniform/param host preamble. ~49KB.
- clj-kondo new source/tests: 0 errors/warnings. `git diff --check` clean.
- Host memory last >30GiB; keep checking, stop/shout blocked below 6GiB.

## Remaining

1. Load the latest `fourier_live_test.clj` with new `native-seam-report` helper
   and rerun targeted suites/lint; optional small gallery renders for review.
2. Commit docs and last SVG close-after-Z/test/lint hardening (explicit paths).
3. Stop owned REPL 7938 and verify it is gone.
4. Push `math-fourier` to origin BuddhiLW/tessera; open PR (do not merge).
5. Move card to review, shout completed with PR URL, report known base failures.

Full implementation/limits/verification documentation: `docs/fourier.md`.
No optional art/svg sample or morphing variant was added; required shapes and
live budget are covered. No GPU performance claim: measure in fosfora.
