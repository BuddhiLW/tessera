# MATH-FOURIER checkpoint

Card: `20261108205553-16315d61`. Branch: `math-fourier`, based on `eff3544`.

## Completed

- Read the task card and bootstrapped this isolated worktree.
- Confirmed the worktree initially had no changes.
- Started a Carto index of the worktree (scan `ddf5efe1-8789-4117-93da-53317aec94bf`); last observed in the embedding phase.
- Requested an isolated CIDER REPL named `tessera-fourier` on port 7938 with `-Xmx2g`. This REPL must be stopped before the restart; recreate it afterwards and verify its heap.
- Available host memory was about 28 GiB; recheck before testing and stop if below 6 GiB.

No Fourier implementation or tests have been written or run yet.

## Next

1. Poll the Carto scan. Use its reported worktree scope ID for indexed queries. Inspect `tessera` curves, `tessera.loops/all`, seam tests, `tessera.layers` ports, `desargues.player.svg-path`, and `desargues.tex.typeset` through Carto only.
2. Implement `tessera.fourier`: equal arc-length resampling of closed paths (curve functions and SVG segments), signed integer-frequency DFT, top-N magnitude selection, and phase-based reconstruction.
3. Render 2D epicycles and projected 3D spheres/rods with a fading analytic trail and drawing head. Include a trefoil; use the TeX formula only if `latex` and `dvisvgm` are installed. Optional art SVG.
4. Add an unrolled N=24 live-layer port: analytic-curve trail SDF and circle ring SDFs following existing layer ports.
5. Register the loops under media family `Curves`, with titles; add seam tests per loop and numerical/port tests. Every temporal quantity must depend on `tau*u`, with seam <= adjacent step.
6. WIP-commit each completed subtask using explicit staged paths. Verify in the owned warm REPL (never ports 7888 or 7910), stop it, push to origin, and open a PR to BuddhiLW/tessera without merging.

## Open questions

Actual curve/SVG data shapes and live-port AST conventions have not yet been inspected. Confirm signed/Nyquist handling, disconnected TeX contour closure policy, and live SDF expression size during implementation. Wrap long/background shell commands in `timeout`.
