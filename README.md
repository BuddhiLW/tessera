# tessera

Seamless background-wallpaper video loops from mathematics: Islamic girih star
patterns, fractals, wave fields and light, tiles that come apart and settle back.

Every motion is periodic in a phase `u ∈ [0, 1)` and an N-frame loop samples
`u = i/N`, so the frame after the last is the first: the videos wrap without a
seam. The generic machinery is in
[desargues](https://github.com/mentat-collective/desargues) (`desargues.wallpaper.*`);
this project holds the loops themselves.

| loop | what moves |
|---|---|
| `neon-khatam` | eight-pointed stars and octagons as strings of glowing beads on a tilted plane; beads march, inner outlines turn, a ring of light travels outward; bloom, red/blue fringe, depth of field |
| `moore-comets` | two closed Moore curves (Hilbert's closed cousin), a thi.ng/color scheme flowing along them, comets of light running around |
| `girih-light` | khatam eight-pointed stars on the 4.8.8 tiling (Hankin's method), the contact angle breathing, a band of light sweeping the ceiling |
| `girih-drift` | six-fold two-point stars on 3.6.3.6, drifting one lattice period per loop |
| `girih-assembly` | the 4.8.8 ceiling comes apart in a radial wave, each tile turning, and settles back |
| `quasicrystal` | seven plane waves summed, phases once around |
| `julia-orbit` | the Julia set of c = 0.7885·e^{2πiu}, c once around its circle |
| `sierpinski-zoom` | a zoom by 2 into a corner of Sierpinski's triangle, its own similarity ratio |
| `girih-gold` | path traced by LuxCore: gold strands extruded over glazed lapis stars and terracotta squares, an area lamp orbiting once per loop |

## Render

Needs a JDK and `ffmpeg`.

```sh
clojure -M:render --list
clojure -M:render --preview                         # out/<id>.png + a 480x270 GIF each
clojure -M:render --only girih-light --format mp4   # 1920x1080, the loop's own length
clojure -M:render --only julia-orbit,quasicrystal --format webm --size 2560x1440 --out out/4k
clojure -M:test
```

Formats: `mp4` (H.264, yuv420p), `webm` (VP9), `gif`, `png` (a numbered sequence).

Against a local desargues checkout instead of the pinned git sha:

```sh
clojure -Sdeps '{:deps {io.github.mentat-collective/desargues {:local/root "../desargues"}}}' -M:render --preview
```

## Live preview

From a REPL, `desargues.wallpaper.preview` keeps one mpv window looping the
current loop; `show!` renders at 640x360 in the background and swaps it in.

```clojure
(require '[desargues.wallpaper.preview :as pv] '[tessera.curves :as c])
(pv/show! c/moore-comets)        ; edit, re-eval, show! again
(pv/status)
```

## Adding a loop

A loop is a map in `tessera.loops`:

```clojure
{:id :my-loop :seconds 12 :fps 30 :size [1920 1080] :view [14.22 8.0]
 :field (fn [u] (fn ^long [^double x ^double y] 0xRRGGBB))   ; per-pixel layer, optional
 :frame (fn [u] {:background "#0b1f3a" :shapes [...]})        ; vector layer, optional
 :light (fn [u] (fn ^double [^double x ^double y] k))         ; brightness, optional
 :ss 2}                                                       ; field supersampling
```

Build each motion from `desargues.wallpaper.loop` (`wave`, `osc`, `window`,
`pulse`, `circle`, `shift`) so it is periodic in `u`. A palette whose phase is
cycled must have integer frequencies (`tessera.palette/cosine` with `c` in
whole numbers), or the colours jump at the wrap. `clojure -M:test` checks
every loop's wrap against its ordinary frame-to-frame motion.

## Path-traced loops (LuxCore)

A loop with `:scene (fn [u] scene-map)` is rendered by the `:luxcore` engine
of `desargues.wallpaper.luxcore`. The scene map becomes LuxCore `.scn`/`.cfg`
text and `luxcoreconsole` renders each frame. Set `DESARGUES_LUXCORE` to
use another binary. Meshes come from `lx/prism` and `lx/ribbons`. Frames
render one at a time. At 640x360 and 48 spp a frame takes about 6 s on CPU.

```sh
clojure -M:render --only girih-gold --format mp4
```

The test suite does not render path-traced loops.

## Roadmap

- fork and update [thi-ng/luxor](https://github.com/thi-ng/luxor) as the scene
  DSL over `desargues.wallpaper.luxcore` (it emits LuxRender 1.x `.lxs` today,
  on thi.ng/geom 0.0.x and an org-mode build);
- GPU rendering (`renderengine.type = PATHOCL`) and denoising for 1080p loops;
- port geometry kernels from [thi-ng/vexed-generation](https://github.com/thi-ng/vexed-generation)
  (Houdini VEX, not linkable) to C compiled to wasm, loaded from cljw with
  `wasm/load`. cljw has no native FFI, so LuxCore itself stays a separate process.
