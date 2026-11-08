# The media pack

Most tessera loops are offline renders: closures, LuxCore scenes and
composed scenes, which cannot become live `.pfx` effects. The media pack
puts every loop, rendered or not, in one directory with a manifest and
thumbnails, so fosfora's Catalog can browse them as clips grouped by
family.

## Layout

```
out/media/
  media.json, media.edn    one manifest, the same data in both formats
  <id>/clip-hd.mp4         1920x1080, hardlinked from out/hd/<id>.mp4
  <id>/clip-uw.mp4         2560x1080, hardlinked from out/uw/<id>.mp4
  <id>/thumb.webp          480x270 still
  <id>/preview.webp        animated, about 3 s, 320x180 at 12 fps
```

- The thumbnail is the clip's middle frame. A loop with no clip falls back
  to `docs/gallery/<id>.webp`, scaled and cropped to 16:9.
- The preview is cut from the middle of the clip and only exists when a
  clip does.
- Clips are hardlinks, so the pack costs no extra disk on the same
  filesystem. On another filesystem they are copied.

## Manifest

```json
{"version": 1,
 "items": [
  {"id": "choreographies",
   "title": "Three-Body Choreographies",
   "description": "Three-body atlas — figure eight, Butterfly I, Moth I, Yin-yang 1a",
   "family": "Orbits",
   "tags": ["three-body", "figure-eight", "n-body", "choreography"],
   "seconds": 24, "fps": 30, "loop": true,
   "clips": {"hd": "choreographies/clip-hd.mp4", "uw": "choreographies/clip-uw.mp4"},
   "thumb": "choreographies/thumb.webp",
   "preview": "choreographies/preview.webp",
   "source": "tessera.loops/choreographies",
   "author": "tessera"}]}
```

- Every path is relative to the manifest's directory.
- `family` is one of: Orbits, Curves, Knots, Fractals, Lattices, Waves,
  Quantum, Tilings, Flows, Scenes.
- `title` is a short tile title. `description` is the loop's own longer
  title.
- `"loop": true` means the clip is seamless.
- Every loop in `tessera.loops/catalog` has an item. A loop that has not
  been rendered yet has `"clips": {}`, and `thumb` and `preview` may be
  absent. Show it as "not rendered".

Families, tags and titles are hand-curated in `tessera.media/families` and
`tessera.media/titles`. A test fails when a new loop has no family.

## Regenerate

```
clojure -M:media            # writes ./out/media from ./out/hd and ./out/uw
clojure -M:media /path/to/tessera
```

or from a REPL:

```clojure
(tessera.media/emit-pack! {:root "/home/klein/PP/mentat-collective/tessera"})
```

Requirements and behaviour:

- `ffmpeg` and `ffprobe` must be on PATH, with libwebp.
- It is idempotent. A clip, thumb or preview newer than its source is
  skipped, and the manifest is rewritten only when it changes.
- Re-run it after rendering, for example after `clojure -M:render` or
  `clojure -M:uw`. Clips rendered since the last run are picked up, and
  their thumbs move from the gallery still to the clip's frame.
- A clip whose render has been removed is unlinked from the pack.

## How fosfora consumes it

Planned on the fosfora side:

- `FOSFORA_MEDIA_DIRS` lists directories, the same pattern as
  `FOSFORA_EFFECT_DIRS`.
- fosfora reads `media.json` in each one and adds a Catalog tab "Media"
  with thumbnails grouped by `family`, in the order above.
- Hovering a tile plays `preview` when it is present.
- Clicking or dragging a tile adds a media layer. On 21:9 output it picks
  `clips.uw`, falling back to `clips.hd`; otherwise `clips.hd`, falling
  back to `clips.uw`. Play it looped; it is seamless.
- An item with empty `clips` is not playable. Show its thumb dimmed and
  marked "not rendered".
