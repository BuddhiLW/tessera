(ns tessera.frame
  "Aspect: one loop spec at any frame size.

   A spec is authored for its :size (1920 x 1080) and :view (the scene window
   in units). The renderer keeps the scale of the limiting axis and shows
   more world along the other, so a 2560 x 1080 frame of a field loop already
   shows a wider stretch of the same plane, never a stretched or cropped one.

   Loops whose content is laid out for the authored window (a particle
   domain, a lattice of breathers, a plot axis, a fixed box) carry
   :reframe, (fn [view] spec): the same loop rebuilt for the window `view`
   that a frame of another aspect shows. `at-size` calls it only off the
   authored aspect, so 16:9 output is the authored spec itself, bit for bit."
  (:require [desargues.wallpaper.render :as r]))

(def ultrawide
  "fosfora's HDMI-0 output."
  [2560 1080])

(defn view-of
  "The spec's authored scene window."
  [spec]
  (or (:view spec) r/default-view))

(defn shown-view
  "The scene window [w h] in units that a `size` frame shows of `view`: the
   renderer fits the view (pixels per unit = min over the axes), so the
   other axis widens."
  [[vw vh] [w h]]
  (let [s (min (/ (double w) (double vw)) (/ (double h) (double vh)))]
    [(/ (double w) s) (/ (double h) s)]))

(defn native-aspect?
  "True when `size` has the aspect of the spec's own :size (exact, integer)."
  [spec [w h]]
  (let [[sw sh] (or (:size spec) [1920 1080])]
    (= (* (long w) (long sh)) (* (long h) (long sw)))))

(defn at-size
  "`spec` for a `size` frame: the spec itself at its own aspect (only :size
   set), otherwise rebuilt by its :reframe for the window that frame shows.
   A spec without :reframe is returned with :size set; the renderer widens
   its view."
  [spec size]
  (cond
    (nil? size) spec
    (native-aspect? spec size) (assoc spec :size size)
    (:reframe spec) (let [build (:reframe spec)
                          view (shown-view (view-of spec) size)]
                      (assoc (build view) :size size :reframe build))
    :else (assoc spec :size size)))

(defn with-reframe
  "`spec`, unchanged, carrying :reframe (fn [view] (f spec view)): `f`
   rebuilds the authored spec for the wider scene window `view`. The
   authored spec is never re-derived, so its own output cannot drift."
  [spec f]
  (assoc spec :reframe (fn [view] (f spec view))))

(defn half-width
  "Half the width of `view`."
  ^double [[vw _]]
  (* 0.5 (double vw)))
