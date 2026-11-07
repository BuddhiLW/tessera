(ns tessera.palette
  "Colour for wallpaper loops: cosine palettes as primitive fns t -> 0xRRGGBB,
   and the tile colours of the girih loops."
  (:require [desargues.wallpaper.loop :as lp]
            [desargues.wallpaper.render :as r]
            [thi.ng.color.gradients :as grad]))

(set! *warn-on-reflection* true)

(defn cosine
  "Inigo Quilez's palette a + b·cos 2π(c·t + d), per channel, as a primitive
   fn t -> 0xRRGGBB. Each argument is [r g b]."
  [[ar ag ab] [br bg bb] [cr cg cb] [dr dg db]]
  (let [ar (double ar) ag (double ag) ab (double ab)
        br (double br) bg (double bg) bb (double bb)
        cr (double cr) cg (double cg) cb (double cb)
        dr (double dr) dg (double dg) db (double db)]
    (fn ^long [^double t]
      (r/rgb (+ ar (* br (Math/cos (* lp/tau (+ (* cr t) dr)))))
             (+ ag (* bg (Math/cos (* lp/tau (+ (* cg t) dg)))))
             (+ ab (* bb (Math/cos (* lp/tau (+ (* cb t) db)))))))))

(defn at
  "Palette `p` at `t`."
  ^long [p ^double t]
  (.invokePrim ^clojure.lang.IFn$DL p t))

(defn scheme
  "A thi.ng/color cosine-gradient scheme (a key of
   thi.ng.color.gradients/cosine-schemes: :blue-magenta-orange, :rainbow1 ..)
   as a primitive palette t -> 0xRRGGBB."
  [k]
  (let [[a b c d] (or (get grad/cosine-schemes k)
                      (throw (ex-info (str "Unknown thi.ng scheme " k) {:scheme k :known (keys grad/cosine-schemes)})))]
    (cosine a b c d)))

(defn around
  "Palette `p` walked there and back once per unit of t: t -> p(1/2 - cos(2πt)/2).
   Periodic in t for any palette, so cycling it by the loop phase never seams."
  [p]
  (fn ^long [^double t]
    (at p (- 0.5 (* 0.5 (Math/cos (* lp/tau t)))))))

(def lapis-gold
  "Lapis, turquoise and gold, the tile colours of Isfahan and Bukhara."
  (cosine [0.32 0.38 0.48] [0.32 0.32 0.30] [1.0 1.0 1.0] [0.05 0.15 0.27]))

(def ember
  "Deep blue through violet to ember gold. Integer frequencies, so shifting
   t by 1 returns every colour."
  (cosine [0.40 0.28 0.32] [0.40 0.30 0.32] [1.0 1.0 1.0] [0.0 0.12 0.30]))

(def ink "#06101e")

(def girih
  "Ground, strand, ink and rosette fill by polygon kind."
  {:ground   "#0b1f3a"
   :strand   "#d9a441"
   :ink      ink
   :octagon  "#1f6f8b"
   :hexagon  "#1f6f8b"
   :square   "#a4462f"
   :triangle "#a4462f"})
