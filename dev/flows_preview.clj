(ns flows-preview
  "Sequential 640x360 previews. Load into an existing :test REPL, then call
   (flows-preview/render!). No extra JVM or parallel frame batches."
  (:require [clojure.java.io :as io]
            [desargues.wallpaper.render :as r]
            [tessera.flows :as f])
  (:import (java.awt.image BufferedImage)))

(defn rgb-bytes ^bytes [^BufferedImage img]
  (let [w (.getWidth img) h (.getHeight img)
        px (.getRGB img 0 0 w h nil 0 w) out (byte-array (* 3 w h))]
    (dotimes [i (alength px)]
      (let [c (aget px i) j (* 3 i)]
        (aset-byte out j (unchecked-byte (bit-shift-right c 16)))
        (aset-byte out (inc j) (unchecked-byte (bit-shift-right c 8)))
        (aset-byte out (+ j 2) (unchecked-byte c))))
    out))

(defn render!
  "Write complete low-fps MP4 loops, one frame at a time. Encode to a partial
   path, then rename on success, so an unfinished 48-byte MP4 is never a preview.
   Default 5 fps preserves the native 18/20-second durations."
  ([] (render! 5 (atom nil)))
  ([fps progress]
   (let [dir (io/file "out/preview")]
     (.mkdirs dir)
     (doseq [[id spec] f/loops]
       (let [out (io/file dir (str (name id) ".mp4"))
             partial (io/file dir (str (name id) ".partial.mp4"))
             args (r/ffmpeg-args :mp4 640 360 fps partial {:crf 19})
             args (into (vec (butlast args)) ["-threads" "2" (last args)])
             proc (.start (doto (ProcessBuilder. ^java.util.List args)
                            (.redirectError (io/file dir (str (name id) "-ffmpeg.log")))))
             n (* fps (:seconds spec)) opts {:size [640 360] :fps fps}]
         (try
           (with-open [stream (.getOutputStream proc)]
             (dotimes [i n]
               (reset! progress {:loop id :done i :total n})
               (let [img (r/render-frame spec (/ (double i) n) opts)]
                 (.write stream ^bytes (rgb-bytes img)))))
           (let [code (.waitFor proc)]
             (when-not (zero? code)
               (throw (ex-info "Preview encoder failed" {:loop id :exit code})))
             (when (< (.length partial) 10000)
               (throw (ex-info "Preview unexpectedly empty" {:loop id :bytes (.length partial)})))
             (java.nio.file.Files/move (.toPath partial) (.toPath out)
                                      (into-array java.nio.file.CopyOption
                                                  [java.nio.file.StandardCopyOption/REPLACE_EXISTING])))
           (finally
             (when (.isAlive proc) (.destroy proc))))))
     (reset! progress {:status :done :fps fps :size [640 360]}))))
