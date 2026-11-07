(ns tessera.main
  "Command line: render the loops.

     clojure -M:render --list
     clojure -M:render --preview                    ; a still + small GIF of each
     clojure -M:render --only girih-light,julia-orbit --format mp4 --size 1920x1080
     clojure -M:render --only quasicrystal --format webm --seconds 6 --out out/"
  (:require [clojure.string :as str]
            [desargues.wallpaper.render :as r]
            [tessera.loops :as loops]))

(defn- split-on
  "The pieces of string `s` between occurrences of char `c`, empty ones dropped."
  [s c]
  (->> s (partition-by #(= c %)) (remove #(= c (first %))) (mapv str/join)))

(defn parse-args
  "Options map of the command line `args`."
  [args]
  (loop [[a b & more :as xs] args opts {}]
    (cond
      (empty? xs) opts
      (= a "--list") (recur (rest xs) (assoc opts :list true))
      (= a "--preview") (recur (rest xs) (assoc opts :preview true))
      (= a "--only") (recur more (assoc opts :only (mapv keyword (split-on b \,))))
      (= a "--format") (recur more (assoc opts :format (keyword b)))
      (= a "--size") (recur more (assoc opts :size (mapv parse-long (split-on b \x))))
      (= a "--fps") (recur more (assoc opts :fps (parse-long b)))
      (= a "--seconds") (recur more (assoc opts :seconds (parse-double b)))
      (= a "--out") (recur more (assoc opts :out b))
      :else (throw (ex-info (str "Unknown argument " a) {:arg a})))))

(defn- progress [id]
  (fn [done total]
    (binding [*out* *err*]
      (print (format "\r  %-16s %4d / %d" (name id) done total))
      (flush))))

(defn- chosen [only]
  (if only
    (mapv #(or (loops/all %) (throw (ex-info (str "Unknown loop " %) {:id % :known (keys loops/all)}))) only)
    (vec (vals loops/all))))

(defn render!
  "Render `spec` per `opts`; returns the written paths."
  [spec {:keys [preview format size fps seconds out]}]
  (let [id (name (:id spec))
        prog (progress (:id spec))]
    (if preview
      [(r/still! spec 0.25 (str out "/" id ".png") {:size [960 540]})
       (r/encode! :gif spec (str out "/" id ".gif") {:size [480 270] :fps 15 :progress prog})]
      (let [fmt (or format :mp4)]
        [(r/encode! fmt spec (str out "/" id "." (name fmt))
                    (cond-> {:progress prog}
                      size (assoc :size size)
                      fps (assoc :fps fps)
                      seconds (assoc :seconds seconds)))]))))

(defn -main [& args]
  (let [{:keys [list only] :as opts} (update (parse-args args) :out #(or % "out"))]
    (if list
      (doseq [{:keys [id title seconds fps]} (vals loops/all)]
        (println (format "%-16s %3ss @ %dfps  %s" (name id) seconds fps title)))
      (doseq [spec (chosen only)]
        (let [t0 (System/nanoTime)
              paths (render! spec opts)]
          (binding [*out* *err*] (println))
          (println (format "%-16s %6.1fs  %s" (name (:id spec)) (/ (- (System/nanoTime) t0) 1e9)
                           (str/join " " paths)))))))
  (shutdown-agents))
