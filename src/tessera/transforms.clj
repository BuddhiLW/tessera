(ns tessera.transforms
  "Loop-locked, single-input trama effects. UV/coverage maps are field data,
   compiled by the same desargues WGSL backend as tessera.layers sources.
   Slots are append-only: loop_rate at 0, then :params in declaration order.
   Texture sampling stays in this adapter, not in the scalar field DSL."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [desargues.wallpaper.field :as field]
            [desargues.wallpaper.trama :as trama]
            [desargues.wallpaper.wgsl :as wgsl]))

(defn- param [s default lo hi]
  [s {:default default :min lo :max hi :label (name s)}])

(def torus
  {:id :tas_torus :title "Tessera Torus" :period 16
   :description "Input-textured log-polar tunnel; mirrored depth, integer flight and rotation cycles. Not the procedural Tunnel source."
   :params [(param 'twist 0.25 -2.0 2.0) (param 'speed 1.0 0.0 4.0)
            (param 'depth_scale 0.7 0.1 3.0) (param 'rotation 1.0 -4.0 4.0)
            (param 'core 0.02 0.002 0.2)]
   :integers '[speed rotation]
   :form {:params '[u aspect twist speed depth_scale rotation core]
          :body '(let [px (* (- x 0.5) aspect) py (- y 0.5)
                       r (sqrt (+ (* px px) (* py py) 0.00000001))
                       z (* (- (log (max r core))) depth_scale)
                       a (+ (/ (atan2 py (+ px 0.00000001)) tau)
                            (* twist z) (* (floor (+ rotation 0.5)) u))
                       v (+ z (* (floor (+ speed 0.5)) u))]
                   [(- 1.0 (abs (- (* 2.0 (fract a)) 1.0)))
                    (- 1.0 (abs (- (* 2.0 (fract v)) 1.0))) 1.0])}})

(def hyperspace
  {:id :tas_hyperspace :title "Tessera Hyperspace" :period 16 :streak? true
   :description "Travelling radial compression wave and 12-tap outward streaks of the input; unlike static Zoom Blur."
   :params [(param 'z_speed 2.0 -4.0 4.0) (param 'warp 0.18 0.0 0.6)
            (param 'density 4.0 0.25 12.0) (param 'streak_length 0.2 0.0 0.8)]
   :integers '[z_speed]
   :form {:params '[u aspect z_speed warp density streak_length]
          :body '(let [px (- x 0.5) py (- y 0.5)
                       r (sqrt (+ (* px px aspect aspect) (* py py) 0.00000001))
                       wave (sin (* tau (- (* density r) (* (floor (+ z_speed 0.5)) u))))
                       scale (exp (* (- warp) wave))]
                   [(+ 0.5 (* px scale)) (+ 0.5 (* py scale)) 1.0])}})

(def tile
  {:id :tas_tile :title "Tessera Stagger Tile" :period 16
   :description "Rectangular n by m mirrored repeat with row offset and loop-locked row sway; use builtin Tile for a simple square grid."
   :params [(param 'columns 4.0 1.0 16.0) (param 'rows 3.0 1.0 16.0)
            (param 'row_offset 0.5 -1.0 1.0) (param 'sway 0.15 0.0 1.0)
            (param 'cycles 1.0 -4.0 4.0)]
   :integers '[columns rows cycles]
   :form {:params '[u aspect columns rows row_offset sway cycles]
          :body '(let [n (max 1.0 (floor (+ columns 0.5)))
                       m (max 1.0 (floor (+ rows 0.5)))
                       row (floor (* y m))
                       phase (+ (* (floor (+ cycles 0.5)) u) (* row 0.25))
                       shift (+ (* row row_offset) (* sway (sin (* tau phase))))]
                   [(+ (* x n) shift) (* y m) 1.0])}})

(def suite
  "Input effects in stable publication order; separate from source layers."
  [torus hyperspace tile])

(defn check
  "Validate the existing trama slot/range contract plus the UV-map ABI.
   Hidden aspect and optional height are supplied by the texture adapter."
  [effect]
  (let [public (mapv first (:params effect))
        expected (into '[u aspect] (cond-> public (:height? effect) (conj 'height)))
        candidate (assoc effect :form (assoc (:form effect) :params (into '[u] public)))
        checked (trama/check candidate)]
    (when (:err checked)
      (throw (ex-info "Invalid input effect" checked)))
    (when-not (and (= expected (get-in effect [:form :params]))
                   (every? (set public) (:integers effect))
                   (vector? (:params effect)))
      (throw (ex-info "Invalid UV-map parameter ABI" {:id (:id effect) :expected expected})))
    (field/lower (:form effect))
    (assoc (:ok checked) :form (:form effect))))

(defn manifest
  "Fosfora's strict trama manifest: effect, one input, one integrated clock."
  [effect]
  (let [e (check effect)]
    (assoc (trama/manifest e) "kind" "effect" "inputs" 1
           "integers" (mapv name (:integers e)))))

(defn- sample-text [effect]
  (if (:streak? effect)
    (str "  let strength = clamp(param(4u), 0.0, 0.8);\n"
         "  var acc = vec4f(0.0);\n"
         "  for (var i = 0u; i < 12u; i++) {\n"
         "    let t = f32(i) / 11.0;\n"
         "    let q = vec2f(0.5) + (mapped.xy - vec2f(0.5)) * exp(-strength * t);\n"
         "    acc += tas_sample(q);\n"
         "  }\n"
         "  return acc / 12.0;\n")
    "  return tas_sample(mapped.xy) * clamp(mapped.z, 0.0, 1.0);\n"))

(defn source-text
  "Pure WGSL packaging. The field returns UV and coverage, not a colour.
   input0 is premultiplied RGBA; coverage multiplies all four channels.
   Mirrored extension avoids hard wrap seams for arbitrary input pictures."
  [effect]
  (let [e (check effect)
        compiled (field/source :wgsl (:form e) {})
        _ (when-not (:ok compiled) (throw (ex-info "WGSL compilation failed" compiled)))
        src (:ok compiled)
        slot (trama/slots e)
        args (map (fn [p]
                    (case p
                      u "tas_phase" aspect "tas_aspect" height "tas_height"
                      (str "param(" (slot p) "u)")))
                  (get-in e [:form :params]))]
    (str "/*! trama\n" (json/write-str (manifest e) :indent true) "\n*/\n"
         "// " (:description e) "\n"
         "// loop_rate is an integral; cycle counts are rounded, never free-running.\n"
         (:helpers src) "\n" (:function src) "\n"
         "fn tas_sample(p: vec2f) -> vec4f {\n"
         "  let q = vec2f(1.0) - abs(vec2f(1.0) - 2.0 * fract(p * 0.5));\n"
         "  let h = 0.5 / u.resolution;\n"
         "  return input0(clamp(q, h, vec2f(1.0) - h));\n}\n"
         "@fragment\nfn fs_main(@builtin(position) coord: vec4f) -> @location(0) vec4f {\n"
         "  let uv = coord.xy / u.resolution;\n"
         "  let tas_aspect = u.resolution.x / u.resolution.y;\n"
         "  let tas_phase = fract(param(0u) / " (wgsl/literal (:period e)) ");\n"
         (when (:height? e)
           "  let tas_height = clamp(dot(tas_sample(uv).rgb, vec3f(0.2126, 0.7152, 0.0722)), 0.0, 1.0);\n")
         "  let mapped = desargues_field(uv.x, uv.y, " (str/join ", " args) ");\n"
         (sample-text e) "}\n")))

(def validation-preamble
  "Minimal host ABI fixture for naga. Sampling uses real texture/sampler
   bindings and explicit LOD, as trama input0 does; not a constant stub."
  (str "struct TasHost { resolution: vec2f, time: f32, padding: f32, values: array<vec4f, 4>, }\n"
       "@group(0) @binding(0) var<uniform> u: TasHost;\n"
       "@group(0) @binding(1) var tas_texture: texture_2d<f32>;\n"
       "@group(0) @binding(2) var tas_sampler: sampler;\n"
       "fn param(i: u32) -> f32 { return u.values[i / 4u][i % 4u]; }\n"
       "fn input0(p: vec2f) -> vec4f { return textureSampleLevel(tas_texture, tas_sampler, p, 0.0); }\n"))

(defn validate-wgsl!
  "Naga validation with a 20s process bound. Missing naga is an error,
   never a silent skip. NAGA may name an alternate executable."
  [text]
  (let [f (java.io.File/createTempFile "tas-validate-" ".wgsl")]
    (try
      (spit f (str validation-preamble text))
      (let [r (shell/sh "timeout" "20" (or (System/getenv "NAGA") "naga") (str f))]
        (when-not (zero? (:exit r))
          (throw (ex-info "Naga rejected input effect" r)))
        {:validator :naga :exit 0})
      (finally (.delete f)))))

(defn emit-trama!
  "Emit validated input effects beside source nodes (normally out/layers).
   Like desargues emitters, returns {id {:ok [path]}}; failures are explicit.
   Validate the entire batch before writing anything."
  ([dir] (emit-trama! suite dir))
  ([effects dir]
   (let [files (mapv (fn [e]
                       (let [text (source-text e)]
                         (validate-wgsl! text)
                         [(str (name (:id e)) ".wgsl") text (:id e)])) effects)]
     (.mkdirs (io/file dir))
     (into (array-map)
           (for [[n text id] files :let [f (io/file dir n)]]
             (do (spit f text) [id {:ok [(str f)]}]))))))
