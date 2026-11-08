(ns tessera.compose.scenes
  "Scenes as data. Each is an EDN-able map compiled by
   tessera.compose.scene/->spec into a tessera.loops spec.

   kaleido-quasicrystal and fabric re-express the approved monoliths of
   tessera.vj and tessera.vj2 layer by layer (parity: test/tessera/compose_test)."
  (:require [tessera.compose.modulate :as m]))

(def kaleido-quasicrystal
  "vj/kaleido-shader as a scene: a stationary D8 fold through the lens
   rho = 3.2 log(1 + 2r), a five-wave field, an A2 net; seven ink layers."
  {:id :kaleido-quasicrystal-composed
   :title "D8 UV kaleidoscope, composed: five-wave contours through a log lens"
   :seconds 20 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 3
   :ground 0 :tone :neon
   :palette {:blue [0.025 0.10 1.0] :cyan [0.02 0.88 1.0] :magenta [1.0 0.025 0.93]
             :lime [0.40 1.0 0.02] :gold [1.0 0.61 0.015] :orange [1.0 0.10 0.015]
             :pale [0.10 0.85 1.0]}
   :nodes [{:type :phase :bind [[:pulse (m/beat 0.85 0.15 2)]]}
           {:type :polar}
           {:type :dihedral-fold :n 8}
           {:type :log-lens :a 3.2 :b 2.0}
           {:type :cartesian}
           {:type :plane-waves :n 5 :k 2.6 :scale 0.5}
           {:type :tri-lattice :scale 0.78}
           {:type :bead-chain :along :g1 :across :d2 :out :beads
            :freq 3.0 :offset 0.026 :width 0.032 :k 36.0}
           {:type :let
            :bind [[:level [:* 1.4 :field]]
                   [:primary [:* [:<= -1.0 [:floor :level]] [:<= [:floor :level] 1.0]]]
                   [:width [:if :primary 0.052 0.020]]
                   [:weight [:if :primary 1.18 0.30]]
                   [:contour [:- [:frac :level] 0.5]]
                   [:grid [:+ [:glow [:- :grid-d 0.030] 0.010]
                           [:* 0.65 [:glow [:- :grid-d 0.065] 0.008]]]]
                   [:meander [:glow [:- [:frac [:+ [:* 3.0 :d1] [:* 3.0 :d2]]] 0.5] 0.080]]
                   [:hatch [:* :meander [:min 1.0 [:* 7.0 :grid-d]]]]
                   [:ex [:- [:frac :g1] 0.5]]
                   [:ey [:- [:frac :g2] 0.5]]]}
           {:type :blob :at [:ex :ey] :out :eyes :k 850.0}
           {:type :radial-ring :r :r :theta :angle :out :petal
            :radius 0.235 :amp 0.026 :lobes 16 :width 0.012}
           {:type :let
            :bind [[:eye-rim [:* 0.5 [:glow [:- [:+ [:abs :ex] [:* 1.7 [:abs :ey]]] 0.10] 0.016]]]
                   [:centre [:+ [:* 1.8 :petal] [:* 1.2 [:glow [:- :r 0.148] 0.013]]]]
                   [:fade (m/fade-in :r 5.0 0.065)]
                   [:mask [:* (m/under 0.85 :hatch) (m/under :beads) (m/under :eyes)]]]}]
   :layers [{:ink :blue :value [:* :fade :mask 0.28 :grid]}
            {:ink :cyan :value [:* :fade :mask [:* :weight [:glow [:- :contour 0.085] :width]]]}
            {:ink :magenta :value [:* :fade :mask [:* :weight [:glow [:+ :contour 0.085] :width]]]}
            {:ink :lime :value [:* :fade :pulse 0.82 :hatch (m/under :beads) (m/under :eyes)]}
            {:ink :gold :value [:* :fade 1.40 :beads]}
            {:ink :orange :value [:* :fade [:+ [:* 1.5 :eyes] :eye-rim]]}
            {:ink :pale :value :centre}]})

(def tau (* 2.0 Math/PI))

(def fabric
  "vj2/fabric-shader as a scene: three painted layers, source-over on
   black. Isometric stepped pyramids at 30 Poisson sites, four serpents,
   and 100 Poisson sites of droplet bouquets, eyes, stars and bead trails
   whose bobbing and pulsing beats are integers."
  {:id :fabric-composed
   :title "UV fabric, composed: isometric relief, serpent ribbons and rainbow droplets"
   :seconds 16 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
   :ground 0
   :layers [{:paint :iso-pyramids
             :layout {:type :poisson :seed 8192 :count 30 :distance 0.99 :sort-by-y? true}
             :motif {:motif :stepped-cross} :tiers 3 :tone :uv
             :scale [:+ 0.78 [:* 0.48 [:frac [:* 0.61803398875 :i]]]]}
            {:paint :serpents :tone :uv :period 0.20
             :ribbons [[-1.4 0.30 0.52 2.7 :stripes] [1.1 -0.10 -0.67 2.9 :scales]
                       [-0.6 -1.15 0.12 2.8 :segments] [0.8 1.30 0.35 2.3 :scales]]}
            {:paint :sprites
             :layout {:type :poisson :seed 73129 :count 100 :distance 0.46}
             :kinds [{:kind :droplet-on-stem :bouquet [[0.21 -0.17 0.72 0.6] [-0.19 -0.12 0.62 -0.6]]}
                     {:kind :eye} {:kind :star4} {:kind :bead-chain}]
             :support 0.50
             :bind [[:phase [:* 2.399963229728653 :i]]
                    [:radius [:+ 0.080 [:* 0.095 [:frac [:* :i 0.61803398875]]]]]
                    [:bob [:* 0.045 [:sin [:+ [:* tau [:* [:+ [:mod :i 3] 1] :u]] :phase]]]]
                    [:pulse [:+ 0.84 [:* 0.14 [:wave [:+ [:* 2.0 :u] [:/ :phase tau]]]]]]]}]})

(def temple
  "New, purely from specs: a stationary D8 kaleidoscope of isometric
   stepped pyramids over seven-wave quasicrystal contours, with droplet
   bouquets the quasicrystal decides (visibility and size sampled per site)
   whose pulses ripple outward as bleuje delays u - r/4. A dashed petalled
   mandala ring sits at the centre. Nothing rotates; light moves."
  {:id :temple
   :title "Temple: D8 kaleidoscope of stepped pyramids over a quasicrystal, droplet bouquets"
   :seconds 16 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
   :ground 0 :tone :neon
   :palette {:magenta [1.0 0.025 0.93] :cyan [0.02 0.88 1.0] :pale [0.20 0.80 1.0]
             :gold [1.0 0.61 0.015]}
   :nodes [{:type :phase :bind [[:pulse (m/beat 0.75 0.25 2)]]}
           {:type :polar}
           {:type :dihedral-fold :n 8}
           {:type :cartesian :in [:r :angle]}
           {:type :plane-waves :n 7 :k 3.1 :scale 0.40 :dirs :half :speed 1}
           {:type :let
            :bind [[:fade (m/fade-in :r 2.5 0.07)]
                   [:level [:- [:frac [:* 1.6 :field]] 0.5]]
                   [:dash [:pow [:wave [:* 24.0 [:/ :theta tau]]] 3.0]]]}
           {:type :radial-ring :r :r :theta :theta :out :ring
            :radius 0.34 :amp 0.03 :lobes 16 :width 0.010}]
   :layers [{:ink :magenta :value [:* :fade 0.55 [:glow [:- :level 0.10] 0.035]]}
            {:ink :cyan :value [:* :fade 0.45 :pulse [:glow [:+ :level 0.10] 0.030]]}
            {:paint :iso-pyramids :at [:px :py] :tone :uv :tiers 3
             :motif {:motif :stepped-cross}
             :layout {:type :iso-grid :spacing 1.15 :extent [11.0 5.0]}
             :scale [:+ 0.50 [:* 0.30 [:frac [:* 0.61803398875 :i]]]]}
            {:paint :sprites :at [:px :py] :support 0.5
             :layout {:type :poisson :seed 4242 :count 70 :distance 0.42 :extent [9.4 3.8]}
             :kinds [{:kind :droplet-on-stem :bouquet [[0.17 -0.14 0.70 0.6] [-0.16 -0.10 0.60 -0.6]]}]
             :nodes [{:type :plane-waves :n 7 :k 3.1 :scale 0.40 :dirs :half :in [:x :y]}]
             :bind [[:phase [:* 2.399963229728653 :i]]
                    [:visible [:- :field -0.15]]
                    [:radius [:+ 0.055 [:* 0.030 [:clamp01 [:+ 0.5 :field]]]]]
                    [:pulse [:+ 0.55 [:* 0.40 (m/delayed 2 [:/ [:hypot :x :y] 4.0])]]]]}
            {:ink :pale :value [:* 1.4 :ring :dash]}
            {:ink :gold :value [:* 0.9 [:glow [:- :r 0.22] 0.012]]}]})

(def serpent-garden
  "New, purely from specs: a brightness-to-size sprite lattice of stepped
   crosses, chevrons and diamonds sized by the graphene band |f(k)| (Dirac
   points shrink to dust), slid one lattice cell's worth of band phase per
   loop; five serpents with all three skins wander over it; droplets whose
   pulses are bleuje delays f(u - d(x)), d a loop-noise field."
  {:id :serpent-garden
   :title "Serpent garden: graphene-sized glyph lattice, serpents, delayed droplets"
   :seconds 18 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
   :ground 0
   :layers [{:paint :sprites :support 0.45
             :layout {:type :lattice :spacing 0.42 :extent [9.0 5.2]}
             :kinds [{:kind :glyph :motif {:motif :stepped-cross}}
                     {:kind :glyph :motif {:motif :chevron}}
                     {:kind :glyph :motif {:motif :diamond :r 0.43}}]
             :nodes [{:type :graphene :a [1.48 0.55] :b [1.48 -1.71] :out :band}]
             :bind [[:kind-of [:mod [:floor [:* 2.380952 [:+ :y 3.0]]] 3]]
                    [:radius [:+ 0.03 [:* 0.30 [:pow [:/ :band 3.0] 1.25]]]]
                    [:phase [:* tau [:+ 0.55 [:* 0.30 [:sin [:+ [:* 0.9 :x] [:* 0.4 :y]]]]]]]
                    [:pulse [:+ 0.55 [:* 0.35 [:/ :band 3.0]]]]]}
            {:paint :serpents :tone :uv :period 0.22
             :ribbons [[-2.2 1.20 -0.35 2.4 :scales] [1.9 0.95 0.42 2.2 :stripes]
                       [-0.4 -0.10 0.08 3.1 :segments] [-2.0 -1.45 0.30 2.0 :stripes]
                       [2.1 -1.30 -0.50 2.3 :scales]]}
            {:paint :sprites :support 0.5
             :layout {:type :poisson :seed 9071 :count 28 :distance 0.95 :extent [8.6 4.6]}
             :kinds [{:kind :droplet-on-stem :bouquet [[0.19 -0.15 0.72 0.6] [-0.18 -0.11 0.62 -0.6]]}
                     {:kind :eye} {:kind :star4}]
             :nodes [{:type :loop-noise :freq 0.45 :radius 0.0 :seed 11 :out :delay}]
             :bind [[:phase [:* 2.399963229728653 :i]]
                    [:radius [:+ 0.075 [:* 0.06 [:frac [:* :i 0.61803398875]]]]]
                    [:bob [:* 0.035 [:sin [:+ [:* tau [:* [:+ [:mod :i 2] 1] :u]] :phase]]]]
                    [:pulse [:+ 0.45 [:* 0.50 (m/delayed 1 :delay)]]]]}]})

(def mandala-tunnel
  "New, purely from specs: the exact pinhole tunnel chart (one axial period
   per loop, a local sinusoidal shear, no spin) tiled with flower-mandala
   wedges: petalled double rings and diamonds in each cell, bead chains
   along the cell walls marching with the travel, quasicrystal hatching in
   the gaps, and a stationary dashed mandala at the vanishing point."
  {:id :mandala-tunnel
   :title "Mandala tunnel: petalled wedges, bead chains and hatching down a cylinder"
   :seconds 18 :fps 30 :size [1920 1080] :view [8.0 4.5] :ss 2
   :ground 0 :tone :neon
   :palette {:magenta [1.0 0.025 0.93] :blue [0.03 0.25 1.0] :lime [0.40 1.0 0.02]
             :gold [1.0 0.61 0.015] :cyan [0.02 0.88 1.0] :pale [0.30 0.80 1.0]}
   :nodes [{:type :polar}
           {:type :let :bind [[:rr [:max :r 1.0e-9]]]}
           {:type :tunnel-map :in [:rr :theta] :cols 12 :shear 0.10}
           {:type :lattice-cell :in [:s :t]}
           {:type :let
            :bind [[:open [:< 0.23 :r]]
                   [:fade [:* :open [:exp [:* -0.047 :z :z]]]]
                   [:cr [:hypot :a :b]]
                   [:cth [:atan2 :b :a]]
                   [:alt [:mod :i 2]]
                   [:aa [:abs :a]]
                   ;; t advances by -1 per loop, so u - t gains exactly 2.
                   [:travel (m/delayed 1 :t)]
                   [:dash [:pow [:wave [:* 24.0 [:/ :theta tau]]] 3.0]]]}
           {:type :radial-ring :r :cr :theta :cth :out :petal :radius 0.30 :amp 0.06 :lobes 8 :width 0.013}
           {:type :radial-ring :r :cr :theta :cth :out :petal2 :radius 0.18 :amp 0.03 :lobes 8 :width 0.010}
           {:type :bead-chain :along :t :across :aa :out :beads
            :freq 8.0 :offset 0.46 :width 0.020 :k 40.0}
           {:type :plane-waves :n 5 :k 9.0 :scale 0.5 :in [:a :b] :speed 0 :out :qc}
           {:type :radial-ring :r :r :theta :theta :out :ring :radius 0.17 :amp 0.0 :lobes 0 :width 0.007}]
   :layers [{:ink :magenta :value [:* :fade [:+ 0.7 [:* 0.3 :travel]] :petal]}
            {:ink :blue :value [:* :fade 0.9 :petal2]}
            {:ink :gold :value [:* :fade 1.2 :beads]}
            {:ink :lime :motif {:motif :diamond :r 0.07} :at [:a :b]
             :modulate [:fade [:- 1.0 :alt]]}
            {:ink :cyan :motif {:motif :diamond :r 0.07} :at [:a :b]
             :modulate [:fade :alt]}
            {:ink :lime :value [:* :fade 0.30 [:< 0.36 :cr]
                                [:glow [:- [:frac [:* 2.0 :qc]] 0.5] 0.10]]}
            {:ink :pale :value [:* 1.1 :ring :dash [:- 1.0 :open]]}]})
