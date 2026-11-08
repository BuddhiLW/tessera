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
