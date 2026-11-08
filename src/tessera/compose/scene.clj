(ns tessera.compose.scene
  "The scene spec: schema, validation and compilation to a loop spec.

   A scene is EDN data: an id, timing, a ground and a palette, shared
   :nodes (charts, fields, phase constants, per-pixel bindings) and a
   bottom-up :layers stack. A layer is an INK layer (a motif stroke or an
   expression, times modulators, in one palette colour; consecutive ink
   layers are tone-mapped together) or a PAINT layer (a painter: sprites,
   blocks, ribbons, drawn source-over)."
  (:require [malli.core :as mc]
            [malli.error :as me]
            [tessera.compose.render :as cr]
            [tessera.compose.layout]
            [tessera.compose.motif]
            [tessera.compose.modulate]))

(def Expr
  "number | slot keyword | [op-keyword expr ..] (see tessera.compose.core/expr)."
  [:schema {:registry {::expr [:or number? keyword?
                               [:and [:vector {:min 1} [:ref ::expr]]
                                [:fn {:error/message "an expression vector starts with an op keyword"}
                                 #(keyword? (first %))]]]}}
   [:ref ::expr]])

(def Node
  [:map {:closed false} [:type keyword?]])

(def Ink
  [:or keyword? [:tuple number? number? number?]])

(def Layer
  [:or
   [:map {:closed false}
    [:ink Ink]
    [:motif {:optional true} [:map {:closed false} [:motif keyword?]]]
    [:at {:optional true} [:tuple keyword? keyword?]]
    [:layout {:optional true} [:vector Node]]
    [:nodes {:optional true} [:vector Node]]
    [:modulate {:optional true} [:vector Expr]]
    [:value {:optional true} Expr]
    [:blend {:optional true} [:enum :screen :add :over]]
    [:tone {:optional true} [:or keyword? map?]]]
   [:map {:closed false}
    [:paint keyword?]]])

(def Scene
  "Malli schema of a composed scene."
  [:map {:closed false}
   [:id keyword?]
   [:title {:optional true} string?]
   [:seconds pos-int?]
   [:fps pos-int?]
   [:size {:optional true} [:tuple pos-int? pos-int?]]
   [:view {:optional true} [:tuple number? number?]]
   [:ss {:optional true} pos-int?]
   [:ground {:optional true} int?]
   [:tone {:optional true} [:or keyword? map?]]
   [:palette {:optional true} [:map-of keyword? [:tuple number? number? number?]]]
   [:nodes {:optional true} [:vector Node]]
   [:layers [:vector {:min 1} Layer]]])

(defn explain
  "Humanised schema errors of `scene`, or nil when valid."
  [scene]
  (some-> (mc/explain Scene scene) me/humanize))

(defn ->spec
  "Compile `scene` to the loop spec tessera.loops expects: the timing keys
   it carries plus :field. Throws on an invalid scene."
  [scene]
  (when-let [e (explain scene)]
    (throw (ex-info "Invalid scene" {:id (:id scene) :errors e})))
  (let [field (cr/compile-scene scene)]
    (-> (select-keys scene [:id :title :seconds :fps :size :view :ss :post :blur])
        (merge {:size [1920 1080] :view [8.0 4.5] :ss 2} (select-keys scene [:size :view :ss]))
        (assoc :field field :scene-spec scene))))
