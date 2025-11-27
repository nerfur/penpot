;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC


(ns app.main.ui.workspace.tokens.management.group
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.main.data.modal :as modal]
   [app.main.data.workspace.tokens.application :as dwta]
   [app.main.data.workspace.tokens.library-edit :as dwtl]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.context :as ctx]
   [app.main.ui.ds.foundations.assets.icon :as i :refer [icon*]]
   [app.main.ui.ds.layers.layer-button :refer [layer-button*]]
   [app.main.ui.workspace.tokens.management.token-pill :refer [token-pill*]]
   [app.util.dom :as dom]
   [cljs.pprint :as pp]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn- parse-token-path
  "Splits a token name into path segments"
  [token-name]
  (str/split token-name #"\."))

(defn- group-by-first-segment
  "Groups tokens by their first path segment."
  [tokens]
  (reduce (fn [acc token]
            (let [[first-segment & rest-segments] (parse-token-path (:name token))
                  rest-path (when (seq rest-segments) (str/join "." rest-segments))]
              (update acc first-segment (fnil conj [])
                      (if rest-path
                        (assoc token :name rest-path)
                        token))))
          {}
          tokens))

(defn- build-tree-node
  "Builds a single tree node with lazy children."
  [segment-name segment-tokens parent-path depth]
  (let [current-path (if parent-path
                       (str parent-path "." segment-name)
                       segment-name)
        is-leaf (every? (fn [token]
                  (let [path-segments (parse-token-path (:name token))
                    segment-count (count path-segments)]
                  (= 1 segment-count)))
                segment-tokens)
        leaf-token (when is-leaf (first segment-tokens))]
    {:name segment-name
     :path current-path
     :depth depth
     :is-token is-leaf
     :token leaf-token
     :has-children (not is-leaf)
     :children-fn (when-not is-leaf
                    (fn []
                      (let [grouped (group-by-first-segment segment-tokens)]
                        (mapv (fn [[name tokens]]
                                (build-tree-node name tokens current-path (inc depth)))
                              grouped))))}))

(defn- build-tree-root
  "Builds the root level of the tree."
  [tokens]
  (let [grouped (group-by-first-segment tokens)]
    (mapv (fn [[segment-name segment-tokens]]
            (build-tree-node segment-name segment-tokens nil 0))
          grouped)))

(defn token-section-icon
  [type]
  (case type
    :border-radius i/corner-radius
    :color i/drop
    :boolean i/boolean-difference
    :font-family i/text-font-family
    :font-size i/text-font-size
    :letter-spacing i/text-letterspacing
    :text-case i/text-mixed
    :text-decoration i/text-underlined
    :font-weight i/text-font-weight
    :typography i/text-typography
    :opacity i/percentage
    :number i/number
    :rotation i/rotation
    :spacing i/padding-extended
    :string i/text-mixed
    :stroke-width i/stroke-size
    :dimensions i/expand
    :sizing i/expand
    "add"))

(def ^:private schema:folder-node
  [:map
   [:node :any]
   [:selected-shapes :any]
   [:is-selected-inside-layout {:optional true} :boolean]
   [:active-theme-tokens {:optional true} :any]
   [:on-token-pill-click {:optional true} fn?]
   [:on-context-menu {:optional true} fn?]])

(mf/defc folder-node*
  {::mf/schema schema:folder-node}
  [{:keys [node selected-shapes is-selected-inside-layout active-theme-tokens on-token-pill-click on-context-menu]}]
  (let [expanded* (mf/use-state false)
        expanded (deref expanded*)
        swap-folder-expanded #(swap! expanded* not)
        _ (pp/pprint (:depth node))]
    [:li {:class (stl/css :folder-node)}
     [:> layer-button* {:label (:name node)
                        :expanded expanded
                        :is-expandable (:has-children node)
                        :on-toggle-expand swap-folder-expanded}]
     (when expanded
       (let [has-children (:has-children node)
             children-fn (:children-fn node)
             _ (pp/pprint {:action "Rendering folder children"
                           :node node})]
         [:div {:class (stl/css :folder-children-wrapper)}
         (when children-fn
           (let [children (children-fn)]
              (for [child children]
              ;;  (let [_ (pp/pprint "Rendering token pill")
              ;;        _ (pp/pprint {:token (:token child)})
              ;;        _ (pp/pprint {:selected-shapes selected-shapes})
              ;;        _ (pp/pprint {:is-selected-inside-layout is-selected-inside-layout})
              ;;        _ (pp/pprint {:active-theme-tokens active-theme-tokens})]
                (if (not (:is-token child))
                  [:ul {:class (stl/css :node-parent)}
                   [:> folder-node* {:key (:path child)
                                     :node child
                                     :selected-shapes selected-shapes
                                     :is-selected-inside-layout is-selected-inside-layout
                                     :active-theme-tokens active-theme-tokens
                                     :on-token-pill-click on-token-pill-click
                                     :on-context-menu on-context-menu}]]
                  [:> token-pill*
                   {:key (get-in child [:token :id])
                    :token (:token child)
                    :selected-shapes selected-shapes
                    :is-selected-inside-layout is-selected-inside-layout
                    :active-theme-tokens active-theme-tokens
                    :on-click on-token-pill-click
                    :on-context-menu on-context-menu}]))))]))]))

(def ^:private schema:token-tree
  [:map
   [:tokens :any]
   [:selected-shapes :any]
   [:is-selected-inside-layout {:optional true} :boolean]
   [:active-theme-tokens {:optional true} :any]
   [:on-token-pill-click {:optional true} fn?]
   [:on-context-menu {:optional true} fn?]])

(mf/defc token-tree*
  {::mf/schema schema:token-tree}
  [{:keys [tokens selected-shapes is-selected-inside-layout active-theme-tokens on-token-pill-click on-context-menu]}]
  (let [tree (build-tree-root tokens)]
    [:div {:class (stl/css :token-tree-wrapper)}
     (for [node tree]
       [:ul {:class (stl/css :node-parent)
             :key (:path node)
             :style {:padding-inline-start (* 16 (inc (:depth node)))}}
        (let [_ (pp/pprint {:node node})]
          (if (:is-token node)
          ;; Render token pill
            [:> token-pill*
             {:token (:token node)
              :selected-shapes selected-shapes
              :is-selected-inside-layout is-selected-inside-layout
              :active-theme-tokens active-theme-tokens
              :on-click on-token-pill-click
              :on-context-menu on-context-menu}]
          ;; Render segment folder
            [:> folder-node* {:node node
                              :selected-shapes selected-shapes
                              :is-selected-inside-layout is-selected-inside-layout
                              :active-theme-tokens active-theme-tokens
                              :on-token-pill-click on-token-pill-click
                              :on-context-menu on-context-menu}]))])]))

(def ^:private schema:token-group
  [:map
   [:type :keyword]
   [:tokens :any]
   [:selected-shapes :any]
   [:is-selected-inside-layout {:optional true} [:maybe :boolean]]
   [:active-theme-tokens {:optional true} :any]
   [:on-token-pill-click {:optional true} fn?]
   [:on-context-menu {:optional true} fn?]])

(mf/defc token-group*
   {::mf/schema schema:token-group}
  [{:keys [type tokens selected-shapes is-selected-inside-layout active-theme-tokens is-open selected-ids]}]
  (let [{:keys [modal title]}
        (get dwta/token-properties type)
        editing-ref  (mf/deref refs/workspace-editor-state)
        not-editing? (empty? editing-ref)

        is-open (d/nilv is-open false)

        can-edit?
        (mf/use-ctx ctx/can-edit?)

        is-selected-inside-layout (d/nilv is-selected-inside-layout false)

        tokens
        (mf/with-memo [tokens]
          (vec (sort-by :name tokens)))

        _ (pp/pprint {:tokens tokens :count (count tokens)})

        expandable? (d/nilv (seq tokens) false)

        on-context-menu
        (mf/use-fn
         (fn [event token]
           (dom/prevent-default event)
           (st/emit! (dwtl/assign-token-context-menu
                      {:type :token
                       :position (dom/get-client-position event)
                       :errors (:errors token)
                       :token-id (:id token)}))))

        on-toggle-open-click
        (mf/use-fn
         (mf/deps is-open type)
         #(st/emit! (dwtl/set-token-type-section-open type (not is-open))))

        on-popover-open-click
        (mf/use-fn
         (mf/deps type title modal)
         (fn [event]
           (dom/stop-propagation event)
           (st/emit! (dwtl/set-token-type-section-open type true)
                     (let [pos (dom/get-client-position event)]
                       (modal/show (:key modal)
                                   {:x (:x pos)
                                    :y (:y pos)
                                    :position :right
                                    :fields (:fields modal)
                                    :title title
                                    :action "create"
                                    :token-type type})))))

        on-token-pill-click
        (mf/use-fn
         (mf/deps not-editing? selected-ids)
         (fn [event token]
           (dom/stop-propagation event)
           (let [_ (pp/pprint "on-token-pill-click")
                 _ (pp/pprint {:token token :selected-ids selected-ids})]
             (when (and not-editing? (seq selected-shapes) (not= (:type token) :number))
               (st/emit! (dwta/toggle-token {:token token
                                             :shape-ids selected-ids}))))))]

    [:div {:class (stl/css :token-section-wrapper)}
     [:> layer-button* {:label title
                        :expanded is-open
                        :description (when expandable?(dm/str (count tokens)))
                        :is-expandable expandable?
                        :on-toggle-expand on-toggle-open-click
                        :icon (token-section-icon type)}
      [:> icon* {:icon-id (token-section-icon type)
                 :class (stl/css :token-section-icon)}]]
      ;; [:> icon* {:icon-id (token-section-icon type)
      ;;         :class (stl/css :token-section-icon)}]
      ;; [:span {:on-click on-toggle-open-click} title]
     (when is-open
       [:> token-tree* {:tokens tokens
                        :selected-shapes selected-shapes
                        :is-selected-inside-layout is-selected-inside-layout
                        :on-token-pill-click on-token-pill-click
                        :on-context-menu on-context-menu
                        :active-theme-tokens active-theme-tokens}])]))



#_[:div {:class (stl/css :token-pills-wrapper)}
   (for [token tokens]
     [:> token-pill*
      {:key (:name token)
       :token token
       :selected-shapes selected-shapes
       :is-selected-inside-layout is-selected-inside-layout
       :active-theme-tokens active-theme-tokens
       :on-click on-token-pill-click
       :on-context-menu on-context-menu}])]
