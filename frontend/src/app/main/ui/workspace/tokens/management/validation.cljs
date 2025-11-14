;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.tokens.management.validation
  (:require
   [app.common.files.tokens :as cft]
   [app.common.schema :as sm]
   [app.common.types.token :as cto]
   [app.util.i18n :refer [tr]]))


;; Schemas ---------------------------------------------------------------------

(defn- make-token-name-schema
  "Generate a dynamic schema validation to check if a token path derived
  from the name already exists at `tokens-tree`."
  [tokens-tree]
  [:and
   [:string {:min 1 :max 255 :error/fn #(str (:value %) (tr "workspace.tokens.token-name-length-validation-error"))}]
   (sm/update-properties cto/token-name assoc :error/fn #(str (:value %) (tr "workspace.tokens.token-name-validation-error")))
   [:fn {:error/fn #(tr "workspace.tokens.token-name-duplication-validation-error" (:value %))}
    #(not (cft/token-name-path-exists? % tokens-tree))]])

(defn validate-token-name
  [tokens-tree name]
  (let [schema    (make-token-name-schema tokens-tree)
        explainer (sm/explainer schema)]
    (-> name explainer sm/simplify not-empty)))

(def ^:private schema:token-description
  [:string {:max 2048 :error/fn #(tr "errors.field-max-length" 2048)}])

(def validate-token-description
  (let [explainer (sm/lazy-explainer schema:token-description)]
    (fn [description]
      (-> description explainer sm/simplify not-empty))))
