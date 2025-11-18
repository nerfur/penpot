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
   [app.common.types.tokens-lib :as ctob]
   [app.plugins.utils :as u]
   [app.util.i18n :refer [tr]]))

;; TODO: those validations should be moved to the common module. For this, we need to have
;; a way to pass a no-op translation function when called from backend or tests.

(defn- make-token-name-schema
  "Generates a dynamic schema to check a token name:
    - Validate name length.
    - Adds a i18n error message to the schema that checks if the name is not well-formed.
    - Checks if other token with a path derived from the name already exists at `tokens-tree`."
  [tokens-tree]
  [:and
   [:string {:min 1 :max 255 :error/fn #(str (:value %) (tr "workspace.tokens.token-name-length-validation-error"))}]
   (sm/update-properties cto/schema:token-name assoc :error/fn #(str (:value %) (tr "workspace.tokens.token-name-validation-error")))
   [:fn {:error/fn #(tr "workspace.tokens.token-name-duplication-validation-error" (:value %))}
    #(not (cft/token-name-path-exists? % tokens-tree))]])

(defn validate-token-name
  "Validates a token name. If valid, returns nil. If not, returns a list of 18n'ed error messages."
  [tokens-tree name]
  (u/validate-with-schema name (make-token-name-schema tokens-tree)))

(def ^:private schema:token-description
  [:string {:max 2048 :error/fn #(tr "errors.field-max-length" 2048)}])

(defn validate-token-description
  "Validates a token description If valid, returns nil. If not, returns a list of 18n'ed error messages."
  [description]
  (u/validate-with-schema description schema:token-description))

(defn- make-token-set-name-schema
  "Generates a dynamic schema to check a token set name:
    - Validate name length.
    - Adds a i18n error message to the schema that checks if the name is not well-formed.
    - Checks if other token set with a path derived from the name already exists at `tokens-tree`."
  [tokens-lib set-id]
  [:and
   [:string {:min 1 :max 255 :error/fn #(str (:value %) (tr "workspace.tokens.token-name-length-validation-error"))}]
   [:fn {:error/fn #(tr "errors.token-set-already-exists" (:value %))}
    (fn [name]
      (let [set (ctob/get-set-by-name tokens-lib name)]
        (or (nil? set) (= (:id set) set-id))))]])

(defn validate-token-set-name
  "Validates a token set name. If valid, returns nil. If not, returns a list of 18n'ed error messages."
  [tokens-lib set-id name]
  (u/validate-with-schema name (make-token-set-name-schema tokens-lib set-id)))
