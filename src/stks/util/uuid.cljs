;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.uuid
  (:refer-clojure :exclude [next uuid zero?])
  (:require [stks.util.uuid-impl :as impl]
            [cljs.core :as c]))

(def zero #uuid "00000000-0000-0000-0000-000000000000")

(defn zero?
  [v]
  (= zero v))

(defn next
  []
  (impl/v1))

(defn random
  "Alias for clj-uuid/v4."
  []
  (impl/v4))

(defn uuid
  "Parse string uuid representation into proper UUID instance."
  [s]
  (c/uuid s))
