;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh

(ns stks.config
  (:require
   [clojure.spec.alpha :as s]))

(def public-uri (.-origin ^js js/location))
