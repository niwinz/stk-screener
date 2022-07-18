;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.fontawesome
  (:require
   [rumext.v2 :as mf]
   ["@fortawesome/free-solid-svg-icons" :as sfa]
   ["@fortawesome/fontawesome-svg-core" :as lfa]
   ["@fortawesome/react-fontawesome" :as fa]))

(defonce load-once
  (do
    (.add lfa/library sfa/fas)
    1))

(mf/defc icon
  [{:keys [name] :as props}]
  [:> fa/FontAwesomeIcon {:icon name}])
