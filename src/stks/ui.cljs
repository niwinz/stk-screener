;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui
  (:require
   [cuerdas.core :as str]
   [potok.core :as ptk]
   [rumext.alpha :as mf]
   [stks.events]
   [stks.store :as st]
   [stks.ui.auth :refer [auth-section]]
   [stks.ui.dashboard :refer [dashboard-section]]
   [stks.ui.header :refer [header]]
   [stks.ui.messages :as ms]
   [stks.ui.symbols :refer [symbols-section]]
   [stks.util.data :as d]
   [stks.util.fontawesome :as fa]))

(mf/defc app
  [props]
  (let [nav (mf/deref st/nav-ref)]
    (mf/use-effect
     (mf/deps)
     (fn []
       (st/emit! (ptk/event :initialize))
       (constantly nil)))

    (when nav
      [:main
       [:& header {:nav nav}]
       (case (:section nav)
         :auth      [:& auth-section]
         :symbols   [:& symbols-section]
         :dashboard [:& dashboard-section]
         [:span "not found"])])))
