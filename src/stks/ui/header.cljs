;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui.header
  (:require
   [cuerdas.core :as str]
   [potok.core :as ptk]
   [rumext.v2 :as mf]
   [stks.events]
   [stks.store :as st]
   [stks.ui.messages :as ms]
   [stks.util.data :as d]
   [stks.util.fontawesome :as fa]))

(mf/defc header
  [{:keys [nav] :as props}]
  (let [logout         #(st/emit! (ptk/event :logout))
        nav-symbols    #(st/emit! (ptk/event :nav {:section :symbols}))
        nav-strategies #(st/emit! (ptk/event :nav {:section :strategies}))
        nav-dashboard  #(st/emit! (ptk/event :nav {:section :dashboard}))]

    [:*
     [:& ms/messages]

     [:div.header
      [:div.section-title
       [:div.breadcrumb
        [:span "Screener"]
        [:span " / "]
        [:span (case (:section nav)
                 :symbols "Symbols"
                 :dashboard "Dashboard"
                 :strategies "Strategies"
                 :auth "Authentication")]]]
      (when-let [{:keys [section]} nav]
        [:nav
         (when (not= :auth section)
           [:ul
            [:li {:on-click nav-dashboard
                  :class (when (= :dashboard section) "active")}
             "Dashboard"]

            [:li {:on-click nav-strategies
                  :class (when (= :strategies section) "active")}
             "Strategies"]

            [:li {:on-click nav-symbols
                  :class (when (= :symbols section) "active")}
             "Symbols"]

            [:li {:on-click logout} "<- Quit"]])])]]))
