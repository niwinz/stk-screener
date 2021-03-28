;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui.dashboard
  (:require
   [cuerdas.core :as str]
   [rumext.alpha :as mf]
   [potok.core :as ptk]
   [stks.events]
   [stks.store :as st]
   [stks.ui.messages :as ms]
   [stks.util.data :as d]
   [stks.util.fontawesome :as fa]))

(mf/defc dashboard-section
  [props]
  (let [symbols (mf/deref st/symbols-ref)]
    [:div.content
     [:div.table.symbols-table
      [:div.table-header
       [:div.table-column.symbol "Symbol"]
       [:div.table-column.signal "S"]
       [:div.table-column.indicator "M5"]
       [:div.table-column.indicator "M30"]
       [:div.table-column.indicator "H4"]
       [:div.table-column.indicator "D1"]
       [:div.table-column.time "Time"]]

    [:hr]

    [:h2 "Active" #_[:& fa/icon {:name "ambulance"}]]
    [:div.table-body
     (for [item (->> (vals symbols)
                     (sort-by :name))]
       [:div.table-row {:key (:id item)}
        [:div.table-column.symbol {:title (:desc item)} (:name item)]
        [:div.table-column.signal "-"]
        [:div.table-column.indicator "↓"]
        [:div.table-column.indicator "⇊"]
        [:div.table-column.indicator "⇊"]
        [:div.table-column.indicator "↓"]
        [:div.table-column.time "2 hours"]])]]]))

      ;; [:hr]

      ;; [:h2 "Inactive"]
      ;; [:div.table-body
      ;;  (for [i (range 3)]
      ;;    [:div.table-row {:key i :class (when (= 1 i) "viewed")}
      ;;     [:div.table-column.symbol (str "EUR/JPY_" i)]
      ;;     [:div.table-column.indicator "↑"]
      ;;     [:div.table-column.indicator "⇈"]
      ;;     [:div.table-column.indicator "⇈"]
      ;;     [:div.table-column.time "2 hours"]])]]])


