;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui.screener
  (:require
   [cuerdas.core :as str]
   [potok.core :as ptk]
   [rumext.v2 :as mf]
   [stks.events]
   [stks.store :as st]
   [stks.ui.messages :as ms]
   [stks.util.data :as d]
   [stks.util.data :as d]
   [stks.util.fontawesome :as fa]
   [stks.util.time :as dt]
   [stks.util.timers :as tm]
   [stks.util.webapi :as wa]))

(mf/defc screener-section
  [props]
  [:section.screener
   [:div.table-header
    ]
   [:div.table-body
    (for [i (range 10)]
      [:div.screener-item {:key i}
       [:div.symbol-name "EUR/USD"]

       [:div.timings
        [:div.timing.m5-timing
         [:span.direction "M5:"]
         [:span.indicator "1"]]
        [:div.timing.h1-timing
         [:span.direction "H1:"]
         [:span.indicator "2"]]
        [:div.timing.d1-timing
         [:span.direction "D1:"]
         [:span.indicator "-4"]]]

       [:div.signals
        [:span "⇈ macd-h1"]
        [:span "⇊ macd-m5"]]
       [:div.spacer]



       ;; [:div.timing.m5-timing
       ;;  [:span.direction "⇈"]
       ;;  [:span.indicator "(1)"]]
       ;; [:div.timing.h1-timing
       ;;  [:span.direction "⇈"]
       ;;  [:span.indicator "(1)"]]
       ;; [:div.timing.d1-timing
       ;;  [:span.direction "⇊"]
       ;;  [:span.indicator "(3)"]]


       ])

    ]])


