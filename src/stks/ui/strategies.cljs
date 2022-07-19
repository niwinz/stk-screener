;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui.strategies
  (:require
   [beicon.core :as rx]
   [cuerdas.core :as str]
   [potok.core :as ptk]
   [rumext.v2 :as mf]
   [stks.events.strategies :refer [available-strategies]]
   [stks.events]
   [stks.repo :as rp]
   [stks.store :as st]
   [stks.ui.messages :as ms]
   [stks.util.data :as d]
   [stks.util.fontawesome :as fa]
   [stks.util.webapi :as wa]))

(extend-protocol cljs.core/INamed
  string
  (-name [s] s)
  (-namespace [s] ""))

(mf/defc strategy-item
  [{:keys [item] :as props}]
  (let [nav (mf/deref st/nav-ref)
        id  (:id item)

        on-click
        (mf/use-callback
         (mf/deps item)
         (fn []
           (st/emit! (ptk/event :toggle-strategy item))))]

    [:fieldset.strategy-item
     {:on-click on-click
      :class (wa/classnames
              :active (contains? (:strategies nav) id))}
     [:legend "Strategy: " (str (:id item))]

     [:div.field
      [:label "Main TF: "]
      [:span (str/upper (name (:main item)))]]

     [:div.field
      [:label "Reference TF: "]
      [:span (str/upper (name (:reference item)))]]]))

(mf/defc strategies-section
  [props]
  [:section.strategies-section
   [:div.strategies-list
    (for [item available-strategies]
      [:& strategy-item {:item item :key (:id item)}])]])
