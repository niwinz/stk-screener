;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui.strategies
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

(mf/defc symbol-item
  [{:keys [symbol-id strategy-id created-at updated-at direction inactive]}]
  (let [symbols       (mf/deref st/symbols-ref)
        symbol-name   (get-in symbols [symbol-id :name])
        toggle-status (mf/with-memo [symbol-id strategy-id]
                        (fn []
                          (st/emit! #(update-in % [:signals symbol-id strategy-id :inactive] not))))]

    [:div.symbol-entry
     {:key (str/concat symbol-id)
      :style {:user-select "none"}
      :on-click toggle-status
      :class (wa/classnames
              :direction-up (= :up direction)
              :direction-down (= :down direction)
              :inactive inactive)}
     [:div.id (or symbol-name symbol-id)]
     [:div.dir (case direction :up "⇈" :down "⇊")]
     [:div.age (dt/age created-at (dt/now))]]))

(mf/defc strategy-item
  {::mf/wrap [mf/memo]}
  [{:keys [strategy-id] :as props}]
  (let [symbols (mf/deref st/selected-symbols-ref)
        signals (mf/deref st/signals-ref)
        render  (mf/use-state 0)]

    (mf/with-effect
      (tm/repeat 2000 #(swap! render inc)))

    [:*
     [:fieldset.strategy-item
      [:legend (name strategy-id)]

      (for [{symbol-id :id} (sort-by :name symbols)]
        (when-let [data (get-in signals [symbol-id strategy-id])]
          [:& symbol-item {:key (str/concat symbol-id)
                           :symbol-id symbol-id
                           :strategy-id strategy-id
                           :created-at (:created-at data)
                           :updated-at (:updated-at data)
                           :inactive (:inactive data)
                           :direction (:dir data)}]))]]))

(mf/defc strategies-section
  [props]
  (let [strategies (mf/deref st/strategies-ref)
        signals    (mf/deref st/signals-ref)]
    [:section.strategies
     (for [id (sort strategies)]
       [:& strategy-item
        {:key (str/concat id)
         :strategy-id id}])]))
