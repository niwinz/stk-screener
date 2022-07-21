;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui.settings
  (:require
   [beicon.core :as rx]
   [cuerdas.core :as str]
   [potok.core :as ptk]
   [rumext.v2 :as mf]
   [stks.events]
   [stks.repo :as rp]
   [stks.store :as st]
   [stks.events.strategies :as stg]
   [stks.ui.messages :as ms]
   [stks.util.data :as d]
   [stks.util.fontawesome :as fa]
   [stks.util.webapi :as wa]))

(mf/defc symbols-section
  [props]
  (let [symbols   (mf/deref st/symbols-ref)
        selected  (-> st/nav-ref mf/deref :symbols)

        on-symbol-change
        (mf/use-fn
         (fn [event]
           (let [node (wa/get-target event)
                 opts (wa/query-all node "option:checked")
                 vals (into #{} (map wa/get-value opts))]
             (st/emit! (ptk/event :select-symbols {:symbols vals})))))

        on-symbol-toggle
        (mf/use-fn
         (fn [event]
           (wa/prevent-default! event)
           (wa/stop-propagation! event)
           (let [node      (wa/get-target event)
                 symbol-id (wa/get-value node)]
             (st/emit! (ptk/event :toggle-symbol {:id symbol-id})))))

        unselect-all
        (mf/use-fn
         (fn [event]
           (st/emit! (ptk/event :select-symbols {:symbols #{}}))))]

    [:section.symbols-section
     [:div.symbols-title
      {:on-double-click unselect-all}
      "Available symbols:"]

     [:select.symbol-list
      {:multiple true
       :value (or (some-> selected clj->js) #js [])
       :on-change on-symbol-change}

      (for [{:keys [id] :as item} (->> (vals symbols)
                                       (sort-by (juxt :exchange :name)))]
        [:option
         {:key id
          :value id
          :title (:desc item)
          ;; :on-click on-symbol-toggle
          }
         (:desc item)])]]))

(mf/defc strategies-section
  []
  (let [selected (mf/deref st/strategies-ref)
        on-change
        (mf/use-fn
         (fn [event]
           (let [node (wa/get-target event)
                 opts (wa/query-all node "option:checked")
                 vals (into #{} (comp (map wa/get-value)
                                      (map keyword)) (seq opts))]
             (st/emit! (ptk/event :select-strategies {:strategies vals})))))

        deselect-all
        (mf/use-fn
         (fn [event]
           (wa/prevent-default! event)
           (wa/stop-propagation! event)
           (st/emit! (ptk/event :select-strategies {:strategies #{}}))))]

    [:section.strategies-section
     [:div.strategy-title {:on-double-click deselect-all} "Strategies:"]
     [:select.strategy-list
      {:multiple true
       :value (or (some-> selected clj->js) #js [])
       :on-change on-change}
      (for [{:keys [id] :as item} stg/available-strategies]
        (let [val (d/name id)]
          [:option {:value val :key val} val]))]]))

(mf/defc settings-section
  [props]
  [:section.settings-section
   [:& symbols-section {}]
   [:& strategies-section {}]])
