;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui.symbols
  (:require
   [beicon.core :as rx]
   [cuerdas.core :as str]
   [potok.core :as ptk]
   [rumext.v2 :as mf]
   [stks.events]
   [stks.repo :as rp]
   [stks.store :as st]
   [stks.ui.messages :as ms]
   [stks.util.data :as d]
   [stks.util.fontawesome :as fa]
   [stks.util.webapi :as wa]))

(mf/defc symbol-list
  [{:keys [exchange] :as props}]
  (let [symbols     (mf/use-state nil)
        selected    (mf/deref st/symbols-ref)

        on-change
        (mf/use-callback
         (mf/deps @symbols)
         (fn [event]
           (let [target  (wa/get-target event)
                 id      (wa/get-value target)
                 symbol  (get @symbols id)]
             (st/emit! (ptk/event :toggle-symbol symbol)))))]

    (mf/use-effect
     (mf/deps exchange)
     (fn []
       (->> (rp/req! :symbols {:exchange exchange})
            (rx/subs (fn [data]
                       (reset! symbols (d/index-by :id data)))))))

    [:section.symbols-list
     #_(when (seq @symbols)
         [:span (pr-str exchange)])
     (for [{:keys [id] :as item} (->> (vals @symbols)
                                      (sort-by :name))]
       [:div.symbol-item {:key id :title (:desc item)}
        [:input {:type "checkbox"
                 :id id
                 :checked (contains? selected id)
                 :value id
                 :on-change on-change}]
        [:label {:for id} (:name item)]])]))

(mf/defc symbols-section
  [props]
  (let [exchanges (mf/use-state nil)
        exchange  (mf/use-state nil)

        on-change
        (mf/use-callback
         (fn [event]
           (let [value (-> (wa/get-target event)
                           (wa/get-value))]
             (reset! exchange value))))]

    (mf/with-effect
      (->> (rp/req! :exchanges)
           (rx/subs (fn [data]
                      (reset! exchanges data)
                      (reset! exchange (first data))))))

    [:section.symbols-section
     [:section.exchange-selection
      [:select {:value (or @exchange "")
                :on-change on-change}

       (for [item @exchanges]
         [:option {:value item :key item} item])]]

     (when @exchange
       [:& symbol-list {:exchange @exchange}])]))
