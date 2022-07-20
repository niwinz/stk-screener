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

(def exchanges
  ["oanda",
   "fxcm",
   "forex.com",
   "icmtrader",
   "fxpro",
   "pepperstoneuk",
   "ic markets",
   "fxpig",
   "pepperstone"])


(mf/defc symbols-section
  [props]
  (let [exchange  (mf/use-state (first exchanges))
        symbols   (mf/use-state nil)
        selected  (mf/deref st/symbols-ref)

        on-change-exchange
        (mf/use-fn
         (fn [event]
           (let [value (-> (wa/get-target event)
                           (wa/get-value))]
             (reset! exchange value))))

        on-symbol-change
        (mf/use-fn
         (fn [event]
           (let [node (wa/get-target event)
                 opts (wa/query-all node "option:checked")
                 vals (into #{} (map wa/get-value opts))]
             (st/emit! (ptk/event :select-symbols {:symbols vals})))))

        deselect-symbols
        (mf/use-fn
         (fn [event]
           (wa/prevent-default! event)
           (wa/stop-propagation! event)
           (st/emit! (ptk/event :select-symbols {:symbols #{}}))))]

    (mf/with-effect [@exchange]
      (->> (rp/req! :symbols {:exchange @exchange})
           (rx/subs (fn [data]
                      (reset! symbols (d/index-by :id data))))))

    [:section.symbols-section
     [:select.exchange-list {:value (or @exchange "")
                             :on-change on-change-exchange}
      (for [item exchanges]
        [:option {:value item :key item } item])]

     [:select.symbol-list
      {:multiple true
       :on-double-click deselect-symbols
       :value (or (some-> selected clj->js) #js [])
       :on-change on-symbol-change}

      (for [{:keys [id] :as item} (->> (vals @symbols)
                                       (sort-by :name))]
        [:option {:value id :title (:desc item) :key id} (:name item)])]]))

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

        deselect-fn
        (mf/use-fn
         (fn [event]
           (wa/prevent-default! event)
           (wa/stop-propagation! event)
           (st/emit! (ptk/event :select-strategies {:strategies #{}}))))]

    [:section.strategies-section
     [:select.strategy-title {:default-value "" :disabled true}
      [:option {:value ""} "strategy"]]

     [:select.strategy-list
      {:multiple true
       :on-double-click deselect-fn
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
