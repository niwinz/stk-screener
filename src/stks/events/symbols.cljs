;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.events.symbols
  (:require
   [stks.util.logging :as log]
   [beicon.core :as rx]
   [cljs.core.async :as a]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [potok.core :as ptk]
   [stks.repo :as rp]
   [stks.events.strategies :as stg]
   [stks.events.messages :as em]
   [stks.util.closeable :as cs]
   [stks.util.cron :as cron]
   [stks.util.data :as d]
   [stks.util.exceptions :as ex]
   [stks.util.spec :as us]
   [stks.util.storage :refer [storage]]
   [stks.util.time :as dt]
   [stks.util.timers :as tm]
   [stks.util.transit :as t]))

(log/set-level! :trace)

(defn- get-timeframes
  "Get required timeframes for the currently selected strategies."
  [{:keys [nav] :as state}]
  (reduce (fn [res id]
            (let [strategy (d/seek #(= id (:id %)) stg/available-strategies)]
              (into res ((juxt :main :reference) strategy))))
          #{}
          (:strategies nav)))

(defn- create-scheduler
  [id tfs]
  (letfn [(next-wait []
            (apply min (->> tfs
                            (map #(get stg/cron-exprs %))
                            (map #(cron/ms-until-next %)))))

          (on-subscribe [subscriber]
            (log/trace :hint "subscribe to symbol scheduler" :symbol-id id :timeframes tfs)
            (let [sem (atom nil)
                  efn (fn emmiter []
                        (rx/push! subscriber id)
                        (let [rsc (tm/schedule (next-wait) emmiter)]
                          (reset! sem rsc)))]
              (efn)
              (fn []
                (log/trace :hint "unsubscribe from symbol scheduler" :symbol-id id :timeframes tfs)
                (when-let [rsc (deref sem)]
                  (tm/dispose! rsc)
                  (reset! sem nil)))))]

    (rx/create on-subscribe)))

(defmethod ptk/resolve ::initialize-scheduler
  [_ _]
  (ptk/reify ::initialize-scheduler
    ptk/WatchEvent
    (watch [_ state stream]
      (let [tframes (get-timeframes state)
            symbols (-> state :nav :symbols)
            stoper  (rx/merge
                     (rx/filter (ptk/type? :stop) stream)
                     (rx/filter (ptk/type? ::terminate-scheduler) stream)
                     (rx/filter (ptk/type? ::initialize-scheduler) stream))]

        (when (and (seq tframes) (seq symbols))
          (->> (rx/from (seq symbols))
               (rx/merge-map (fn [symbol-id]
                               (->> (create-scheduler symbol-id tframes)
                                    (rx/mapcat (constantly tframes))
                                    (rx/mapcat #(rp/req! :symbol-data {:id symbol-id :timeframe %}))
                                    (rx/reduce conj [])
                                    (rx/map (fn [data]
                                              (let [data (d/index-by :timeframe data)]
                                                (assoc data :id symbol-id))))
                                    (rx/map #(ptk/event ::stg/execute %)))))
               (rx/take-until stoper)))))))
