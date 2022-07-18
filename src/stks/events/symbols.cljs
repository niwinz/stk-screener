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

(defn create-scheduler
  [id tfs]
  (letfn [(next-wait []
            (apply min (->> tfs
                            (map #(get stg/cron-exprs %))
                            (map #(cron/ms-until-next %)))))

          (on-subscribe [subscriber]
            (log/trace :hint "subscribe to symbol scheduler" :symbol id)
            (let [sem (atom nil)
                  efn (fn emmiter []
                        (rx/push! subscriber id)
                        (let [rsc (tm/schedule (next-wait) emmiter)]
                          (reset! sem rsc)))]
              (efn)
              (fn []
                (log/trace :hint "unsubscribe from symbol scheduler" :symbol id)
                (when-let [rsc (deref sem)]
                  (tm/dispose! rsc)
                  (reset! sem nil)))))]

    (rx/create on-subscribe)))


(defmethod ptk/resolve :stop-symbol-scheduler
  [_ {:keys [id]}]
  (ptk/reify :stop-symbol-scheduler
    IDeref
    (-deref [_] id)))

(defmethod ptk/resolve :init-symbol-scheduler
  [_ {:keys [id] :as params}]
  (ptk/reify :init-symbol-scheduler
    IDeref
    (-deref [_] params)

    ptk/WatchEvent
    (watch [_ state stream]
      (when-let [tfs (seq (stg/get-timeframes state))]
        (prn :init-symbol-scheduler tfs)

        (let [stoper (rx/merge
                      (rx/filter (ptk/type? :stop) stream)
                      (->> stream
                           (rx/filter (ptk/type? :stop-symbol-scheduler))
                           (rx/filter #(= id (deref %)))))]
          (rx/merge
           (->> (create-scheduler id tfs)
                (rx/map #(ptk/event :fetch-symbol-data {:id %}))
                (rx/take-until stoper)
                (rx/observe-on :async))

           (->> stream
                (rx/filter (ptk/type? :symbol-data))
                (rx/filter #(= id (:id (deref %))))
                (rx/map deref)
                (rx/map #(ptk/event :execute-strategies %))
                (rx/take-until stoper))))))))

(defmethod ptk/resolve :fetch-symbol-data
  [_ {:keys [id]}]
  (ptk/reify :fetch-symbol-data
    ptk/WatchEvent
    (watch [_ state stream]
      (log/trace :event :fetch-symbol-data :method :watch :symbol id)
      (let [tfs (stg/get-timeframes state)]
        (->> (rx/from (seq tfs))
             (rx/mapcat #(rp/req! :symbol-data {:id id :timeframe %}))
             (rx/reduce conj [])
             (rx/map (fn [data]
                       (let [data (d/index-by :timeframe data)
                             data (assoc data :id id)]
                         (ptk/data-event :symbol-data data)))))))))
