;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.events.symbols
  (:require
   [lambdaisland.glogi :as log]
   [beicon.core :as rx]
   [cljs.core.async :as a]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [potok.core :as ptk]
   [stks.repo :as rp]
   [stks.events.messages :as em]
   [stks.util.closeable :as cs]
   [stks.util.cron :as cron]
   [stks.util.data :as d]
   [stks.util.exceptions :as ex]
   [stks.util.spec :as us]
   [stks.util.storage :refer [storage]]
   [stks.util.time :as dt]
   [stks.util.transit :as t]))

(log/set-level 'stks.events.symbols :trace)

(def cron-table
  {:m5  (cron/parse "*/5 * * * 1-5")
   :m30 (cron/parse "*/30 * * * 1-5")
   :h4  (cron/parse "0 */4 * * 1-5")
   :d1  (cron/parse "0 0 * * 1-5")})

;; A state of the currently rinning schedules.
(defonce schedules (atom {}))


(defn schedule-symbol
  [id timeframe]
  (log/info :fn :schedule-observable :symbol id :tf timeframe)
  (letfn [(timeout [timeframe]
            (let [cexp (get cron-table timeframe)
                  wait (cron/ms-until-next cexp)]
              (log/trace :fn :schedule-observable :action :timeout :wait wait)
              (a/timeout wait)))

          (on-subscribe [sink]
            (let [close-ch (a/chan 1)]
              (a/go-loop []
                (let [[_ port] (a/alts! [close-ch (timeout timeframe)])]
                  (when-not (= port close-ch)
                    (sink (ptk/event :refresh-symbol {:id id :timeframe timeframe}))
                    (recur))))
              (fn []
                (log/trace :fn :schedule-observable :action :close :symbol id :tf timeframe)
                (a/close! close-ch))))]

    (rx/create on-subscribe)))

(defmethod ptk/resolve :schedule-symbol
  [_ {:keys [id] :as symbol}]
  (ptk/reify :schedule-symbol
    ptk/WatchEvent
    (watch [_ state stream]
      (log/trace :event :schedule-symbol :method :watch :symbol id)
      (let [close (->> stream
                       (rx/filter (ptk/type? :del-symbol))
                       (rx/filter #(= id (deref %))))]
        (rx/merge
         (->> (schedule-symbol id :m5)
              (rx/take-until close))
         (->> (schedule-symbol id :m30)
              (rx/take-until close)))))))

(defmethod ptk/resolve :refresh-symbol
  [_ {:keys [id timeframe] :as params}]
  (ptk/reify :refresh-symbol
    ptk/WatchEvent
    (watch [_ state stream]
      (log/trace :event :refresh-symbol :method :watch :symbol id :tf timeframe)
      nil)))

(defmethod ptk/resolve :add-symbol
  [_ {:keys [id] :as symbol}]
  (ptk/reify :select-symbol
    ptk/UpdateEvent
    (update [_ state]
      (update state :symbols assoc id symbol))

    ptk/WatchEvent
    (watch [_ state stream]
      (rx/of (ptk/event :schedule-symbol symbol)))))

(defmethod ptk/resolve :del-symbol
  [_ {:keys [id] :as symbol}]
  (ptk/reify :del-symbol
    cljs.core/IDeref
    (-deref [_] id)

    ptk/UpdateEvent
    (update [_ state]
      (update state :symbols dissoc id))))
