;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.events.strategies
  (:require
   [stks.util.logging :as log]
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
   [stks.util.uuid :as uuid]
   [stks.util.transit :as t]))

(log/set-level! :trace)

(def available-strategies
  [{:id :macd-m30
    :main :m30
    :reference :h4}
   {:id :macd-h4
    :main :h4
    :reference :d1}])

(def cron-exprs
  {:m30 "0 */30 * * * *"
   :h4  "0 0 */4 * * *"
   :d1  "0 0 0 * * *"})

;; --- STRATEGY SETUP EVENTS

(defmethod ptk/resolve ::execute
  [_ sdata]
  (ptk/reify :execute
    ptk/WatchEvent
    (watch [_ state stream]
      (let [selected (get-in state [:nav :strategies])]
        (log/trace :hint "execute strategies"
                   :symbol-id (:id sdata)
                   :strategies selected)

        (->> (rx/from (seq available-strategies))
             (rx/filter #(contains? selected (:id %)))
             (rx/map (fn [strategy]
                       {:symbol-id (:id sdata)
                        :strategy-id (:id strategy)
                        :main        (get sdata (:main strategy))
                        :ref         (get sdata (:reference strategy))}))
             (rx/map #(ptk/event :execute-strategy %))
             (rx/observe-on :async))))))

(defmulti execute-strategy :strategy-id)
(defmethod execute-strategy :default [_] nil)

(defmethod ptk/resolve ::execute-strategy
  [_ {:keys [symbol-id strategy-id] :as params}]
  (ptk/reify ::execute-strategy
    ptk/UpdateEvent
    (update [_ state]
      (let [result (execute-strategy params)
            now    (dt/now)]
        (log/trace :hint "execute strategy"
                   :strategy strategy-id
                   :symbol-id symbol-id
                   :result result)
        (if result
          (update-in state [:signals strategy-id symbol-id]
                     (fn [signal]
                       (if (some? signal)
                         (-> (merge signal result)
                             (assoc :updated-at (dt/now)))
                           (-> result
                               (assoc :created-at now)
                               (assoc :updated-at now)))))
          (update-in state [:signals strategy-id] dissoc symbol-id))))))

;; --- STRATEGY IMPL

(derive :macd-m30 ::macd)
(derive :macd-h4 ::macd)

(defmethod execute-strategy ::macd
  [{:keys [main ref] :as sdata}]
  ;; (println "========== init execute-strategy" :macd-m30)
  ;; (cljs.pprint/pprint main)
  ;; (cljs.pprint/pprint ref)
  ;; (println "========== end  execute-strategy" :macd-m30)
  (let [main (-> main :entries first)
        res  (map #(if (> (:macd1 %) (:macd2 %)) :up :down) (:entries ref))]
    (cond
      (and (every? (partial = :up) res)
           (neg? (:macd1 main)))
      {:dir :up}

      (and (every? (partial = :down) res)
           (pos? (:macd1 main)))
      {:dir :down})))
