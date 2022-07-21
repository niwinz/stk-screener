;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.events.strategies
  (:require
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [potok.core :as ptk]
   [stks.repo :as rp]
   [stks.util.cron :as cron]
   [stks.util.data :as d]
   [stks.util.logging :as log]
   [stks.util.spec :as us]
   [stks.util.time :as dt]
   [stks.util.timers :as tm]))

(log/set-level! :info)

(def available-strategies
  [{:id :macd-m5h4
    :main :m5
    :reference :h4}
   {:id :macd-m5h1
    :main :m5
    :reference :h1}
   {:id :macd-h1d1
    :main :h1
    :reference :d1}])

(def cron-exprs
  {:m5  "0 */5 * * * *"
   :h1  "0 0 * * * *"
   :h4  "0 0 */4 * * *"
   :d1  "0 0 0 * * *"})

;; --- SYMBOLS & SCHEDULER

(defn- create-timeframe-scheduler
  [timeframe]
  (letfn [(next-wait []
            (->> timeframe
                 (get cron-exprs)
                 (cron/ms-until-next)))

          (on-subscribe [subscriber]
            (log/trace :hint "subscribe to timeframe scheduler" :timeframe timeframe)
            (let [sem (atom nil)
                  efn (fn emmiter []
                        (rx/push! subscriber timeframe)>
                        (let [rsc (tm/schedule (next-wait) emmiter)]
                          (reset! sem rsc)))]
              (efn)
              (fn []
                (log/trace :hint "unsubscribe from symbol scheduler" :timeframes timeframe)
                (when-let [rsc (deref sem)]
                  (tm/dispose! rsc)
                  (reset! sem nil)))))]

    (rx/create on-subscribe)))

(defmethod ptk/resolve ::initialize-scheduler
  [_ _]
  (ptk/reify ::initialize-scheduler
    ptk/WatchEvent
    (watch [_ state stream]
      (let [symbols (-> state :nav :symbols)
            stoper  (rx/merge
                     (rx/filter (ptk/type? :stop) stream)
                     (rx/filter (ptk/type? ::initialize-scheduler) stream))]

        (->> (rx/merge
              (->> (rx/from (keys cron-exprs))
                   (rx/merge-map (fn [timeframe]
                                   (->> (create-timeframe-scheduler timeframe)
                                        (rx/map #(ptk/event ::fetch-ohlc-data %))
                                        (rx/observe-on :async)))))
              (->> stream
                   (rx/filter (ptk/type? ::ohlc-data-fetched))
                   (rx/debounce 1000)
                   (rx/mapcat (constantly symbols))
                   (rx/map #(ptk/event ::execute-strategies %))))

             (rx/take-until stoper))))))

(defmethod ptk/resolve ::fetch-ohlc-data
  [_ timeframe]
  (ptk/reify ::fetch-ohlc-data
    ptk/WatchEvent
    (watch [_ state stream]
      (log/trace :hint "fetch OHLC data" :timeframe timeframe)
      (let [selected-symbols (-> state :nav :symbols)]
        (->> (rx/from selected-symbols)
             (rx/mapcat #(rp/req! :ohlc-data {:symbol-id % :timeframe timeframe}))
             (rx/map #(ptk/event ::ohlc-data-fetched %)))))))

(defmethod ptk/resolve ::ohlc-data-fetched
  [_ data]
  (ptk/reify ::ohlc-data-fetched
    ptk/UpdateEvent
    (update [_ state]
      (let [timeframe (:timeframe data)
            symbol-id (:symbol-id data)]
        (assoc-in state [:ohlc symbol-id timeframe] (:entries data))))))

(defmethod ptk/resolve ::execute-strategies
  [_ symbol-id]
  (ptk/reify ::fetch-ohlc-data
    ptk/WatchEvent
    (watch [_ state _]
      (let [selected-strategies (-> state :nav :strategies)]
        (->> (rx/from available-strategies)
             (rx/filter #(contains? selected-strategies (:id %)))
             (rx/map (fn [strategy]
                       {:symbol-id symbol-id
                        :strategy-id (:id strategy)
                        :main        (get-in state [:ohlc symbol-id (:main strategy)])
                        :ref         (get-in state [:ohlc symbol-id (:reference strategy)])}))
             (rx/map #(ptk/event ::execute-strategy %))
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
          (update-in state [:signals symbol-id strategy-id]
                     (fn [signal]
                       (if (some? signal)
                         (-> (merge signal result)
                             (assoc :updated-at (dt/now)))
                         (-> result
                             (assoc :created-at now)
                             (assoc :updated-at now)))))
          (update-in state [:signals symbol-id] dissoc strategy-id))))))

;; --- STRATEGY IMPL

(defn increasing?
  [data attr]
  (loop [prev  ##Inf
         items (seq data)]
    (if-let [current (some-> items first attr)]
      (if (> prev current)
        (recur current (rest items))
        false)
      true)))

(defn decreasing?
  [data attr]
  (loop [prev  ##-Inf
         items (seq data)]
    (if-let [current (some-> items first attr)]
      (if (< prev current)
        (recur current (rest items))
        false)
      true)))

(derive :macd-m5h1 ::macd)
(derive :macd-m5h4 ::macd)
(derive :macd-h1d1 ::macd)

(defmethod execute-strategy ::macd
  [{:keys [main ref strategy-id symbol-id] :as sdata}]
  ;; (prn "execute-strategy" symbol-id strategy-id)
  (let [main (->> main first)
        ref  (->> ref (take 3))]

    (when (= "FXPRO:14" symbol-id)
      (println "========== init execute-strategy" strategy-id)
      (cljs.pprint/pprint main)
      (cljs.pprint/pprint ref)
      (println "========== end  execute-strategy" strategy-id))

    (cond
      (and (seq ref)
           (increasing? ref :macd2)
           (neg? (:macd1 main))
           (neg? (:macd2 main)))
      {:dir :up}

      (and (seq ref)
           (decreasing? ref :macd2)
           (pos? (:macd1 main))
           (pos? (:macd2 main)))

      {:dir :down})))
