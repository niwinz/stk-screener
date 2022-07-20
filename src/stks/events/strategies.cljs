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

(log/set-level! :trace)

(def available-strategies
  [{:id :macd-m5
    :main :m5
    :reference :h4}
   {:id :macd-h1
    :main :h1
    :reference :d1}])

(def cron-exprs
  {:m30 "0 */30 * * * *"
   :h1  "0 0 * * * *"
   :m5  "0 */5 * * * *"
   :h4  "0 0 */4 * * *"
   :d1  "0 0 0 * * *"})

;; --- SYMBOLS & SCHEDULER

(defn- get-timeframes
  "Get required timeframes for the currently selected strategies."
  [{:keys [nav] :as state}]
  (reduce (fn [res id]
            (let [strategy (d/seek #(= id (:id %)) available-strategies)]
              (into res ((juxt :main :reference) strategy))))
          #{}
          (:strategies nav)))

(defn- create-scheduler
  [id tfs]
  (letfn [(next-wait []
            (apply min (->> tfs
                            (map #(get cron-exprs %))
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
                                    (rx/mapcat (fn [symbol-id]
                                                 (->> (rx/from tframes)
                                                      (rx/map #(array-map :id symbol-id :timeframe %))
                                                      (rx/mapcat #(rp/req! :symbol-data %))
                                                      (rx/reduce conj [])
                                                      ;; (rx/tap #(prn "FF1" %))
                                                      (rx/map (fn [data]
                                                                (let [data (d/index-by :timeframe data)]
                                                                  (assoc data :id symbol-id)))))))
                                    (rx/map #(ptk/event ::execute-strategies %)))))
               (rx/take-until stoper)))))))


;; --- STRATEGY SETUP EVENTS

(defmethod ptk/resolve ::execute-strategies
  [_ sdata]
  (ptk/reify ::execute-strategies
    ptk/WatchEvent
    (watch [_ state stream]
      (let [selected (get-in state [:nav :strategies])]
        ;; (log/trace :hint "execute strategies"
        ;;            :symbol-id (:id sdata)
        ;;            :strategies selected)

        (->> (rx/from (seq available-strategies))
             (rx/filter #(contains? selected (:id %)))
             (rx/map (fn [strategy]
                       {:symbol-id (:id sdata)
                        :strategy-id (:id strategy)
                        :main        (get sdata (:main strategy))
                        :ref         (get sdata (:reference strategy))}))
             (rx/map #(ptk/event ::execute-strategy %))
             (rx/observe-on :async))))))

(defmulti execute-strategy :strategy-id)
(defmethod execute-strategy :default [_] nil)

(defmethod ptk/resolve ::execute-strategy
  [_ {:keys [symbol-id strategy-id] :as params}]
  (ptk/reify ::execute-strategy
    ptk/UpdateEvent
    (update [_ state]
      ;; (prn "KKKK" params)
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

(derive :macd-m5 ::macd)
(derive :macd-h1 ::macd)

(defmethod execute-strategy ::macd
  [{:keys [main ref strategy-id] :as sdata}]
  (letfn [(growing? [data]
            (loop [prev  ##Inf
                   items (seq data)]
              (if-let [current (some-> items first :macd2)]
                (if (> prev current)
                  (recur current (rest items))
                  false)
                true)))

          (decreasing? [data]
            (loop [prev  ##-Inf
                   items (seq data)]
              (if-let [current (some-> items first :macd2)]
                (if (< prev current)
                  (recur current (rest items))
                  false)
                true)))]

    (let [main (->> main :entries first)
          ref  (->> ref :entries  (drop 1) (take 3))]

      ;; (println "========== init execute-strategy" strategy-id)
      ;; (cljs.pprint/pprint main)
      ;; (cljs.pprint/pprint ref)
      ;; (println "========== end  execute-strategy" strategy-id)

      (cond
        (and (growing? ref)
             (neg? (:macd1 main))
             (neg? (:macd2 main)))
        {:dir :up}

        (and (decreasing? ref)
             (pos? (:macd1 main))
             (pos? (:macd2 main)))

        {:dir :down}))))
