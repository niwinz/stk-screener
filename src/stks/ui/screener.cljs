;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui.screener
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

(defn- increasing?
  [data attr]
  (loop [prev  ##Inf
         items (seq data)]
    (if-let [current (some-> items first attr)]
      (if (> prev current)
        (recur current (rest items))
        false)
      true)))

(defn- decreasing?
  [data attr]
  (loop [prev  ##-Inf
         items (seq data)]
    (if-let [current (some-> items first attr)]
      (if (< prev current)
        (recur current (rest items))
        false)
      true)))

(mf/defc symbol-timing
  [{:keys [timeframe data] :as props}]
  (let [entries (get data timeframe)
        macd    (->> entries first :macd2)
        smpl    (->> entries (drop 1) (take 3))
        posv?   (pos? macd)
        incr?   (increasing? smpl :macd2)]
    [:div.timing {:class (str/concat (name timeframe) "-timing")}
     [:span.timeframe
      {:class (wa/classnames
               :positive incr?
               :negative (not incr?))}

      (name timeframe) ":"]
     [:span.indicator
      (if posv? "+" "-")
      (if incr? "⇈" "⇊")]]))

(mf/defc symbol-row
  {::mf/wrap [mf/memo]}
  [{:keys [symbol signals data] :as props}]

  (let [data-m5 (:m5 data)
        macd-m5 (-> data-m5 first :macd2)
        smpl-m5 (->> data-m5 (take 3))

        data-h1 (:h1 data)
        macd-h1 (-> data-h1 first :macd2)
        smpl-h1 (->> data-h1 (take 3))

        data-h4 (:h4 data)
        macd-h4 (-> data-h4 first :macd2)
        smpl-h4 (->> data-h4 (take 3))

        data-d1 (:d1 data)
        macd-d1 (-> data-d1 first :macd2)
        smpl-d1 (->> data-d1 (take 3))
        ]
    [:div.screener-item
     [:div.symbol-name {:title (:id symbol)} (:name symbol)]
     [:div.timings
      [:& symbol-timing {:timeframe :m5 :data data}]
      [:& symbol-timing {:timeframe :h1 :data data}]
      [:& symbol-timing {:timeframe :h4 :data data}]
      [:& symbol-timing {:timeframe :d1 :data data}]]

     [:div.signals {}
      (for [[id signal] signals]
        [:div.signal {:key (name id) :class (when (:inactive signal) "inactive")}
         [:span.signal-name (name id) ":"]
         [:span.signal-direction
          {:class (wa/classnames
                   :positive (= :up (:dir signal))
                   :negative (= :down (:dir signal)))}
          "⇈"]])]

     [:div.spacer]]))

(mf/defc screener-section
  [props]
  (let [symbols (mf/deref st/selected-symbols-ref)
        signals (mf/deref st/signals-ref)
        ohlc    (mf/deref st/ohlc-ref)]
    [:section.screener
     [:div.table-header]
     [:div.table-body
      (for [{:keys [id] :as symbol} (sort-by :name symbols)]
        [:& symbol-row
         {:key id
          :symbol  symbol
          :data    (get ohlc id)
          :signals (get signals id)}])]]))

