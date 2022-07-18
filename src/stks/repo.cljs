;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.repo
  (:require
   [cuerdas.core :as str]
   [beicon.core :as rx]
   [stks.util.logging :as log]
   [lambdaisland.uri :as u]
   [cljs.spec.alpha :as s]
   [stks.util.spec :as us]
   [stks.util.data :as d]
   [stks.util.http :as http]
   [stks.util.object :as obj]
   [stks.util.storage :refer [storage cache]]
   [stks.util.time :as dt]))

(log/set-level! :trace)

;; --- HELPERS

(declare handle-response)

;; Base Finnhub URI
(def base-uri (u/uri "https://finnhub.io/api/v1/"))

(defn- with-cache
  [{:keys [key max-age disable]} observable]
  (let [entry (get cache key)
        age   (when entry
                (dt/diff (dt/now)
                         (:created-at entry)))]
    (if (and (some? entry)
             (< age max-age)
             (not disable))
      (do
        (log/trace :cache :hit :age age :key key)
        (rx/of (:data entry)))
      (do
        (log/trace :cache :miss :key key)
        (->> observable
             (rx/tap (fn [data]
                       (let [entry {:created-at (dt/now) :data data}]
                         (swap! cache assoc key entry)))))))))

(defn- handle-response
  [response]
  (cond
    (http/success? response)
    (rx/of (:body response))

    (= (:status response) 400)
    (rx/throw (:body response))

    (= (:status response) 401)
    (rx/throw {:type :authentication
               :code :not-authenticated})

    (= (:status response) 404)
    (rx/throw {:type :not-found :code :object-not-found})

    :else
    (rx/throw {:type :internal-error
               :status (:status response)
               :body (:body response)})))

(defn- default-params
  []
  {:token (:stks/token storage)})

(defn- request-finnhub
  ([suffix] (request-finnhub suffix {}))
  ([suffix params]
   (let [uri    (u/join base-uri suffix)
         params (merge params (default-params))]
     (->> (http/send! {:method :get :uri uri :query params})
          (rx/mapcat handle-response)))))

(defmulti request (fn [id params opts] id))

;; --- PUBLIC API

(defn req!
  ([id] (req! id {} {}))
  ([id params] (req! id params {}))
  ([id params options] (request id params options)))

;; --- IMPL

(def default-cache-duration
  (dt/duration {:minutes 5}))

(defmethod request :exchanges
  [id params opts]
  (with-cache {:key [id params] :max-age default-cache-duration}
    (->> (request-finnhub "forex/exchange")
         (rx/map vec))))

(s/def ::exchange ::us/string)
(s/def ::symbols-params
  (s/keys :req-un [::exchange]))

(defmethod request :symbols
  [id params opts]
  (us/assert ::symbols-params params)
  (with-cache {:key [id params] :max-age default-cache-duration}
    (->> (request-finnhub "forex/symbol" params)
         (rx/map (fn [symbols]
                   (mapv (fn [item]
                           (let [cid          (obj/get item "symbol")
                                 [exchange _] (str/split cid #":")]
                             {:id cid
                              :name (obj/get item "displaySymbol")
                              :desc (obj/get item "description")
                              :exchange (str/lower exchange)}))
                         (seq symbols)))))))

(defmethod request :symbol-data
  [id {:keys [id timeframe] :as params} opts]
  (letfn [(extract-field [data field]
            (-> (obj/get data "macd") (seq) (reverse)))
          (on-data [data]
            ;; (js/console.log data)
            (let [macd1   (extract-field data "macd")
                  macd2   (extract-field data "macdSignal")
                  close   (extract-field data "c")
                  ts      (extract-field data "t")

                  entries (map (fn [macd1 macd2 close ts]
                                 {:ts    (dt/epoch->datetime ts {:zone "utc"})
                                  :close close
                                  :macd1 macd1
                                  :macd2 macd2})
                               (take 5 macd1)
                               (take 5 macd2)
                               (take 5 close)
                               (take 5 ts))]

              ;; (prn "REPO" :symbol-data)
              ;; (stks.util.pprint/pprint entries)

              {:symbol-id   id
               :timeframe timeframe
               :entries entries}))
          ]

    (let [now     (dt/now)
          params' {:symbol id
                   :resolution (case timeframe
                                 :m5 "5"
                                 :m30 "30"
                                 :h4 "240"
                                 :d1 "D")
                   :to (dt/format now :epoch)
                   :from (case timeframe
                           :m5  (-> now (dt/minus {:hours 4}) (dt/format :epoch))
                           :m30 (-> now (dt/minus {:days 3})  (dt/format :epoch))
                           :h4  (-> now (dt/minus {:days 13}) (dt/format :epoch))
                           :d1  (-> now (dt/minus {:days 60}) (dt/format :epoch)))
                   :indicator "macd"}]
      #_(with-cache {:key [id timeframe 5]
                   :disable true
                   :max-age (case timeframe
                              :m30 (dt/duration {:minutes 15})
                              :h4  (dt/duration {:hours 1})
                              (dt/duration {:minutes 5}))})
      (do
        (->> (request-finnhub "indicator" params')
             (rx/map on-data))))))

