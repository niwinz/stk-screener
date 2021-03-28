;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.repo
  (:require
   [cuerdas.core :as str]
   [beicon.core :as rx]
   [lambdaisland.glogi :as log]
   [lambdaisland.uri :as u]
   [stks.util.data :as d]
   [stks.util.http :as http]
   [stks.util.object :as obj]
   [stks.util.storage :refer [storage]]
   [stks.util.time :as dt]))

(log/set-level 'stks.repo :trace)

;; --- HELPERS

(declare handle-response)

;; Base Finnhub URI
(def base-uri (u/uri "https://finnhub.io/api/v1/"))
(defonce cache (atom (::cache storage)))

(add-watch cache ::persistence #(swap! storage assoc ::cache %4))

(defn- with-cache
  [params observable]
  (let [entry (get @cache params)
        age   (when entry
                (- (inst-ms (dt/now))
                   (inst-ms (:created-at entry))))]
    (if entry
      (do
        (log/trace :cache :hit :age age :key params)
        (rx/of (:data entry)))
      (do
        (log/trace :cache :miss :key params)
        (->> observable
             (rx/tap (fn [data]
                       (let [entry {:created-at (dt/now) :data data}]
                         (swap! cache assoc params entry)))))))))

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
  {:token (:token storage)})

(defn- request-finnhub
  ([suffix] (request-finnhub suffix {}))
  ([suffix params]
   (let [uri    (u/join base-uri suffix)
         params (d/merge params (default-params))]
     (->> (http/req! {:method :get :uri uri :query params})
          (rx/from)
          (rx/mapcat handle-response)))))

(defmulti request (fn [id params opts] id))

;; --- PUBLIC API

(defn req!
  ([id] (req! id {} {}))
  ([id params] (req! id params {}))
  ([id params options] (request id params options)))

;; --- IMPL

(defmethod request :exchanges
  [id params opts]
  (with-cache [id params]
    (->> (request-finnhub "forex/exchange")
         (rx/map vec))))

(defmethod request :symbols
  [id params opts]
  (with-cache [id params]
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

