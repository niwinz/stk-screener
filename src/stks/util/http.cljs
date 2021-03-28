;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.http
  (:require
   [beicon.core :as rx]
   [lambdaisland.uri :as u]
   [promesa.core :as p]
   [stks.util.data :as d]
   [stks.util.exceptions :as ex]
   [stks.util.timers :as tm]
   [stks.util.transit :as t]))

(defn translate-method
  [method]
  (case method
    :head    "HEAD"
    :options "OPTIONS"
    :get     "GET"
    :post    "POST"
    :put     "PUT"
    :patch   "PATCH"
    :delete  "DELETE"
    :trace   "TRACE"))

(defn req!
  [{:keys [method uri query headers body response-type timeout mode]
    :or {timeout 10000 response-type :json mode :cors headers {}}}]
  (let [ctrl    (js/AbortController.)
        query   (cond
                  (string? query) query
                  (map? query)    (u/map->query-string query)
                  :else nil)
        uri    (-> uri
                   (assoc :query query))
        sem    (tm/schedule timeout #(.abort ^js ctrl))
        params #js {:method (translate-method method)
                    :headers (clj->js headers)
                    :body body
                    :mode (name mode)
                    :redirect "follow"
                    :credentials "same-origin"
                    :referrerPolicy "no-referrer"
                    :signal (.-signal ^js ctrl)
                    }]

    (-> (js/fetch (str uri) params)
        (p/then (fn [response]
                  ;; cancel internal timer
                  (sem)

                  (let [res {::response response
                             :status (.-status ^js response)}]
                    (case response-type
                      :json (-> (.json ^js response)
                                (p/then #(assoc res :body %)))
                      :text (-> (.text ^js response)
                                (p/then #(assoc res :body %)))
                      :blob (-> (.blob ^js response)
                                (p/then #(assoc res :body %))))))))))

(defn success?
  [{:keys [status]}]
  (<= 200 status 299))

(defn server-error?
  [{:keys [status]}]
  (<= 500 status 599))

(defn client-error?
  [{:keys [status]}]
  (<= 400 status 499))
