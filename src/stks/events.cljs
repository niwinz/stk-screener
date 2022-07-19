;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.events
  (:require
   [stks.util.logging :as log]
   [lambdaisland.uri :as u]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [potok.core :as ptk]
   [stks.repo :as rp]
   [stks.events.types.nav :as-alias t.nav]
   [stks.events.symbols :as sym]
   [stks.events.strategies]
   [stks.events.messages :as em]
   [stks.util.data :as d]
   [stks.util.webapi :as wa]
   [stks.util.exceptions :as ex]
   [stks.util.spec :as us]
   [stks.util.storage :refer [storage]]
   [stks.util.time :as dt]
   [stks.util.transit :as t]))

(log/set-level! :trace)

(def re-throw #(rx/throw %))

(defmethod ptk/resolve :authenticate
  [_ {:keys [token]}]
  (us/assert string? token)
  (ptk/reify :authenticate
    ;; ptk/UpdateEvent
    ;; (update [_ state]
    ;;   (assoc state :token token))

    ptk/WatchEvent
    (watch [_ state stream]
      (rx/of (ptk/event :nav {:section :dashboard})))

    ptk/EffectEvent
    (effect [_ _ _]
      (swap! storage assoc :stks/token token))))

(defmethod ptk/resolve :logout
  [_ _]
  (ptk/reify :logout
    ptk/UpdateEvent
    (update [_ state]
      {})

    ptk/WatchEvent
    (watch [_ state stream]
      (rx/of (ptk/event :nav {:section :auth})))

    ptk/EffectEvent
    (effect [_ _ _]
      (swap! storage dissoc :stks/token))))

(defmethod ptk/resolve :setup
  [_ params]
  (ptk/reify :setup
    ptk/WatchEvent
    (watch [_ state stream]
      (let [uri    (wa/get-current-uri)
            token  (:stks/token storage)
            params (u/query-string->map (:query uri))
            params (if (nil? token)
                     (assoc params :section :auth)
                     (assoc params :section :dashboard))]
        (rx/of (ptk/event :nav params)
               (ptk/event :initialize))))))

(defmethod ptk/resolve :initialize
  [_ params]
  (ptk/reify :initialize
    ptk/WatchEvent
    (watch [_ state stream]
      (rx/of (ptk/event ::sym/initialize-scheduler)))))

(s/def ::t.nav/section ::us/keyword)
(s/def ::t.nav/token ::us/string)
(s/def ::t.nav/strategies ::us/set-of-kw)
(s/def ::t.nav/symbols ::us/set-of-str)
(s/def ::t.nav/params
  (s/keys :opt-un [::t.nav/section
                   ::t.nav/token
                   ::t.nav/strategies
                   ::t.nav/symbols]))

(defmethod ptk/resolve :nav
  [_ params]
  (ptk/reify :nav
    IDeref
    (-deref [_] params)

    ptk/UpdateEvent
    (update [_ state]
      (log/trace :hint "navigate" :params params)
      (-> state
          (update :nav (fn [nav]
                         (reduce-kv (fn [res k v]
                                      (if (nil? v)
                                        (dissoc res k)
                                        (assoc res k v)))
                                    nav
                                    params)))
          (update :nav #(us/conform ::t.nav/params %))))

    ptk/EffectEvent
    (effect [_ state stream]
      (let [uri   (wa/get-current-uri)
            nav   (s/unform ::t.nav/params (:nav state))
            query (u/map->query-string nav)
            uri   (assoc uri :query query)]
        (.pushState js/history #js {} "" (str uri))))))

(defmethod ptk/resolve :toggle-strategy
  [_ {:keys [id] :as strategy}]
  (ptk/reify :activate-strategy
    ptk/WatchEvent
    (watch [_ state stream]
      (let [{:keys [strategies symbols] :or {strategies #{}}} (:nav state)]
        (if (contains? strategies id)
          (let [strategies (disj strategies id)]
            (rx/of (ptk/event :nav {:strategies (if (empty? strategies) nil strategies)})
                   (ptk/event ::sym/initialize-scheduler)
                   #(update % :signals dissoc id)))


          (let [strategies (conj strategies id)]
            (rx/of (ptk/event :nav {:strategies strategies})
                   (ptk/event ::sym/initialize-scheduler))))))))

(defmethod ptk/resolve :toggle-symbol
  [_ {:keys [id] :as symbol}]
  (ptk/reify :toggle-symbol
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:symbols id] symbol))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [symbols (get-in state [:nav :symbols] #{})]
        (rx/concat
         (if (contains? symbols id)
           (rx/of (ptk/event :nav {:symbols (disj symbols id)}))
           (rx/of (ptk/event :nav {:symbols (conj symbols id)})))

         (rx/of (ptk/event ::sym/initialize-scheduler)))))))
