;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.events
  (:require
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [lambdaisland.uri :as u]
   [okulary.core :as l]
   [potok.core :as ptk]
   [clojure.set :as set]
   [stks.events.strategies :as stg]
   [stks.events.types.nav :as-alias types.nav]
   [stks.util.logging :as log]
   [stks.util.spec :as us]
   [stks.util.storage :refer [storage]]
   [stks.util.webapi :as wa]))

(log/set-level! :trace)

(def re-throw #(rx/throw %))

(defmethod ptk/resolve :authenticate
  [_ {:keys [token]}]
  (us/assert string? token)
  (ptk/reify :authenticate
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
            params (cond-> params
                     (nil? token)
                     (assoc :section :auth)

                     (nil? (:section params))
                     (assoc :section :dashboard))]
        (rx/of (ptk/event :nav params)
               (ptk/event :initialize))))))

(defmethod ptk/resolve :initialize
  [_ params]
  (ptk/reify :initialize
    ptk/WatchEvent
    (watch [_ state stream]
      (rx/of (ptk/event ::stg/initialize-scheduler)))))

(s/def ::types.nav/section ::us/keyword)
(s/def ::types.nav/token ::us/string)
(s/def ::types.nav/strategies ::us/set-of-kw)
(s/def ::types.nav/symbols ::us/set-of-str)
(s/def ::types.nav/params
  (s/keys :opt-un [::types.nav/section
                   ::types.nav/token
                   ::types.nav/strategies
                   ::types.nav/symbols]))

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
          (update :nav #(us/conform ::types.nav/params %))))

    ptk/EffectEvent
    (effect [_ state stream]
      (let [uri   (wa/get-current-uri)
            nav   (s/unform ::types.nav/params (:nav state))
            query (u/map->query-string nav)
            uri   (assoc uri :query query)]
        (.pushState js/history #js {} "" (str uri))))))

(defmethod ptk/resolve :select-strategies
  [_ {:keys [strategies]}]
  (us/verify! ::us/set-of-str strategies)
  (ptk/reify ::select-strategies
    ptk/UpdateEvent
    (update [_ state]
      (update state :signals (fn [data]
                               (let [current-keys (into #{} (keys data))
                                     remove-keys  (set/difference current-keys strategies)]
                                 (reduce dissoc data remove-keys)))))
    ptk/WatchEvent
    (watch [_ state stream]
      (rx/of (ptk/event :nav {:strategies strategies})
             (ptk/event ::stg/initialize-scheduler)))))

(defmethod ptk/resolve :toggle-strategy
  [_ {:keys [id] :as strategy}]
  (ptk/reify :activate-strategy
    ptk/WatchEvent
    (watch [_ state stream]
      (let [{:keys [strategies symbols] :or {strategies #{}}} (:nav state)]
        (if (contains? strategies id)
          (let [strategies (disj strategies id)]
            (rx/of (ptk/event :nav {:strategies (if (empty? strategies) nil strategies)})
                   (ptk/event ::stg/initialize-scheduler)
                   #(update % :signals dissoc id)))


          (let [strategies (conj strategies id)]
            (rx/of (ptk/event :nav {:strategies strategies})
                   (ptk/event ::stg/initialize-scheduler))))))))

(defmethod ptk/resolve :select-symbols
  [_ {:keys [symbols]}]
  (us/verify! ::us/set-of-str symbols)
  (ptk/reify ::select-symbols
    ;; ptk/UpdateEvent
    ;; (update [_ state]
    ;;   (update state :nav assoc :symbols symbols))

    ptk/WatchEvent
    (watch [_ state stream]
      (rx/of (ptk/event :nav {:symbols symbols})
             (rx/of (ptk/event ::stg/initialize-scheduler))))))

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

         (rx/of (ptk/event ::stg/initialize-scheduler)))))))
