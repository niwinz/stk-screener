;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.events
  (:require
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [potok.core :as ptk]
   [stks.repo :as rp]
   [stks.events.symbols]
   [stks.events.messages :as em]
   [stks.util.data :as d]
   [stks.util.exceptions :as ex]
   [stks.util.spec :as us]
   [stks.util.storage :refer [storage]]
   [stks.util.time :as dt]
   [stks.util.transit :as t]))

(def re-throw #(rx/throw %))

;; --- GENERAL PURPOSE EVENTS

(defmethod ptk/resolve :initialize
  [_ _]
  (ptk/reify ::initialize
    ptk/WatchEvent
    (watch [_ state stream]
      (let [symbols (:symbols state)]
        (rx/merge
         (rx/from (->> (keys symbols) (map #(ptk/event :schedule-symbol {:id %}))))
         (when-not (some? (:token storage))
           (rx/of (ptk/event :nav {:section :auth}))))))))

(defmethod ptk/resolve :refresh-exchanges
  [_ _]
  (ptk/reify :refresh-exchanges
    ptk/WatchEvent
    (watch [_ state stream]
      (->> (rp/req! :exchanges)
           (rx/map (fn [exchanges]
                     #(assoc % :exchanges exchanges)))))))

(defmethod ptk/resolve :nav
  [_ {:keys [section]}]
  (ptk/reify :navigate
    ptk/UpdateEvent
    (update [_ state]
      (assoc state :nav {:section section}))))

(defmethod ptk/resolve :authenticate
  [_ {:keys [token]}]
  (us/assert string? token)
  (ptk/reify :navigate
    ptk/UpdateEvent
    (update [_ state]
      (assoc state
             :nav {:section :dashboard}))

    ptk/EffectEvent
    (effect [_ state stream]
      (prn :authenticate :effect)
      (swap! storage assoc :token token))))

(defmethod ptk/resolve :logout
  [_ _]
  (ptk/reify :logout
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (dissoc :token)
          (assoc :nav {:section :auth})))))

