;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.store
  (:require
   [stks.util.logging :as log]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [potok.core :as ptk]
   [beicon.core :as rx]
   [stks.util.data :as d]
   [stks.util.storage :refer [storage]]
   [stks.util.timers :as tm]))

(log/set-level! :info)

(defonce state  (ptk/store {:resolve ptk/resolve
                            :state (::state storage)}))
(defonce stream (ptk/input-stream state))

(defonce debug-subscription
  (->> stream
       (rx/filter ptk/event?)
       (rx/subs #(log/trace :source "stream"
                            :event (ptk/type %)
                            :data (when (satisfies? IDeref %)
                                    (deref %))))))

(defmethod ptk/resolve :default
  [type params]
  (ptk/data-event type params))

(def nav-ref
  (l/derived :nav state))

(def message-ref
  (l/derived :message state))

(def symbols-ref
  (l/derived :symbols nav-ref))

(def strategies-ref
  (l/derived :strategies nav-ref))

(def signals-ref
  (l/derived :signals state))

(defn emit!
  ([] nil)
  ([event]
   (ptk/emit! state event)
   nil)
  ([event & events]
   (apply ptk/emit! state (cons event events))
   nil))

(defn- on-change
  [state]
  (swap! storage assoc ::state (dissoc state :nav :token)))

(defn init
  "Initialize the state materialization."
  ([] (init {}))
  ([props]
   (add-watch state ::persistence #(on-change %4))
   (let [state (::state storage)]
     (emit! #(merge % state props)))))


