;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.store
  (:require
   [beicon.core :as rx]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [potok.core :as ptk]
   [stks.util.logging :as log]
   [stks.util.storage :refer [storage]]))

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
  (l/derived :symbols state))

(def strategies-ref
  (l/derived :strategies nav-ref))

(def signals-ref
  (l/derived :signals state))

(def ohlc-ref
  (l/derived :ohlc state))

(def selected-symbols-ref
  (l/derived
   (fn [state]
     (let [selected (-> state :nav :symbols)
           symbols  (:symbols state)]
       (mapv #(get symbols %) selected)))
   state
   =))

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


(defn ^:export dump-state
  []
  (clj->js @state))
