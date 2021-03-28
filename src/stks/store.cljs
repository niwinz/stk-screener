;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.store
  (:require
   [cuerdas.core :as str]
   [okulary.core :as l]
   [potok.core :as ptk]
   [stks.util.data :as d]
   [stks.util.storage :refer [storage]]
   [stks.util.timers :as tm]))

(defonce state  (ptk/store {:resolve ptk/resolve
                            :state (::state storage)}))
(defonce stream (ptk/input-stream state))

(defmethod ptk/resolve :default
  [type params]
  (ptk/data-event type params))

(def nav-ref
  (l/derived :nav state))

(def message-ref
  (l/derived :message state))

(def symbols-ref
  (l/derived :symbols state))

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
  (swap! storage assoc ::state state))

(defn init
  "Initialize the state materialization."
  ([] (init {}))
  ([props]
   (add-watch state ::persistence #(on-change %4))
   (let [state (::state storage)]
     (emit! #(d/merge % state)))))


