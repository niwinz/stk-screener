;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.transit
  (:require
   [cognitect.transit :as t]
   [stks.util.time :as dt]))

(def instant-read-handler
  (t/read-handler (fn [value] (dt/iso->datetime value))))

(def duration-read-handler
  (t/read-handler (fn [value] (dt/duration value))))

(def instant-write-handler
  (t/write-handler
   (constantly "instant")
   (fn [v] (dt/format v :iso))))

(def duration-write-handler
  (t/write-handler
   (constantly "duration")
   (fn [v] (inst-ms v))))

(def ^:privare +read-handlers+
  {"u" uuid
   "duration" duration-read-handler
   "instant" instant-read-handler})

(def ^:privare +write-handlers+
  {dt/DateTime instant-write-handler
   dt/Duration duration-write-handler})

;; --- Public Api

(defn decode
  [data]
  (let [r (t/reader :json {:handlers +read-handlers+})]
    (t/read r data)))

(defn encode
  [data]
  (try
    (let [w (t/writer :json {:handlers +write-handlers+})]
      (t/write w data))
    (catch :default e
      (throw e))))
