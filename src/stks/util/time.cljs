;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.time
  (:require
   [stks.util.object :as obj]
   [cuerdas.core :as str]
   ["luxon" :as lxn]
   ["humanize-duration" :as hmd]))

(def DateTime lxn/DateTime)
(def Duration lxn/Duration)

(defprotocol ITimeMath
  (plus [_ o])
  (minus [_ o]))

(defprotocol ITimeFormat
  (format [_ fmt]))

(defn duration?
  [o]
  (instance? Duration o))

(defn datetime?
  [o]
  (instance? DateTime o))

(defn duration
  [o]
  (cond
    (integer? o)  (.fromMillis Duration o)
    (duration? o) o
    (string? o)   (.fromISO Duration o)
    (map? o)      (.fromObject Duration (clj->js o))
    :else         (throw (js/Error. "unexpected arguments"))))

(defn datetime
  ([s] (datetime s nil))
  ([s {:keys [zone force-zone] :or {zone "local" force-zone false}}]
   (cond
     (integer? s)
     (.fromMillis ^js DateTime s #js {:zone zone :setZone force-zone})

     (map? s)
     (.fromObject ^js DateTime (-> (clj->js s)
                                   (obj/set! "zone" zone)
                                   (obj/set! "setZone" force-zone)))

     :else
     (throw (js/Error. "invalid arguments")))))

(defn epoch->datetime
  ([seconds] (epoch->datetime seconds nil))
  ([seconds {:keys [zone force-zone] :or {zone "local" force-zone false}}]
   (.fromSeconds ^js DateTime seconds #js {:zone zone :setZone force-zone})))

(defn iso->datetime
  "A faster option for transit date parsing."
  [s]
  (.fromISO ^js DateTime s #js {:zone "local"}))

(defn parse-datetime
  ([s] (parse-datetime s :iso nil))
  ([s fmt] (parse-datetime s fmt nil))
  ([s fmt {:keys [zone force-zone] :or {zone "local" force-zone false}}]
   (if (string? fmt)
     (.fromFormat ^js DateTime s fmt #js {:zone zone :setZone force-zone})
     (case fmt
       :iso     (.fromISO ^js DateTime s #js {:zone zone :setZone force-zone})
       :rfc2822 (.fromRFC2822 ^js DateTime s #js {:zone zone :setZone force-zone})
       :http    (.fromHTTP ^js DateTime s #js {:zone zone :setZone force-zone})))))

(defn now
  []
  (.local ^js DateTime))

(defn utc-now
  []
  (.utc ^js DateTime))

(defn ->utc
  [dt]
  (.toUTC ^js dt))

(defn diff
  [dt1 dt2]
  (.diff ^js dt1 dt2))

(extend-protocol IEquiv
  DateTime
  (-equiv [it other]
    (.equals it other))

  Duration
  (-equiv [it other]
    (.equals it other)))

(extend-protocol Inst
  DateTime
  (inst-ms* [inst] (.toMillis ^js inst))

  Duration
  (inst-ms* [inst] (.toMillis ^js inst)))

(extend-protocol IComparable
  DateTime
  (-compare [it other]
    (if ^boolean (.equals it other)
      0
      (if (< (inst-ms it) (inst-ms other)) -1 1)))

  Duration
  (-compare [it other]
    (if ^boolean (.equals it other)
      0
      (if (< (inst-ms it) (inst-ms other)) -1 1))))

(extend-protocol ITimeMath
  DateTime
  (plus [it o]
    (if (map? o)
      (.plus ^js it (clj->js o))
      (.plus ^js it o)))

  (minus [it o]
    (if (map? o)
      (.minus ^js it (clj->js o))
      (.minus ^js it o)))

  Duration
  (plus [it o]
    (if (map? o)
      (.plus ^js it (clj->js o))
      (.plus ^js it o)))

  (minus [it o]
    (if (map? o)
      (.minus ^js it (clj->js o))
      (.minus ^js it o))))

(extend-protocol IPrintWithWriter
  DateTime
  (-pr-writer [p writer opts]
    (-write writer (str/fmt "#stks/datetime \"%s\"" (format p :iso))))

  Duration
  (-pr-writer [p writer opts]
    (-write writer (str/fmt "#stks/duration \"%s\"" (format p :iso)))))

(defn- resolve-format
  [v]
  (case v
    :time-24-simple        (.-TIME_24_SIMPLE ^js DateTime)
    :datetime-short        (.-DATETIME_SHORT ^js DateTime)
    :datetime-med          (.-DATETIME_MED ^js DateTime)
    :datetime-full         (.-DATETIME_FULL ^js DateTime)
    :date-full             (.-DATE_FULL ^js DateTime)
    :date-med-with-weekday (.-DATE_MED_WITH_WEEKDAY ^js DateTime)
    v))

(defn- format-datetime
  [dt fmt]
  (case fmt
    :iso     (.toISO ^js dt)
    :rfc2822 (.toRFC2822 ^js dt)
    :http    (.toHTTP ^js dt)
    :json    (.toJSON ^js dt)
    :date    (.toDate ^js dt)
    :epoch   (js/Math.floor (.toSeconds ^js dt))
    :millis  (.toMillis ^js dt)
    (let [f (resolve-format fmt)]
      (if (string? f)
        (.toFormat ^js dt f)
        (.toLocaleString ^js dt f)))))

(extend-protocol ITimeFormat
  DateTime
  (format [it fmt]
    (format-datetime it fmt))

  Duration
  (format [it fmt]
    (case fmt
      :iso (.toISO it)
      :json (.toJSON it)
      (.toFrormat ^js it fmt))))

;; -- DURATION HUMANIZATION

;; (def ^:private humanizer-options
;;   #js {:language "shortEn"
;;        :spacer ""
;;        :round true
;;        :largest 2
;;        :languages #js {:shortEn #js {:y  (constantly "y")
;;                                      :mo (constantly "mo")
;;                                      :w  (constantly "w")
;;                                      :d  (constantly "d")
;;                                      :h  (constantly "h")
;;                                      :m  (constantly "m")
;;                                      :s  (constantly "s")
;;                                      :ms (constantly "ms")}}})

;; (def ^js js-humanize
;;   (.humanizer hmd humanizer-options))

;; (defn humanize-duration
;;   ([ms] (js-humanize ms))
;;   ([ms {:keys [locale largest round units]
;;         :or {largest 2 round true}}]
;;    (let [params (obj/merge #js {:language "shortEn"
;;                                 :largest largest
;;                                 :round round}
;;                            (when units
;;                              #js {:units (clj->js units)}))]

;;      (js-humanize ms params))))

(defn age
  [t1 t2]
  (let [t1 (inst-ms t1)
        t2 (inst-ms t2)]
    (hmd (- t1 t2)
         #js {:round true
              :language "en"
              :largest 1})))


(defn timeago
  [v]
  (when v
    (let [nowms (inst-ms (now))
          vms   (inst-ms v)]
      (hmd (- nowms vms)
           #js {:round true
                :language "en"
                :largest 1}))))

