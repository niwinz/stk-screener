;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.cron
  (:require
   ["cron-parser" :as cp]
   ["cron-parser/lib/date" :as CronDate]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [potok.core :as ptk]
   [stks.util.data :as d]
   [stks.util.exceptions :as ex]
   [stks.util.spec :as us]
   [stks.util.object :as obj]
   [stks.util.time :as dt]))

;; Cron expression:
;; *    *    *    *    *    *
;; ┬    ┬    ┬    ┬    ┬    ┬
;; │    │    │    │    │    |
;; │    │    │    │    │    └ day of week (0 - 7) (0 or 7 is Sun)
;; │    │    │    │    └───── month (1 - 12)
;; │    │    │    └────────── day of month (1 - 31)
;; │    │    └─────────────── hour (0 - 23)
;; │    └──────────────────── minute (0 - 59)
;; └───────────────────────── second (0 - 59, optional)

(extend-protocol Inst
  CronDate
  (inst-ms* [inst] (.toMillis ^js (obj/get inst "_date"))))

(defn ms-until-next
  [repr]
  (let [d (cp/parseExpression repr #js {:tz "UTC"})]
    (- (inst-ms (.next d))
       (inst-ms (dt/now)))))
