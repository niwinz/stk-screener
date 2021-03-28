;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.timers
  (:require
   [promesa.core :as p]))

(defn schedule
  ([func]
   (schedule 0 func))
  ([ms func]
   (let [sem (js/setTimeout #(func) ms)]
     #(js/clearTimeout sem))))

(defn asap
  [f]
  (-> (p/resolved nil)
      (p/then f)))

(defn interval
  [ms func]
  (let [sem (js/setInterval #(func) ms)]
    #(js/clearInterval sem)))

(if (and (exists? js/window) (.-requestIdleCallback js/window))
  (do
    (def ^:private request-idle-callback #(js/requestIdleCallback %))
    (def ^:private cancel-idle-callback #(js/cancelIdleCallback %)))
  (do
    (def ^:private request-idle-callback #(js/setTimeout % 100))
    (def ^:private cancel-idle-callback #(js/clearTimeout %))))

(defn schedule-on-idle
  [func]
  (let [sem (request-idle-callback #(func))]
    #(cancel-idle-callback sem)))
