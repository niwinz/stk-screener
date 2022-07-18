;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.timers
  (:refer-clojure :exclude [repeat])
  (:require
   [beicon.core :as rx]
   [promesa.core :as p]))

(defn schedule
  ([func]
   (schedule 0 func))
  ([ms func]
   (let [sem (js/setTimeout #(func) ms)
         f   #(js/clearTimeout sem)]
     (specify! f
       rx/IDisposable
       (-dispose [_]
         (^function f))))))

(defn dispose!
  [v]
  (rx/dispose! v))

(defn asap
  [f]
  (-> (p/resolved nil)
      (p/then f)))

(defn repeat
  [ms func]
  (let [sem (js/setInterval #(func) ms)
        f   #(js/clearInterval sem)]
    (specify! f
      rx/IDisposable
      (-dispose [_]
        (^function f)))))

(if (and (exists? js/window) (.-requestIdleCallback js/window))
  (do
    (def ^:private request-idle-callback #(js/requestIdleCallback %))
    (def ^:private cancel-idle-callback #(js/cancelIdleCallback %)))
  (do
    (def ^:private request-idle-callback #(js/setTimeout % 100))
    (def ^:private cancel-idle-callback #(js/clearTimeout %))))

(defn schedule-on-idle
  [func]
  (let [sem (request-idle-callback #(func))
        f   #(cancel-idle-callback sem)]
    (specify! f
      rx/IDisposable
      (-dispose [_]
        (^function f)))))

