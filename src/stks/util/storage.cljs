;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.storage
  (:require
   [stks.util.transit :as t]
   [stks.util.timers :as tm]
   [stks.util.exceptions :as ex]))

(defn- persist
  [storage prev curr]
  (run! (fn [key]
          (let [prev* (get prev key)
                curr* (get curr key)]
            (when (not= curr* prev*)
              (tm/schedule-on-idle
               #(if (some? curr*)
                  (.setItem ^js storage (t/encode key) (t/encode curr*))
                  (.removeItem ^js storage (t/encode key)))))))

        (into #{} (concat (keys curr)
                          (keys prev)))))

(defn- load
  [storage]
  (let [len (.-length ^js storage)]
    (reduce (fn [res index]
              (let [key (.key ^js storage index)
                    val (.getItem ^js storage key)]
                (try
                  (assoc res (t/decode key) (t/decode val))
                  (catch :default e
                    res))))
            {}
            (range len))))

(defn- make-storage
  [storage]
  (let [data (atom (load storage))]
    (add-watch data :sub #(persist storage %3 %4))
    (reify
      Object
      (toString [_]
        (str "Storage" (pr-str @data)))

      ICounted
      (-count [_]
        (count @data))

      ISeqable
      (-seq [_]
        (seq @data))

      IDeref
      (-deref [_] @data)

      IReset
      (-reset! [self newval]
        (reset! data newval))

      ISwap
      (-swap! [self f]
        (swap! data f))
      (-swap! [self f x]
        (swap! data f x))
      (-swap! [self f x y]
        (swap! data f x y))
      (-swap! [self f x y more]
        (apply swap! data f x y more))

      ILookup
      (-lookup [_ key]
        (get @data key nil))
      (-lookup [_ key not-found]
        (get @data key not-found)))))


(defonce storage
  (make-storage js/localStorage))

(defonce cache
  (make-storage js/sessionStorage))
