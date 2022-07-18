;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.logging)

(defmacro log
  [& props]
  (let [{:keys [level cause ::logger ::raw]} props]
    `(stks.util.logging/write-log! ~(or logger (str *ns*)) ~level ~cause
                                   (or ~raw (fn [] (stks.util.logging/build-message ~(vec props)))))))

(defmacro info
  [& params]
  `(log :level :info ~@params))

(defmacro error
  [& params]
  `(log :level :error ~@params))

(defmacro warn
  [& params]
  `(log :level :warn ~@params))

(defmacro debug
  [& params]
  `(log :level :debug ~@params))

(defmacro trace
  [& params]
  `(log :level :trace ~@params))

(defmacro set-level!
  ([level]
   `(set-level* ~(str *ns*) ~level))
  ([n level]
   `(set-level* ~n ~level)))
