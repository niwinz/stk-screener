;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.exceptions
  "A helpers for work with exceptions."
  (:require [clojure.spec.alpha :as s]))

(s/def ::type keyword?)
(s/def ::code keyword?)
(s/def ::mesage string?)
(s/def ::hint string?)
(s/def ::cause #?(:clj #(instance? Throwable %)
                  :cljs #(instance? js/Error %)))

(s/def ::error-params
  (s/keys :req-un [::type]
          :opt-un [::code
                   ::hint
                   ::cause]))

(defn error
  [& {:keys [hint cause ::data type] :as params}]
  (s/assert ::error-params params)
  (let [payload (-> params
                    (dissoc :cause ::data)
                    (merge data))
        hint    (or hint (pr-str type))]
    (ex-info hint payload cause)))

(defmacro raise
  [& args]
  `(throw (error ~@args)))

(defn try*
  [f on-error]
  (try (f) (catch #?(:clj Exception :cljs :default) e (on-error e))))

;; http://clj-me.cgrand.net/2013/09/11/macros-closures-and-unexpected-object-retention/
;; Explains the use of ^:once metadata

(defmacro ignoring
  [& exprs]
  `(try* (^:once fn* [] ~@exprs) (constantly nil)))

(defmacro try
  [& exprs]
  `(try* (^:once fn* [] ~@exprs) identity))

(defn ex-info?
  [v]
  #?(:cljs (instance? cljs.core.ExceptionInfo v)
     :clj (instance? clojure.lang.ExceptionInfo v)))
