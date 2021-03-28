;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.spec
  "Data manipulation and query helper functions."
  (:refer-clojure :exclude [assert])
  #?(:cljs (:require-macros [stks.util.spec :refer [assert]]))
  (:require
   #?(:clj  [clojure.spec.alpha :as s]
      :cljs [cljs.spec.alpha :as s])
   [expound.alpha :as expound]
   [stks.util.exceptions :as ex]
   [cuerdas.core :as str]))

(s/check-asserts true)

;; --- Conformers

(defn boolean-conformer
  [v]
  (if (boolean? v)
    v
    (if (string? v)
      (if (re-matches #"^(?:t|true|false|f|0|1)$" v)
        (contains? #{"t" "true" "1"} v)
        ::s/invalid)
      ::s/invalid)))

(defn boolean-unformer
  [v]
  (if v "true" "false"))

(defn- number-conformer
  [v]
  (cond
    (number? v) v
    (str/numeric? v)
    #?(:clj (Double/parseDouble v)
       :cljs (js/parseFloat v))
    :else ::s/invalid))

(defn- integer-conformer
  [v]
  (cond
    (integer? v) v
    (string? v)
    (if (re-matches #"^[-+]?\d+$" v)
      #?(:clj (Long/parseLong v)
         :cljs (js/parseInt v 10))
      ::s/invalid)
    :else ::s/invalid))

(defn- color-conformer
  [v]
  (if (and (string? v) (re-matches #"^#(?:[0-9a-fA-F]{3}){1,2}$" v))
    v
    ::s/invalid))

(defn keyword-conformer
  [v]
  (cond
    (keyword? v)
    v

    (string? v)
    (keyword v)

    :else
    ::s/invalid))

;; --- Default Specs

(s/def ::keyword (s/conformer keyword-conformer name))
(s/def ::inst inst?)
(s/def ::string string?)
(s/def ::color (s/conformer color-conformer str))
(s/def ::boolean (s/conformer boolean-conformer boolean-unformer))
(s/def ::number (s/conformer number-conformer str))
(s/def ::integer (s/conformer integer-conformer str))
(s/def ::not-empty-string (s/and string? #(not (str/empty? %))))
(s/def ::url string?)
(s/def ::fn fn?)

;; --- Macros

(defn spec-assert*
  [spec x message context]
  (if (s/valid? spec x)
    x
    (let [data    (s/explain-data spec x)
          explain (with-out-str (s/explain-out data))]
      (ex/raise :type :assertion
                :code :spec-validation
                :hint message
                :data data
                :explain explain
                :context context
                #?@(:cljs [:stack (.-stack (ex-info message {}))])))))


(defmacro assert
  "Development only assertion macro."
  [spec x]
  (when *assert*
    (let [nsdata  (:ns &env)
          context (when nsdata
                    {:ns (str (:name nsdata))
                     :name (pr-str spec)
                     :line (:line &env)
                     :file (:file (:meta nsdata))})
          message (str "Spec Assertion: '" (pr-str spec) "'")]
      `(spec-assert* ~spec ~x ~message ~context))))

(defmacro verify
  "Always active assertion macro (does not obey to :elide-asserts)"
  [spec x]
  (let [nsdata  (:ns &env)
        context (when nsdata
                  {:ns (str (:name nsdata))
                   :name (pr-str spec)
                   :line (:line &env)
                   :file (:file (:meta nsdata))})
        message (str "Spec Assertion: '" (pr-str spec) "'")]
    `(spec-assert* ~spec ~x ~message ~context)))

;; --- Public Api

(defn conform
  [spec data]
  (let [result (s/conform spec data)]
    (when (= result ::s/invalid)
      (let [data    (s/explain-data spec data)
            explain (with-out-str
                      (s/explain-out data))]
        (throw (ex/error :type :validation
                         :code :spec-validation
                         :explain explain
                         :data data))))
    result))

(defmacro instrument!
  [& {:keys [sym spec]}]
  (when *assert*
    (let [message (str "Spec failed on: " sym)]
      `(let [origf# ~sym
             mdata# (meta (var ~sym))]
         (set! ~sym (fn [& params#]
                      (spec-assert* ~spec params# ~message mdata#)
                      (apply origf# params#)))))))

