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
   [stks.util.data :as d]
   [stks.util.exceptions :as ex]
   [cuerdas.core :as str]))

(s/check-asserts true)

;; --- SPEC: boolean

(letfn [(conformer [v]
          (if (boolean? v)
            v
            (if (string? v)
              (if (re-matches #"^(?:t|true|false|f|0|1)$" v)
                (contains? #{"t" "true" "1"} v)
                ::s/invalid)
              ::s/invalid)))
        (unformer [v]
          (if v "true" "false"))]
  (s/def ::boolean (s/conformer conformer unformer)))

;; --- SPEC: number

(letfn [(conformer [v]
          (cond
            (number? v)      v
            (str/numeric? v) #?(:cljs (js/parseFloat v)
                                :clj  (Double/parseDouble v))
            :else            ::s/invalid))]
  (s/def ::number (s/conformer conformer str)))

;; --- SPEC: integer

(letfn [(conformer [v]
          (cond
            (integer? v) v
            (string? v)
            (if (re-matches #"^[-+]?\d+$" v)
              #?(:clj (Long/parseLong v)
                 :cljs (js/parseInt v 10))
              ::s/invalid)
            :else ::s/invalid))]
  (s/def ::integer (s/conformer conformer str)))

;; --- SPEC: keyword

(letfn [(conformer [v]
          (cond
            (keyword? v) v
            (string? v)  (keyword v)
            :else        ::s/invalid))

        (unformer [v]
          (d/name v))]
  (s/def ::keyword (s/conformer conformer unformer)))

;; --- SPEC: set of Keywords

(letfn [(conform-fn [dest s]
          (let [xform (keep (fn [s]
                              (cond
                                (string? s) (keyword s)
                                (keyword? s) s
                                :else nil)))]
            (cond
              (set? s)    (into dest xform s)
              (string? s) (into dest xform (str/words s))
              :else       ::s/invalid)))]

  (s/def ::set-of-keywords
    (s/conformer
     (fn [s] (conform-fn #{} s))
     (fn [s] (str/join " " (map name s)))))

  (s/def ::vec-of-keywords
    (s/conformer
     (fn [s] (conform-fn [] s))
     (fn [s] (str/join " " (map name s))))))


;; --- SPEC: set-of-kw

(letfn [(conformer [v]
          (cond
            (string? v) (into #{} (map keyword) (str/words v))
            (vector? v) (into #{} (map str/keyword) v)
            (set? v)    (into #{} (filter keyword?) v)
            :else       ::s/invalid))
        (unformer [v]
          (into #{} (map d/name) v))]
  (s/def ::set-of-kw (s/conformer conformer unformer)))

;; --- SPEC: set-of-str

(letfn [(conformer [v]
          (cond
            (string? v) (into #{} (map str/trim) (str/split v #","))
            (vector? v) (into #{} v)
            (set? v)    (into #{} (filter string?) v)
            :else       ::s/invalid))
        (unformer [v] v)]
  (s/def ::set-of-str (s/conformer conformer unformer)))

;; --- Default Specs

(s/def ::inst inst?)
(s/def ::string string?)
(s/def ::not-empty-string (s/and string? #(not (str/empty? %))))
(s/def ::url string?)
(s/def ::fn fn?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn explain-data
  [spec value]
  (s/explain-data spec value))

(defn valid?
  [spec value]
  (s/valid? spec value))

(defmacro assert-expr*
  "Auxiliar macro for expression assertion."
  [expr hint]
  `(when-not ~expr
     (ex/raise :type :assertion
               :code :expr-validation
               :hint ~hint)))

(defmacro assert-spec*
  "Auxiliar macro for spec assertion."
  [spec value hint]
  (let [context (if-let [nsdata (:ns &env)]
                  {:ns (str (:name nsdata))
                   :name (pr-str spec)
                   :line (:line &env)
                   :file (:file (:meta nsdata))}
                  {:ns   (str (ns-name *ns*))
                   :name (pr-str spec)
                   :line (:line (meta &form))})
        hint    (or hint (str "spec assert: " (pr-str spec)))]

    `(if (valid? ~spec ~value)
       ~value
       (let [data# (explain-data ~spec ~value)]
         (ex/raise :type :assertion
                   :code :spec-validation
                   :hint ~hint
                   ::ex/data (merge ~context data#))))))

(defmacro assert
  "Is a spec specific assertion macro that only evaluates if *assert*
  is true. DEPRECATED: it should be replaced by the new, general
  purpose assert! macro."
  [spec value]
  (when *assert*
    `(assert-spec* ~spec ~value nil)))

(defmacro verify
  "Is a spec specific assertion macro that evaluates always,
  independently of *assert* value. DEPRECATED: should be replaced by
  the new, general purpose `verify!` macro."
  [spec value]
  `(assert-spec* ~spec ~value nil))

(defmacro assert!
  "General purpose assertion macro."
  [& params]
  ;; If we only receive two arguments, this means we use the simplified form
  (let [pcnt (count params)]
    (cond
      ;; When we have a single argument, this means a simplified form
      ;; of expr assertion
      (= 1 pcnt)
      (let [expr (first params)
            hint (str "expr assert failed:" (pr-str expr))]
        (when *assert*
          `(assert-expr* ~expr ~hint)))

      ;; If we have two arguments, this can be spec or expr
      ;; assertion. The spec assertion is determined if the first
      ;; argument is a qualified keyword.
      (= 2 pcnt)
      (let [[spec-or-expr value-or-msg] params]
        (if (qualified-keyword? spec-or-expr)
          `(assert-spec* ~spec-or-expr ~value-or-msg nil)
          `(assert-expr* ~spec-or-expr ~value-or-msg)))

      (= 3 pcnt)
      (let [[spec value hint] params]
        `(assert-spec* ~spec ~value ~hint))

      :else
      (let [{:keys [spec expr hint always? val]} params]
        (when (or always? *assert*)
          (if spec
            `(assert-spec* ~spec ~val ~hint)
            `(assert-expr* ~expr ~hint)))))))

(defmacro verify!
  "A variant of `assert!` macro that evaluates always, independently
  of the *assert* value."
  [& params]
  (binding [*assert* true]
    `(assert! ~@params)))

;; --- Public Api


(defn conform
  [spec data]
  (let [result (s/conform spec data)]
    (when (= result ::s/invalid)
      (let [data (s/explain-data spec data)]
        (throw (ex/error :type :validation
                         :code :spec-validation
                         ::ex/data data))))
    result))

(defn pretty-explain
  ([data] (pretty-explain data nil))
  ([data {:keys [max-problems] :or {max-problems 10}}]
   (when (and (contains? data ::s/problems)
              (contains? data ::s/value)
              (contains? data ::s/spec))
     (binding [s/*explain-out* expound/printer]
       (with-out-str
         (s/explain-out (update data ::s/problems #(take max-problems %))))))))

