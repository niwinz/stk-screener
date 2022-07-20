;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.data
  "Data manipulation and query helper functions."
  (:refer-clojure :exclude [concat read-string hash-map merge name])
  (:require
   #?(:cljs [cljs.reader :as r]
      :clj  [clojure.edn :as r])
   #?(:clj [clojure.core :as c]
      :cljs [cljs.core :as c])
   [cuerdas.core :as str]))

#?(:cljs
   (extend-protocol cljs.core/INamed
     string
     (-name [s] s)
     (-namespace [s] "")))

(defn name
  "Safer version of cljs.core/name."
  [v]
  (cond
    (keyword? v) (c/name v)
    (string? v)  v
    :else        (str v)))

(defn deep-merge
  ([a b]
   (if (map? a)
     (merge-with deep-merge a b)
     b))
  ([a b & rest]
   (reduce deep-merge a (cons b rest))))

(defn enumerate
  ([items] (enumerate items 0))
  ([items start]
   (loop [idx start
          items items
          res []]
     (if (empty? items)
       res
       (recur (inc idx)
              (rest items)
              (conj res [idx (first items)]))))))

(defn seek
  ([pred coll]
   (seek pred coll nil))
  ([pred coll not-found]
   (reduce (fn [_ x]
             (if (pred x)
               (reduced x)
               not-found))
           not-found coll)))

(defn index-by
  "Return a indexed map of the collection keyed by the result of
  executing the getter over each element of the collection."
  [getter coll]
  (persistent!
   (reduce #(assoc! %1 (getter %2) %2) (transient {}) coll)))

(defn index-of-pred
  [coll pred]
  (loop [c    (first coll)
         coll (rest coll)
         index 0]
    (if (nil? c)
      nil
      (if (pred c)
        index
        (recur (first coll)
               (rest coll)
               (inc index))))))

(defn index-of
  [coll v]
  (index-of-pred coll #(= % v)))

(defn replace-by-id
  ([value]
   (map (fn [item]
          (if (= (:id item) (:id value))
            value
            item))))
  ([coll value]
   (sequence (replace-by-id value) coll)))

(defn without-nils
  "Given a map, return a map removing key-value
  pairs when value is `nil`."
  [data]
  (into {} (remove (comp nil? second) data)))

(defn without-keys
  "Return a map without the keys provided
  in the `keys` parameter."
  [data keys]
  (persistent!
   (reduce #(dissoc! %1 %2) (transient data) keys)))

(defn mapm
  "Map over the values of a map"
  [mfn coll]
  (into {} (map (fn [[key val]] [key (mfn val key)]) coll)))

(defn filterm
  "Filter values of a map that satisfy a predicate"
  [pred coll]
  (into {} (filter pred coll)))

(defn removem
  "Remove values of a map that satisfy a predicate"
  [pred coll]
  (into {} (remove pred coll)))

(def sentinel
  #?(:cljs (js/Object.) :clj (Object.)))

(defn update-in-when
  [m key-seq f & args]
  (let [found (get-in m key-seq sentinel)]
    (if-not (identical? sentinel found)
      (assoc-in m key-seq (apply f found args))
      m)))

(defn update-when
  [m key f & args]
  (let [found (get m key sentinel)]
    (if-not (identical? sentinel found)
      (assoc m key (apply f found args))
      m)))

(defn assoc-in-when
  [m key-seq v]
  (let [found (get-in m key-seq sentinel)]
    (if-not (identical? sentinel found)
      (assoc-in m key-seq v)
      m)))

(defn assoc-when
  [m key v]
  (let [found (get m key sentinel)]
    (if-not (identical? sentinel found)
      (assoc m key v)
      m)))

(defn domap
  "A side effect map version."
  ([f]
   (map (fn [x] (f x) x)))
  ([f coll]
   (map (fn [x] (f x) x) coll)))

(defn- nan?
  [v]
  (not= v v))

(defn read-string
  [v]
  (r/read-string v))

(defn coalesce-str
  [val default]
  (if (or (nil? val) (nan? val))
    default
    (str val)))

(defn coalesce
  [val default]
  (or val default))

(defn nilf
  "Returns a new function that if you pass nil as any argument will
  return nil"
  [f]
  (fn [& args]
    (if (some nil? args)
      nil
      (apply f args))))

(defn nilv
  "Returns a default value if the given value is nil"
  [v default]
  (if (some? v) v default))

(defn keywordize
  "A helper that adapts database style row to clojure style map,
  converting all snake_case attrs into kebab-case, and it only works
  on a first level of the map."
  [m]
  (let [xf #(if (string? %) (keyword (str/replace % #"_" "-")) %)]
    (persistent!
     (reduce-kv (fn [m k v] (assoc! m (xf k) v))
                (transient {}) m))))

