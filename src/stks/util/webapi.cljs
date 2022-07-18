;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.webapi
  (:require
   [lambdaisland.uri :as u]
   [stks.util.exceptions :as ex]
   [stks.util.object :as obj]
   [cuerdas.core :as str]
   [goog.dom :as dom]))

(defn classnames
  [& params]
  (assert (even? (count params)))
  (str/join " " (reduce (fn [acc [k v]]
                          (if (true? (boolean v))
                            (conj acc (name k))
                            acc))
                        []
                        (partition 2 params))))


(defn get-element
  [id]
  (dom/getElement id))

(defn stop-propagation!
  [e]
  (when e
    (.stopPropagation e)))

(defn prevent-default!
  [e]
  (when e
    (.preventDefault e)))

(defn get-target
  "Extract the target from event instance."
  [event]
  (.-target event))

(defn get-value
  "Extract the value from dom node."
  [node]
  (.-value node))

(defn checked?
  "Check if the node that represents a radio
  or checkbox is checked or not."
  [node]
  (.-checked node))

(defn set-value!
  [node value]
  (set! (.-value node) value))

(defn query
  [el query]
  (.querySelector el query))

(defn focus!
  [node]
  (.focus ^js node))

(defn get-current-uri
  []
  (u/uri (.-href js/location)))

