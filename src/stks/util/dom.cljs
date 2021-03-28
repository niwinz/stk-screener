;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.dom
  (:require
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


;; --- New methods

(defn get-element-by-class
  ([classname]
   (dom/getElementByClass classname))
  ([classname node]
   (dom/getElementByClass classname node)))

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

(defn get-parent
  [dom]
  (.-parentElement ^js dom))

(defn get-value
  "Extract the value from dom node."
  [node]
  (.-value node))

(defn get-attribute
  "Extract the value of one attribute of a dom node."
  [node attr-name]
  (.getAttribute node attr-name))

(def get-target-val (comp get-value get-target))

(defn click
  "Click a node"
  [node]
  (.click node))

(defn checked?
  "Check if the node that represents a radio
  or checkbox is checked or not."
  [node]
  (.-checked node))

(defn valid?
  "Check if the node that is a form input
  has a valid value, against html5 form validation
  properties (required, min/max, pattern...)."
  [node]
  (.-valid (.-validity node)))

(defn set-value!
  [node value]
  (set! (.-value node) value))

(defn ^boolean equals?
  [node-a node-b]
  (.isEqualNode ^js node-a node-b))

(defn create-element
  ([tag]
   (.createElement js/document tag))
  ([ns tag]
   (.createElementNS js/document ns tag)))

(defn set-html!
  [el html]
  (set! (.-innerHTML el) html))

(defn append-child!
  [el child]
  (.appendChild el child))

(defn get-first-child
  [el]
  (.-firstChild el))

(defn get-tag-name
  [el]
  (.-tagName el))

(defn get-outer-html
  [el]
  (.-outerHTML el))

(defn get-inner-text
  [el]
  (.-innerText el))

(defn query
  [el query]
  (.querySelector el query))

(defn focus!
  [node]
  (.focus ^js node))

(defn ^boolean blob?
  [v]
  (instance? js/Blob v))

(defn create-blob
  "Create a blob from content."
  ([content]
   (create-blob content "application/octet-stream"))
  ([content mimetype]
   (js/Blob. #js [content] #js {:type mimetype})))

(defn revoke-uri
  [url]
  (js/URL.revokeObjectURL url))

(defn create-uri
  "Create a url from blob."
  [b]
  {:pre [(blob? b)]}
  (js/URL.createObjectURL b))

(defn get-root []
  (query js/document "#app"))

(defn ^boolean has-class?
  [node class-name]
  (let [class-list (.-classList ^js node)]
    (.contains ^js class-list class-name)))
