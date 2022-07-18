;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.util.logging
  (:require-macros [stks.util.logging])
  (:require
   [cuerdas.core :as str :include-macros true]
   [fipp.edn :as fpp]
   [goog.log :as glog]
   [stks.util.data :as d]
   [stks.util.exceptions :as ex]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLJ Specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private reserved-props
  #{:level :cause ::logger ::async ::raw ::context})

(def ^:private props-xform
  (comp (partition-all 2)
        (remove (fn [[k]] (contains? reserved-props k)))
        (map vec)))

(defn build-message
  [props]
  (loop [pairs  (sequence props-xform props)
         result []]
    (if-let [[k v] (first pairs)]
      (recur (rest pairs)
             (conj result (str/concat (d/name k) "=" (pr-str v))))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-logger
  [lname]
  (glog/getLogger
   (cond
     (string? lname) lname
     (= lname :root) ""
     (simple-ident? lname) (name lname)
     (qualified-ident? lname) (str (namespace lname) "." (name lname))
     :else (str lname))))

(defn get-level
  [level]
  (case level
    :off     (.-OFF ^js glog/Level)
    :shout   (.-SHOUT ^js glog/Level)
    :error   (.-SEVERE ^js  glog/Level)
    :severe  (.-SEVERE ^js glog/Level)
    :warning (.-WARNING ^js glog/Level)
    :warn    (.-WARNING ^js glog/Level)
    :info    (.-INFO ^js glog/Level)
    :config  (.-CONFIG ^js glog/Level)
    :debug   (.-FINE ^js glog/Level)
    :fine    (.-FINE ^js glog/Level)
    :finer   (.-FINER ^js glog/Level)
    :trace   (.-FINER ^js glog/Level)
    :finest  (.-FINEST ^js glog/Level)
    :all     (.-ALL ^js glog/Level)))

(defn write-log!
  [logger level exception message]
  (when glog/ENABLED
    (let [logger (get-logger logger)
          level  (get-level level)]
      (when (and logger (glog/isLoggable logger level))
        (let [message (if (fn? message) (message) message)
              message (if (string? message) message (str/join ", " message))
              record  (glog/LogRecord. level message (.getName ^js logger))]
          (when exception (.setException record exception))
          (glog/publishLogRecord logger record))))))

(def ^:private colors
  {:gray3  "#8e908c"
   :gray4  "#969896"
   :gray5  "#4d4d4c"
   :gray6  "#282a2e"
   :black  "#1d1f21"
   :red    "#c82829"
   :blue   "#4271ae"
   :orange "#f5871f"})

(defn- level->color
  [level]
  (letfn [(get-level-value [l] (.-value ^js (get-level l)))]
    (condp <= (get-level-value level)
      (get-level-value :error) (get colors :red)
      (get-level-value :warn)  (get colors :orange)
      (get-level-value :info)  (get colors :blue)
      (get-level-value :debug) (get colors :gray4)
      (get-level-value :trace) (get colors :gray3)
      (get colors :gray2))))

(defn- level->short-name
  [l]
  (case l
    :fine "DBG"
    :debug "DBG"
    :finer "TRC"
    :trace "TRC"
    :info "INF"
    :warn "WRN"
    :warning "WRN"
    :error "ERR"
    (subs (.-name ^js (get-level l)) 0 3)))

(defn set-level*
  "Set the level (a keyword) of the given logger, identified by name."
  [name lvl]
  (some-> (get-logger name)
          (glog/setLevel (get-level lvl))))

(defn set-levels!
  [lvls]
  (doseq [[logger level] lvls
          :let [level (if (string? level) (keyword level) level)]]
    (set-level* logger level)))

(defn- prepare-message
  [message]
  (loop [kvpairs  (seq message)
         message  []
         specials []]
    (if (nil? kvpairs)
      [message specials]
      (let [[k v] (first kvpairs)]
        (cond
          (= k :err)
          (recur (next kvpairs)
                 message
                 (conj specials [:error nil v]))

          (and (qualified-ident? k)
               (= "js" (namespace k)))
          (recur (next kvpairs)
                 message
                 (conj specials [:js (name k) (if (object? v) v (clj->js v))]))

          :else
          (recur (next kvpairs)
                 (conj message (str/concat (d/name k) "=" (pr-str v)))
                 specials))))))

(defn default-handler
  [{:keys [message level logger-name exception] :as params}]
  (let [header-styles (str "font-weight: 600; color: " (level->color level))
        normal-styles (str "font-weight: 300; color: " (get colors :gray6))
        level-name    (level->short-name level)
        header        (str "%c" level-name " [" logger-name "] ")]

    (if (string? message)
      (let [message (str header "%c" message)]
        (js/console.log message header-styles normal-styles))
      (let [[message specials] (prepare-message message)]
        (if (seq specials)
          (let [message (str header "%c" message)]
            (js/console.group message header-styles normal-styles)
            (doseq [[type n v] specials]
              (case type
                :js (js/console.log n v)
                :error (if (ex/ex-info? v)
                         (js/console.error (pr-str v))
                         (js/console.error v))))
            (js/console.groupEnd message))
          (let [message (str header "%c" message)]
            (js/console.log message header-styles normal-styles)))))

    (when exception
      (when-let [data (ex-data exception)]
        (js/console.error "cause data:" (pr-str data)))
      (js/console.error (.-stack exception)))))


(defn record->map
  [^js record]
  {:seqn (.-sequenceNumber_ record)
   :time (.-time_ record)
   :level (keyword (str/lower (.-name (.-level_ record))))
   :message (.-msg_ record)
   :logger-name (.-loggerName_ record)
   :exception (.-exception_ record)})

(defonce default-console-handler
  (comp default-handler record->map))

(defn initialize!
  []
  (let [l (get-logger :root)]
    (glog/removeHandler l default-console-handler)
    (glog/addHandler l default-console-handler)
    nil))
