;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui
  (:require
   [beicon.core :as rx]
   [cuerdas.core :as str]
   [potok.core :as ptk]
   [rumext.v2 :as mf]
   [stks.config :as cf]
   [stks.events]
   [stks.store :as st]
   [stks.ui.auth :refer [auth-section]]
   [stks.ui.screener :refer [screener-section]]
   [stks.ui.header :refer [header]]
   [stks.ui.messages :as ms]
   [stks.ui.strategies :refer [strategies-section]]
   [stks.ui.settings :refer [settings-section]]
   [stks.util.data :as d]
   [stks.util.fontawesome :as fa]
   [stks.util.pprint :as pp]
   [stks.util.spec :as us]))

(mf/defc app
  [props]
  (when-let [nav (mf/deref st/nav-ref)]
    [:main
     [:& header {:nav nav}]
     (case (:section nav)
       :auth       [:& auth-section]
       :settings   [:& settings-section]
       :screener   [:& screener-section]
       :strategies [:& strategies-section]
       [:span "not found"])]))


;; This is a pure frontend error that can be caused by an active
;; assertion (assertion that is preserved on production builds). From
;; the user perspective this should be treated as internal error.
(defmethod ptk/handle-error :assertion
  [{:keys [message hint] :as error}]
  (let [message (or message hint)
        message (str/concat "Internal Assertion Error: " message)
        context (str/ffmt "ns: '%'\nname: '%'\nfile: '%:%'"
                          (:ns error)
                          (:name error)
                          (str/concat cf/public-uri "js/cljs-runtime/" (:file error))
                          (:line error))]

    ;; Print to the console some debugging info
    (js/console.group message)
    (js/console.info context)
    (js/console.log (us/pretty-explain error))
    (js/console.groupEnd message)))


(defmethod ptk/handle-error :authentication
  [error]
  (rx/of (ptk/event :logout)))

;; Error that happens on an active bussines model validation does not
;; passes an validation (example: profile can't leave a team). From
;; the user perspective a error flash message should be visualized but
;; user can continue operate on the application.
(defmethod ptk/handle-error :validation
  [error]
  ;; (ts/schedule
  ;;  (st/emitf
  ;;   (dm/show {:content "Unexpected validation error (server side)."
  ;;             :type :error})))

  ;; Print to the console some debug info.
  (js/console.group "Validation Error")
  (js/console.error (pp/pprint-str error))
  (js/console.log (us/pretty-explain error))
  (js/console.groupEnd "Validation Error"))

(defmethod ptk/handle-error :default
  [error]
  (cond
    (instance? ExceptionInfo error)
    (ptk/handle-error (ex-data error))

    (map? error)
    (ptk/handle-error error)

    :else
    (let [msg (str/concat "Internal error: " (ex-message error))]
      (js/console.group msg)
      (js/console.error "stack:" (.-stack error))
      (js/console.groupEnd msg))))

#_
(defonce uncaught-error-handler
  (letfn [(on-error [event]
            (ptk/handle-error (unchecked-get event "error"))
            (.preventDefault ^js event))]
    (.addEventListener js/window "error" on-error)
    (fn []
      (.removeEventListener js/window "error" on-error))))
