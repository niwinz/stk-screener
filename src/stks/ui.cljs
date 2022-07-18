;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui
  (:require
   [cljs.pprint :refer [pprint]]
   [stks.util.pprint :as pp]
   [cuerdas.core :as str]
   [expound.alpha :as expound]
   [potok.core :as ptk]
   [rumext.v2 :as mf]
   [stks.config :as cf]
   [stks.events]
   [stks.store :as st]
   [stks.ui.auth :refer [auth-section]]
   [stks.ui.dashboard :refer [dashboard-section]]
   [stks.ui.header :refer [header]]
   [stks.ui.messages :as ms]
   [stks.ui.strategies :refer [strategies-section]]
   [stks.ui.symbols :refer [symbols-section]]
   [stks.util.data :as d]
   [stks.util.spec :as us]
   [stks.util.fontawesome :as fa]))

(mf/defc app
  [props]
  (when-let [nav (mf/deref st/nav-ref)]
    [:main
     [:& header {:nav nav}]
     (case (:section nav)
       :auth       [:& auth-section]
       :symbols    [:& symbols-section]
       :dashboard  [:& dashboard-section]
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
  (if (instance? ExceptionInfo error)
    (ptk/handle-error (ex-data error))
    (do
      #_(ts/schedule
       (st/emitf (dm/assign-exception error)))

      (js/console.group "Internal error:")
      (js/console.log "hint:" (or (ex-message error)
                                  (:hint error)
                                  (:message error)))
      (js/console.error (clj->js error))
      (js/console.error "stack:" (.-stack error))
      (js/console.groupEnd "Internal error:"))))

#_
(defonce uncaught-error-handler
  (letfn [(on-error [event]
            (ptk/handle-error (unchecked-get event "error"))
            (.preventDefault ^js event))]
    (.addEventListener js/window "error" on-error)
    (fn []
      (.removeEventListener js/window "error" on-error))))
