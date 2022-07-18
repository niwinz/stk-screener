;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui.auth
  (:require
   [cuerdas.core :as str]
   [potok.core :as ptk]
   [rumext.v2 :as mf]
   [stks.events]
   [stks.store :as st]
   [stks.ui.messages :as ms]
   [stks.util.data :as d]
   [stks.util.fontawesome :as fa]
   [stks.util.webapi :as wa]))

(mf/defc auth-section
  [props]
  (let [token (mf/use-state "")
        input (mf/use-ref)

        on-change
        (mf/use-callback
         (fn [event]
           (let [value (-> (wa/get-target event)
                           (wa/get-value))]
             (reset! token (str/trim value)))))

        on-error
        (mf/use-callback
         (fn []
           (wa/focus! (mf/ref-val input))))

        on-submit
        (mf/use-callback
         (mf/deps @token)
         (fn [event]
           (wa/prevent-default! event)
           (let [params (with-meta {:token @token}
                          {:on-error on-error})]
             (st/emit! (ptk/event :authenticate params)))))]

    [:section.auth-section
     [:h2 "Authentication Dialog"]
     [:form {:on-submit on-submit}
      [:div.row
       [:input {:placeholder "FINNHUB Token"
                :type "text"
                :value @token
                :ref input
                :on-change on-change}]]
      [:div.row
       [:input {:disabled (str/blank? @token)
                :type "submit"
                :value "Submit"}]]]]))

