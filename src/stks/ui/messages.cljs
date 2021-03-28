;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.ui.messages
  (:require
   [stks.events.messages :as em]
   [stks.store :as st]
   [stks.util.dom :as dom]
   [stks.util.fontawesome :as fa]
   [potok.core :as ptk]
   [rumext.alpha :as mf]))

(defn- type->icon
  [type]
  (mf/html
   (case type
    :warning [:& fa/icon {:name "exclamation"}]
    :error   [:& fa/icon {:name "bomb"}]
    :success [:& fa/icon {:name "check"}]
    :info    [:& fa/icon {:name "info"}])))

(mf/defc message-item
  [{:keys [type status on-close quick? content] :as props}]
  (let [klass (dom/classnames
               :success (= type :success)
               :error   (= type :error)
               :info    (= type :info)
               :warning (= type :warning)
               :hide    (= status :hide)
               :quick   quick?)]
    [:section.message {:class klass}
     [:div.content
      [:div.icon (type->icon type)]
      [:span content]]
     [:div.close {:on-click on-close}
      [:& fa/icon {:name "times"}]]]))

(mf/defc messages
  {::mf/wrap [mf/memo]}
  []
  (let [message  (mf/deref st/message-ref)
        ;; message  {:type :success
        ;;           :content "kaka de vaca"}
        on-close (mf/use-fn #(st/emit! (em/hide)))]
    (when message
      [:& message-item
       {:type (:type message)
        :quick? (boolean (:timeout message))
        :status (:status message)
        :content (:content message)
        :on-close on-close}])))



