;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.core
  (:require
   [lambdaisland.glogi :as log]
   [lambdaisland.glogi.console :as glogi-console]
   [promesa.core :as p]
   [rumext.alpha :as mf]
   [stks.store :as st]
   [stks.ui :as ui]
   [stks.util.dom :as dom]))

(glogi-console/install!)
(enable-console-print!)

(defn start
  [& args]
  (log/info :msg "initializing")
  (st/init)
  (mf/mount (mf/element ui/app)
            (dom/get-element "app")))

(defn stop
  [done]
  ;; an empty line for visual feedback of restart
  (js/console.log "")

  (log/info :msg "stoping")
  (done))

(defn restart
  []
  (mf/unmount (dom/get-element "app"))
  (mf/unmount (dom/get-element "modal"))
  (start))

(defn ^:dev/after-load after-load
  []
  (restart))
