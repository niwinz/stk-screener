;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh <niwi@niwi.nz>

(ns stks.core
  (:require
   [lambdaisland.uri :as u]
   [potok.core :as ptk]
   [promesa.core :as p]
   [rumext.v2 :as mf]
   [stks.store :as st]
   [stks.ui :as ui]
   [stks.util.logging :as log]
   [stks.util.webapi :as wa]))

(log/initialize!)
(enable-console-print!)

(defonce root
  (-> "app" wa/get-element mf/create-root))

(log/set-level! :trace)

(defn start
  [& args]
  (log/info :hint "initializing")
  (st/init)
  (st/emit! (ptk/event :setup))
  (mf/render! root (mf/element ui/app)))

(defn stop
  [done]
  ;; an empty line for visual feedback of restart
  (js/console.log "")
  (log/info :hint "stoping")
  (st/emit! (ptk/event :stop))
  (done))

(defn restart
  []
  (st/emit! (ptk/event :stop))
  (start))

(defn ^:dev/after-load after-load
  []
  (restart))
