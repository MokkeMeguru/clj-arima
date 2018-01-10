(ns clj-arima.view
  (:require [com.hypirion.clj-xchart :as chart]
            [clj-arima.data.quandl :refer [sample-data]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def sample-chart
;;   (chart/xy-chart
;;    {"NIKKEI/INDEX"
;;     {:x (map clj-time.coerce/to-date (:date sample-data))
;;      :y (:close-price sample-data)}}
;;    {:title "NIKKEI/INDEX"
;;     :x-axis {:title "date"}
;;     :y-axis {:title "value"}
;;     :theme :xchart}))
;; (defn test
;;   (chart/view sample-chart))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
