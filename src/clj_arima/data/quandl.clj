(ns clj-arima.data.quandl
  (:require [clj-quandl-api.core :as quandl]
            [clj-time.core :as t]
            [clj-time.coerce :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(quandl/set-api-key! "8zvcussLPAzz7xbdisQb")
(def sample-data (quandl/quandl "NIKKEI/INDEX"
                                :rows 100
                                :collapse "weekly"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
