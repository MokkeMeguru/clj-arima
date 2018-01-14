(ns clj-arima.core
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]
        [clojure.core.matrix.dataset])
  (:require [clj-arima.arima :refer [single-arima-forecasting-with-aic
                                     arima-forecasting-with-aic]]
            [clj-arima.view :refer [tschart barchart func-plot view]]
            [clj-arima.test.adf :refer [stationary-adftest]]))

(set-current-implementation :vectorz)
