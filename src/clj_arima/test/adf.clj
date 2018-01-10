(ns clj-arima.test.adf
  (:require [clj-arima.data.quandl :refer [sample-data]])
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]))

(set-current-implementation :vectorz)

