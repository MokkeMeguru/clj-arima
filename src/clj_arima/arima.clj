(ns clj-arima.arima
  (:require [clojure.core.matrix.linear :as linear]
            [clojure.core.matrix.stats :as stats]
            [clj-arima.util :as util]
            [clj-arima.arima.ar :as ar]
            [clj-arima.arima.ma :as ma]
            [clj-arima.data.quandl :refer [sample-data]]
            [clj-arima.util.difference :as diff]
            [clj-arima.test.adf :as adf]
            [clj-arima.util.log-likelihood :as likelihood])
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]
        [clojure.core.matrix.dataset]))

(set-current-implementation :vectorz)

(defn arma [yseq eseq pseq qseq sigma]
  (let [e (util/normal-dist 1 :sd sigma)
        ar (apply + (map * yseq pseq))
        ma (apply + (map * eseq qseq))
        y (+ ar ma e)]
    (cons y (lazy-seq
             (arma (cons y yseq)
                   (cons e eseq)
                   pseq qseq sigma)))) libgfortran3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (let [ys (util/normal-dist 10 :sd 1.0)
;;       es (util/normal-dist 10 :sd 0.2)
;;       ps [0 0 0 0.3 0.5]
;;       qs [0.2 0.8]]
;;   (->> (arma ys es ps qs 0.2)
;;        (take 500)
;;        (util/acf)
;;        (take 20)
;;        (clj-arima.view/barchart)
;;        (clj-arima.view/view)))
;; (let [ys (util/normal-dist 10 :sd 1.0)
;;       es (util/normal-dist 10 :sd 0.2)
;;       ps [0 0 0 0.3 0.5]
;;       qs [0.2 0.8]]
;;   (->> (arma ys es ps qs 0.2)
;;        (take 500)
;;        (clj-arima.view/tschart)
;;        (clj-arima.view/view)))
;;
;; (let [ys (util/normal-dist 10 :sd 1.0)
;;       es (util/normal-dist 10 :sd 0.2)
;;       ps   [0 0 0 0.3 0.5]
;;       qs   [0.2 0.8]]
;;   (->> (arma ys es ps qs 0.2)
;;        (take 500)
;;        (util/pacf)
;;        (take 15)
;;        (clj-arima.view/barchart)
;;        (clj-arima.view/view)))
;;
;; (->> (:close-price sample-data)
;;      (diff/difference 3)
;;      (util/acf)
;;      (take 25)
;;      (clj-arima.view/barchart)
;;      (clj-arima.view/view))
;;
;; (util/acf-pacf-line (:close-price sample-data))
;;
;; (adf/stationary-adftest (:close-price sample-data) 1 false)
;;
;;
;; (->> (second
;;       (second
;;        (diff/seasonaldifference
;;         (:date sample-data)
;;         (:close-price sample-data))))
;;      (util/acf)
;;      (take 25)
;;      (clj-arima.view/barchart)
;;      (clj-arima.view/view))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let [init (util/normal-dist 2)
      coefs [0 0.5]
      data (take 100 (ar/ar init coefs 0.2))
      f (fn [coef]
          (likelihood/log-likelihood [0 coef] 2 0 data))]
  (f 0.1))
