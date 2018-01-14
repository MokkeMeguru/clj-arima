(ns clj-arima.arima
  (:require [clojure.core.matrix.linear :as linear]
            [clojure.core.matrix.stats :as stats]
            [clj-arima.util :as util]
            [clj-arima.arima.ar :as ar]
            [clj-arima.arima.ma :as ma]
            ;; [clj-arima.data.quandl :refer [sample-data]]
            [clj-arima.util.difference :as diff]
            [clj-arima.test.adf :as adf]
            [clj-arima.util.log-likelihood :as likelihood]
            [incanter.core :as i])
  (:use [clojure.core.matrix]
        ;;[clojure.core.matrix.operators]
        ;; [clojure.core.matrix.dataset]
        )
  (:import [org.apache.commons.math3.optim.nonlinear.scalar ObjectiveFunction]
           [org.apache.commons.math3.analysis MultivariateFunction]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv SimplexOptimizer]
           [org.apache.commons.math3.optim OptimizationData]
           [org.apache.commons.math3.optim MaxEval]
           [org.apache.commons.math3.optim.nonlinear.scalar GoalType]
           [org.apache.commons.math3.optim InitialGuess]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv NelderMeadSimplex]))

;; (set-current-implementation :vectorz)

(defn arma [yseq eseq pseq qseq sigma]
  (let [e (util/normal-dist 1 :sd sigma)
        ar (apply + (map * yseq pseq))
        ma (apply + (map * eseq qseq))
        y (+ ar ma e)]
    (cons y (lazy-seq
             (arma (cons y yseq)
                   (cons e eseq)
                   pseq qseq sigma)))))

(defn objective-function [f]
  (ObjectiveFunction. (reify MultivariateFunction
                        (value [_ v]
                          (f (vec v))))))

(defn arma-model [p q yseq]
  (let [m (+ p q)
        f (fn [params]
            (likelihood/log-likelihood params p q yseq))
        optimal (.optimize (SimplexOptimizer. 1e-10 1e-10)
                           (into-array
                            OptimizationData
                            [(MaxEval. 100000)
                             (objective-function f)
                             GoalType/MAXIMIZE
                             (->> (repeat 0.0)
                                  (take m)
                                  (double-array)
                                  (InitialGuess.))
                             (->> (repeat 0.1)
                                  (take m)
                                  (double-array)
                                  (NelderMeadSimplex.))]))
        point (-> optimal .getPoint vec)
        value (-> optimal .getValue)]
    {:ar (take p point)
     :ma (drop p point)
     :ll value}))

(defn arima-forecasting-with-aic* [maxp d maxq data len]
  (let [test (remove (partial = [0 0])
                     (for [p (range (inc maxp)) q (range (inc maxq))] [p q]))
        diffdata (diff/difference d data)
        arma-suggest (map (fn [[p q]] (arma-model p q diffdata)) test)
        aic (fn [{:keys [ar ma ll] :as params}]
              (assoc params
                     :aic (- (* 2 (+ (count ar) (count ma) 1))
                             (* 2 ll))))
        mapped-arma-suggest (map aic arma-suggest)
        best-param (loop [d mapped-arma-suggest
                          best nil]
                     (cond
                       (= '() d) best
                       (nil? best) (recur (rest d) (first d))
                       (> (:aic best) (:aic (first d))) (recur (rest d) (first d))
                       :else (recur (rest d) best)))
        init (take 12 diffdata)
        forecasts (array
                   (for [n (range 1000)]
                     (take len
                           (arma init (vec (util/normal-dist 2 :mean 0 :sd 0.01))
                                 (:ar best-param)
                                 (:ma best-param) 0.0449))))
        forecast-mean (-> (map stats/mean (transpose forecasts)))
        forecast-sd (-> (map stats/sd (transpose forecasts))
                        (div 2)
                        (emul 1.96))
        upper (->> (map + forecast-mean forecast-sd)
                   (concat diffdata))
        lower (->> (map - forecast-mean forecast-sd)
                   (concat diffdata))
        n (count upper)]
    [upper lower n d best-param]))

(defn arima-forecasting-with-aic
  "
  in [latest ..... oldest]
  out [oldest ..... latest]
  "
  [maxp d maxq data len]
  (let [rdata (reverse data)
        [upper lower n d best-param]
        (arima-forecasting-with-aic* maxp d maxq rdata len)
        start (first rdata)
        upper-res (diff/undifference (take d rdata) (* -1 upper))
        lower-res (diff/undifference (take d rdata) (* -1 upper))]
    {:upper upper-res :lower lower-res :n n :d d
     :p (count (:ar best-param)) :q (count (:ma best-param))
     :aic (:aic best-param)}))

(defn single-arima-forecasting-with-aic* [p d q data len]
  (let [diffdata (diff/difference d data)
        arma-suggest (arma-model p q diffdata)
        aic (fn [{:keys [ar ma ll] :as params}]
              (assoc params
                     :aic (- (* 2 (+ (count ar) (count ma) 1))
                             (* 2 ll))))
        best-param (aic arma-suggest)
        init (take 12 diffdata)
        forecasts (array
                   (for [n (range 1000)]
                     (take len
                           (arma init (vec (util/normal-dist 2 :mean 0 :sd 0.01))
                                 (:ar best-param)
                                 (:ma best-param) 0.0449))))
        forecast-mean (-> (map stats/mean (transpose forecasts)))
        forecast-sd (-> (map stats/sd (transpose forecasts))
                        (div 2)
                        (emul 1.96))
        upper (->> (map + forecast-mean forecast-sd)
                   (concat diffdata))
        lower (->> (map - forecast-mean forecast-sd)
                   (concat diffdata))
        n (count upper)]
    [upper lower n d (:aic best-param)]))

(defn single-arima-forecasting-with-aic
  "
  in [latest ..... oldest]
  out [oldest ..... latest]
  "
  [maxp d maxq data len]
  (let [rdata (reverse data)
        [upper lower n d best-aic]
        (arima-forecasting-with-aic* maxp d maxq rdata len)
        start (first rdata)
        upper-res (diff/undifference (take d rdata) (* -1 upper))
        lower-res (diff/undifference (take d rdata) (* -1 upper))]
    {:upper upper-res :lower lower-res :n n :d d
     :p (:ar best-aic) :q (:ma best-aic) :aic (:aic best-aic)}))

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
;; (let [init (util/normal-dist 2)
;;       coefs [0 0.5]
;;       data (take 100 (ar/ar init coefs 0.2))
;;       f (fn [coef]
;;           (likelihood/log-likelihood [0 coef] 2 0 data))]
;;   (f 0.1))
;;
;; (arma-model 2 1 (->> (:close-price sample-data) (diff/difference 1)))
;;
;;
;; (def w (arima-forecasting-with-aic 2 1 2 (:close-price sample-data) 10))
;;
;; (clj-arima.view/view
;;  (clj-arima.view/tschart (:upper w)))
;;
;; (def w (arima-forecasting-with-aic 2 1 2 (:close-price sample-data) 10))
;; (:q w)
;; (def w2 (single-arima-forecasting-with-aic 2 1 2 (:close-price sample-data) 10))
;; (:aic w2)
;;
;; (clj-arima.view/view
;;  (clj-arima.view/tschart (:upper w2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
