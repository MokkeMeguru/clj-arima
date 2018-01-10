(ns clj-arima.difference
  (:require [clj-arima.data.quandl :refer [sample-data]]
            [clj-time.core :as t]
            [clj-time.format :as f])
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]))

(set-current-implementation :vectorz)
;; data
;; [latest ... oldest]

(defn difference
  "k difference"
  [^Integer k ^clojure.lang.PersistentVector data]
  (- (drop-last k data)
     (drop k data)))

(defn- setup-data
  ;; (52 * 7 = 364) + delect (1) + (52 * 7 = 364)
  [rawdata]
  (loop [x rawdata
         newx nil]
    (if (< 364 (count x))
      (recur (vec (drop 365 x)) (concat newx (take 364 x)))
      (concat newx x))))

(defn seasonaldifference
  "seasonaldifference
  return : {:data data :date data}"
  [rawdate
   rawdata]
  (let [date (setup-data rawdate)
        data (setup-data rawdata)
        diff (loop [d data
                    dif []]
               (if (<= 53 (count d))
                 (recur
                  (vec (drop 1 d))
                  (conj dif (- (first d) (nth d (dec 53)))))
                 dif))
        difft (take (count diff) date)]
    {:date difft :data diff}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(count
 (:date
  (seasonaldifference
   (:date sample-data)
   (:close-price sample-data))))

(type (:date sample-data))
(:date sample-data)

(difference 1 (:close-price sample-data))
(count (:close-price sample-data))
(setup-data (:close-price sample-data))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
