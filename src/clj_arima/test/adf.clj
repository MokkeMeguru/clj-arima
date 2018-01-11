(ns clj-arima.test.adf
  (:require [clj-arima.data.quandl :refer [sample-data]]
            [clj-arima.util.difference :as diff]
            [clojure.core.matrix.linear :as linear])
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]
        [clojure.core.matrix.dataset]))

(set-current-implementation :vectorz)

(defn- embed
  ([x xlen lag]
   (let [map- (map #(vector % (+ (- xlen lag) %)) (range lag))]
     (array (map #(subvec x (first %) (second %)) map-))))
  ([x xlen lag option]
   (let [map- (map #(vector % (+ lag option %)) (range lag))]
     (array (map #(subvec x (first %) (second %)) map-)))))



(defn- stat-adf-array
  "NOTICE : (dec (count x)) > lag"
  [x lag]
  (let [diffx (diff/difference 1 x)
        diffxlen (count diffx)
        emb (embed diffx diffxlen lag 3)
        r-size (+ 3 lag)
        yt1 (array (vector (subvec x 1 (inc r-size))))
        const (array (vector (take r-size (repeat 1))))
        trend (array (vector (take r-size (iterate dec r-size))))]
    (transpose (array (join yt1 emb const trend)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (shape (stat-adf-array [0 1 2 3 4 5 6 7 8] 3)) ;; [6 6]
;; (pm  (stat-adf-array [0 1 2 3 4 5 6 7 8] 3))
;; [[1.000 -1.000 -1.000 -1.000 1.000 6.000]
;;  [2.000 -1.000 -1.000 -1.000 1.000 5.000]
;;  [3.000 -1.000 -1.000 -1.000 1.000 4.000]
;;  [4.000 -1.000 -1.000 -1.000 1.000 3.000]
;;  [5.000 -1.000 -1.000 -1.000 1.000 2.000]
;;  [6.000 -1.000 -1.000 -1.000 1.000 1.000]]
;;    yt1  \delta yt1 .......   const trend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn stationary-adftest
  [x lag]
  (let [a (stat-adf-array x lag)
        dyt (subvec (diff/difference 1 x) 0 (+ lag 3))
        x (linear/solve a dyt)]
    x))

(stationary-adftest [0 1 2 3 4 5 6 7 8 9] 1)
