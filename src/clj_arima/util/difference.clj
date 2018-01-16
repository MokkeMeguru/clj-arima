(ns clj-arima.util.difference
  (:require ;; [clj-arima.data.quandl :refer [sample-data]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            )
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]
        ;; [clojure.core.matrix.dataset]
        ))

;; data
;; [latest ... oldest]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn difference
  "k difference"
  [^Integer k ^clojure.lang.PersistentVector data]
  (if (zero? k)
    data
    (difference (dec k) (- (drop-last k data)
                           (drop k data)))))

(defn- setup-data
  [rawdata]
  (loop [x rawdata
         newx nil]
    (if (< 364 (count x))
      (recur (vec (drop 365 x)) (concat newx (take 364 x)))
      (concat newx x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-seeds [mat]
  (if (= 1 (count mat))
    mat
    (reverse
     (cons (first mat)
           (reverse
            (map #(-> % last (* -1))
                 (loop [m (reverse (difference 1 mat))
                        t (dec (count mat))
                        acc (list)]
                   (if (zero? t) acc
                       (recur (difference 1 m) (dec t) (cons m acc))))))))))

(defn undifference* [start preseq]
  (loop [pre start
         pseq preseq
         acc (list start)]
    (if (= '() pseq)
      (reverse acc)
      (let [post (+ pre (first pseq))]
        (recur post (rest pseq) (cons post acc))))))

(defn undifference [mat diff]
  (loop [m mat
         d diff]
    (if (= '() m)
      d
      (recur (rest m) (undifference* (first m) d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (count
;;  (:date
;;   (seasonaldifference
;;    (:date sample-data)
;;    (:close-price sample-data))))

;; (type (:date sample-data))
;; (:date sample-data)

;; (difference 1 (:close-price sample-data))
;; (count (:close-price sample-data))
;; (setup-data (:close-price sample-data))

;; (create-seeds [2 3 5 7 11 13]) => [2 -1 -1 1 -3 9]

;; (= (take 10 (:close-price sample-data))
;;    (take 10 (undifference
;;              (create-seeds (take 1 (:close-price sample-data)))
;;              (* -1 (difference 1 (:close-price sample-data))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
