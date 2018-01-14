(ns clj-arima.test.adf
  (:require ;;[clj-arima.data.quandl :refer [sample-data]]
            [clj-arima.util.difference :as diff]
            [clojure.core.matrix.linear :as linear]
            [clojure.core.matrix.stats :as stats])
  (:use [clojure.core.matrix]
        ;;[clojure.core.matrix.operators]
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

(defn- stationary-adftest-elem
  [x lag]
  (let [a (stat-adf-array x lag)
        dyt (subvec (diff/difference 1 x) 0 (+ lag 3))]
    (linear/solve a dyt)))

(def Rejection-value
  {:trend
   {:5%
    {:25 -3.60
     :50 -3.50
     :100 -3.45
     :250 -3.43
     :500 -3.42
     :over -3.41}
    :1%
    {:25 -4.38
     :50 -4.15
     :100 -4.04
     :250 -3.99
     :500 -3.98
     :over -3.96}
    :non-trend
    {:5%
     {:25 -3.00
      :50 -2.93
      :100 -2.89
      :250 -2.88
      :500 -2.87
      :over -2.86}
     :1%
     {:25 -3.75
      :50 -3.58
      :100 -3.51
      :250 -3.46
      :500 -3.44
      :over -3.43}}}})

(defn stationary-adftest
  "pmax is (* 12 (pow (/ data-len 100) 0.25))
  https://faculty.washington.edu/ezivot/econ584/notes/unitrootLecture2.pdf
  restrict : true => 1%
           : false=> 5%
  "
  [data lag restrict]
  (let [data-len (count data)
        adf      (if (< lag (* 12 (pow (/ data-len 100) 0.25)))
                   (for [i (range (- data-len (+ lag lag 3)))]
                     (first (stationary-adftest-elem
                             (subvec data i (+ i lag lag 3)) lag)))
                   nil)
        rejection (get-in Rejection-value
                          [:trend
                           (if restrict :1% :5%)
                           (condp >= data-len
                             25 :25
                             50 :50
                             100 :100
                             250 :250
                             500 :500
                             :over)])
        adf-value (/ (stats/mean adf) (stats/sd adf))]
    (if adf
      {:adf adf-value :reject? (> rejection adf-value)}
      {:adf "ERROR"})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (stationary-adftest (:close-price sample-data) 9)
;; 2.2866587359315345E-15
;; (def a (stationary-adftest (:close-price sample-data) 11 true))
;; (:reject? a) => true
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
