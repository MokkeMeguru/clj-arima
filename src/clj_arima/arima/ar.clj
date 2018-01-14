(ns clj-arima.arima.ar
  (:require [clojure.core.matrix.linear :as linear]
            [clojure.core.matrix.stats :as stats]
            [clj-arima.util :as util]))

(defn ar [seq coefs sigma]
  (let [e (util/normal-dist 1 :sd sigma)
        y (apply + e (map * seq coefs))]
    (cons y (lazy-seq
             (ar (cons y seq) coefs sigma)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (clj-arima.view/view (clj-arima.view/tschart (take 100 (ar [1] [2] 0))))
;; 
;; (clj-arima.view/view
;;  (clj-arima.view/tschart (take 30 (ar (util/normal-dist 5) [0 0 0 0 1] 0))))
;;
;; (clj-arima.view/view
;;  (clj-arima.view/barchart
;;   (take 20
;;         (util/acf (take 100
;;                         (ar (util/normal-dist 5) [0 0 0 0 1] 0.2))))
;;   :name "ACF"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
