(ns clj-arima.arima.ma
  (:require [clojure.core.matrix.linear :as linear]
            [clojure.core.matrix.stats :as stats]
            [clj-arima.util :as util])
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]
        [clojure.core.matrix.dataset]))

(set-current-implementation :vectorz)

(defn random-walk [y]
  (let [e (util/normal-dist 1)
        y (+ y e)]
    (cons y (lazy-seq (random-walk y)))))

(defn ma [eseq coefs sigma]
  (let [e (util/normal-dist 1 :sd sigma)
        y (apply + e (map * eseq coefs))]
    (cons y (lazy-seq
             (ma (cons e eseq) coefs sigma)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (clj-arima.view/view  (clj-arima.view/tschart (take 50 (random-walk 0))))
;; (clj-arima.view/view
;;  (clj-arima.view/tschart
;;   (take 100
;;         (ma (util/normal-dist 5) [0 0 0 0 1] 0.5))))

;; (clj-arima.view/view
;;  (clj-arima.view/barchart
;;   (take 15
;;         (util/acf
;;          (take 5000
;;                (ma (util/normal-dist 5) [0 0 0 0 1] 0.2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
