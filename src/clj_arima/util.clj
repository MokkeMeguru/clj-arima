(ns clj-arima.util
  (:require [clojure.core.matrix.stats :as stats]
            [clojure.core.matrix.linear :as linear])
  (:import (cern.jet.random.tdouble Normal))
  (:use [clojure.core.matrix]
        ;; [clojure.core.matrix.operators]
        ;; [clojure.core.matrix.dataset]
        ))

(set-current-implementation :vectorz)

(defn normal-dist
  [^Integer n & {:keys [mean sd] :or {mean 0 sd 1}}]
  (if (= n 1)
    (Normal/staticNextDouble mean sd)
    (for [_ (range n)] (Normal/staticNextDouble mean sd))))

(defn- acf* [seq var n k]
  (let [lag (->> (drop k seq)
                 (map * seq)
                 esum)]
    (cons (/ lag n var)
          (lazy-seq (acf* seq var n (inc k))))))

(defn acf [seq]
  "autocorrelation"
  (acf* (- seq (stats/mean seq))
        (stats/variance seq)
        (dec (count seq)) 0))

(defn- acv* [seq n k]
  (let [lag (->> (drop k seq)
                 (map * seq)
                 esum)]
    (cons (/ lag n)
          (lazy-seq
           (acv* seq n (inc k))))))

(defn- acv [seq]
  (acv* (- seq (stats/mean seq))
        (count seq) 0))

(defn- pac* [pacs sigma prev next]
  (let [acv (first next)
        sum (esum (mmul pacs (reverse prev)))
        pac (/ (- acv sum) sigma)]
    (cons pac
          (lazy-seq
           (pac* (->> (mmul pacs pac)
                      (reverse)
                      (- pacs)
                      (cons pac))
                 (* (- 1 (pow pac 2)) sigma)
                 (cons acv prev)
                 (rest next))))))

(defn pacf [seq]
  (let [acvs (acv seq)
        acv1 (first acvs)
        acv2 (second acvs)
        pac (/ acv2 acv1)]
    (concat [1.0 pac]
            (pac* (vector pac)
                  (- acv1 (* pac acv2))
                  (vector acv2)
                  (drop 2 acvs)))))

(defn acf-pacf-line
  "5% threshold"
  [seq]
  (/ 2 (sqrt (count seq))))
