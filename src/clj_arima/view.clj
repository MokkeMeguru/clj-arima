(ns clj-arima.view
  (:require [com.hypirion.clj-xchart :as chart]
            ;;[clj-arima.data.quandl :refer [sample-data]]
            ))

(defn tschart [seq & {:keys [name] :or {name "tsdata"}}]
  (let [seq-len (count seq)]
    (chart/xy-chart
     {name
      {:x (range seq-len)
       :y seq}}
     {:title name
      :x-axis {:title "Time"}
      :y-axis {:title "Value"}
      :theme :xchart})))

(defn barchart [seq & {:keys [name] :or {name "A"}}]
  (let [seq-len (count seq)]
    (chart/category-chart*
     {name
      {:x (range seq-len)
       :y seq}}
     {:theme :xchart})))

(defn func-plot [func a b & {:keys [name] :or {name "function-plot"}}]
  (if (> a b)
    (func-plot b a)
    (let [line (range a b (/ (- b a) 200))
          yseq (map func line)]
      (chart/xy-chart
       {name
        {:x line
         :y yseq}}
       {:title name
        :theme :xchart}))))


(defn view
  [chart]
  (chart/view chart))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def sample-chart
;;   (chart/xy-chart
;;    {"NIKKEI/INDEX"
;;     {:x (map clj-time.coerce/to-date (:date sample-data))
;;      :y (:close-price sample-data)}}
;;    {:title "NIKKEI/INDEX"
;;     :x-axis {:title "date"}
;;     :y-axis {:title "value"}
;;     :theme :xchart}))
;; (defn test
;;   (chart/view sample-chart))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
