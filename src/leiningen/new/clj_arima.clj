(ns leiningen.new.clj-arima
  (:require [leiningen.new.templates :refer [renderer name-to-path ->files]]
            [leiningen.core.main :as main]))

(def render (renderer "clj-arima"))

(defn clj-arima
  "FIXME: write documentation"
  [name]
  (let [data {:name name
              :sanitized (name-to-path name)}]
    (main/info "Generating fresh 'lein new' clj-arima project.")
    (->files data
             ["src/{{sanitized}}/foo.clj" (render "foo.clj" data)])))
