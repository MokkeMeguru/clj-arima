(defproject clj-arima "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-time "0.14.2"]
                 [net.mikera/core.matrix "0.61.0"]
                 [net.mikera/vectorz-clj "0.47.0"]
                 [expresso "0.2.2"]
                 ;; [com.hypirion/clj-xchart "0.2.0"]
                 [net.sourceforge.parallelcolt/parallelcolt "0.10.1"]
                 ;; will remove
                 [incanter/incanter-core "1.5.7"]
                 ;; test
                 [clj-quandl-api "0.2.1"]
                 ;; to use apache-commons-math3
                 [org.apache.commons/commons-math3 "3.6.1"]])
