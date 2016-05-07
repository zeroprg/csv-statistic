(defproject csv-statistic "0.1.0-SNAPSHOT"
  :description "CSV FILE REDUCING EXAMPLES IN CLOJURE"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
  [org.clojure/clojure "1.8.0"]
  [org.clojure/data.csv "0.1.3"]
  ]
  :main ^:skip-aot csv-statistic.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
  :jvm-opts["-Xmx6g"]