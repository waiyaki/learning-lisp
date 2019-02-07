(defproject social "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.stuartsierra/component "0.4.0"]
                 [org.clojure/core.async "0.4.490"]]
  :main ^:skip-aot social.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
