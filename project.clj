(defproject savagemett/yadic "SNAPSHOT"
  :description "A Clojure DI container"

  :url "http://github.com/savagematt/yadic-clj"

  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.6.0"]]

  :java-source-paths ["src"]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]

                   :plugins      [[lein-set-version "0.4.1"]
                                  [lein-midje "3.1.0"]]}})
