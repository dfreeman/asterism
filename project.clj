(defproject asterism "1.0.0-SNAPSHOT"
  :description "A language prototyping framework"
  :url "https://github.com/dfreeman/asterism"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]
                   :plugins [[lein-midje "3.1.0"]]}}
  :main asterism.core)
