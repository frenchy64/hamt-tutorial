(defproject com.ambrosebs.fast-attr "0.1.0-SNAPSHOT"
  :description "Exploration in Hash Array Mapped Tries."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.3"]]
  :profiles {:dev {:dependencies [[collection-check "0.1.6"]
                                  [criterium "0.4.4"]
                                  [rhizome "0.2.7"]
                                  [org.clojure/test.check "1.1.1"]]}}
  :java-source-paths ["java"])
