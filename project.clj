(defproject text-processor "0.1.0-SNAPSHOT"
  :description "Takes all files in the specified input directory and prints them in justified columns to the specified output file."
  :url "https://github.com/jasonracey/text-processor"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.5"]]
  :main ^:skip-aot text-processor.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
