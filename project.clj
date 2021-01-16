(defproject aoc "0.1.0-SNAPSHOT"
  :description "Advent of Code, in clojure"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [net.mikera/core.matrix "0.62.0"]
                 [hashp "0.2.0"]
                 [philoskim/debux "0.7.7"]
                 ]
  :main ^:skip-aot aoc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
