(defproject clj-game-of-life "0.1.0-SNAPSHOT"
  :description "This is a simple implementation of Conway's Game of Life."
  :url "http://example.com/FIXME"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot clj-game-of-life.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
