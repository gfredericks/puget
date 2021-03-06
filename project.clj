(defproject mvxcvi/puget "0.7.0-SNAPSHOT"
  :description "Colorizing canonical Clojure printer for EDN values."
  :url "https://github.com/greglook/puget"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]

  :aliases {"docs" ["do" ["hiera"] ["marg" "--dir" "target/docs"]]
            "tests" ["do" ["check"] ["test"] ["cloverage"]]}

  :dependencies
  [[org.clojure/clojure "1.6.0"]
   [org.clojure/data.codec "0.1.0"]
   [fipp "0.5.1"]]

  :profiles
  {:coverage
   {:plugins
    [[lein-cloverage "1.0.2"]]}})
