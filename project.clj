(defproject com.gfredericks.forks.mvxcvi/puget "0.8.1"
  :description "Fork of mvxcvi/puget, colorizing canonical Clojure printer for EDN values."
  :url "https://github.com/greglook/puget"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-repositories [["releases" :clojars]]

  :plugins [[lein-cloverage "1.0.2"]]

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [fipp "0.5.2"]]

  :cljfmt {:indents {with-options [[:block 1]]}}

  :codox {:defaults {:doc/format :markdown}
          :output-dir "doc/api"
          :src-dir-uri "https://github.com/greglook/puget/blob/master/"
          :src-linenum-anchor-prefix "L"}

  :hiera {:path "doc/ns-hiera.png"
          :cluster-depth 1})
