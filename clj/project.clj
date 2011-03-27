;; The only requirement of the project.clj file is that it includes a
;; defproject form. It can have other code in it as well, including
;; loading other task definitions.

(defproject fmath "0.0.1"
  :description "Some sample math functions for FX options"
  :url "http://blog.ericwhite.ca"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :main fmath
  :dev-dependencies [[org.clojars.brandonw/lein-nailgun "1.0.1"]])
