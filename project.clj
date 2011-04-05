(defproject amalloy/utils "0.3.6"
  :description "A collection of handy-dandy tools I've collected as I notice repeated patterns in my Clojure code"
  :dependencies [[clojure "[1.2.0,)"]
                 [clojure-contrib "[1.2.0,)"]]
  :dev-dependencies [[org.clojars.gjahad/swank-clojure "1.3.1.1-SNAPSHOT"]
                     [clojure-source "1.2.1"]
                     [clj-stacktrace "0.2.0"]]
  :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"])
