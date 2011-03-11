(ns amalloy.utils.macro
  (:use clojure.contrib.macro-utils))

(defmacro anon-macro
  ([args macro-body & body]
     `(macrolet [(name# ~args ~macro-body)]
                (name# ~@body))))