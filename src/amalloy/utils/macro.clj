(ns amalloy.utils.macro
  (:use clojure.contrib.macro-utils))

(defn- partition-params [argvec actual-args]
  (if (some #{'&} argvec)
    [actual-args] ; one seq with all args
    (partition (count argvec) actual-args)))

(defmacro anon-macro
  ([args macro-body & body]
     `(macrolet [(name# ~args ~macro-body)]
                (name# ~@body))))

(defmacro macro-do
  [macro-args body & args]
  `(anon-macro [arg#]
               (for [~macro-args arg#]
                 ~body) ; (cons 'do) already supplied by macrolet
               ~(partition-params macro-args args)))
