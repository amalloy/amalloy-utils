(ns amalloy.utils.core
  (:use amalloy.utils
        (amalloy.utils [seq :only [lazy-loop]])))

(defmacro cond* [& clauses]
  (cons `cond
        (lazy-loop [[test & [expr & more :as all]] clauses]
          (when test
            (if (vector? test)
              (concat test
                      (lazy-recur all))
              (concat [test expr]
                      (lazy-recur more)))))))

(comment
  (cond*
   (zero? x) 1
   [(nil? x)
    10]
   :else 0)
  ==>
  (cond (zero? x) 1
        (nil? x) 10
        :else 0))
