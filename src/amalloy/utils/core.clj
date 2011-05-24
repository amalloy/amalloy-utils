(ns amalloy.utils.core
  (:use amalloy.utils
        (amalloy.utils [seq :only [lazy-loop]])))

(defmacro cond+
  "Like cond, but allowing bracketed [test expr] pairs to be grouped
  together, as required in common lisp, for cases in which that
  improves readability."
  [& clauses]
  (cons `cond
        (lazy-loop [[test & [expr & more :as all]] clauses]
          (when test
            (if (vector? test)
              (concat test
                      (lazy-recur all))
              (concat [test expr]
                      (lazy-recur more)))))))

(defmacro let+
  "Like let, but allowing (binding expr) pairs to be grouped together,
  as required in common lisp, for cases in which that improves
  readability."
  [clauses & body]
  `(let ~(vec 
          (lazy-loop [[test & [expr & more :as all]] clauses]
            (when test
              (if (seq? test)
                (concat test
                        (lazy-recur all))
                (concat [test expr]
                        (lazy-recur more))))))
     ~@body))

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
