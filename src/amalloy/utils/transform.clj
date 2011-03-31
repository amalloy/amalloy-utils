(ns amalloy.utils.transform
  (:require [amalloy.utils :as core]))

(defn transform-if
  "Returns a function that tests pred against its argument. If the result
is true, return (f arg); otherwise, return (f-not arg) (defaults to
identity)."
  ([pred f]
     (fn [x]
       (if (pred x) (f x) x)))
  ([pred f f-not]
     (fn [x]
       ((if (pred x) f f-not) x))))

;; coerce objects to various types
(def make-str (transform-if #(instance? clojure.lang.Named %) name str))
(def make-kw keyword)
(def make-int (transform-if string? #(Integer/parseInt %)))

(defn key-comparator
  "Given a transformation function (and optionally a direction), return a comparator which does its work by comparing the values of (transform x) and (transform y)."
  ([modifier]
     (fn [a b]
       (- (modifier a) (modifier b))))
  ([direction modifier]
     (let [f (comparator modifier)]
       (condp core/invoke direction
         #{:desc :descending -} (comp - f)
         #{:asc :ascending +} f))))


(defmacro with-adjustments
  "Create new bindings for binding args, by applying adjustment
  function to current values of bindings."
  [adjustment bindings & body]
  (let [bindings (vec bindings)]
    `(let [~bindings (map ~adjustment ~bindings)]
       ~@body)))
