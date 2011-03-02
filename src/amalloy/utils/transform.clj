(ns amalloy.utils.transform)

(defn transform-if
  "Returns a function that tests pred against its argument. If the result
is true, return (f arg); otherwise, return (f-not arg) (defaults to
identity)."
  ([pred f]
     (fn [x]
       (if (pred x) (f x) x)))
  ([pred f f-not]
     (fn [x]
       (if (pred x) (f x) (f-not x)))))

;; coerce objects to various types
(def make-str (transform-if keyword? name str))
(def make-kw keyword)
(def make-int (transform-if string? #(Integer/parseInt %)))
