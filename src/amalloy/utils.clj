(ns amalloy.utils
  (:use [clojure.contrib.def :only [defalias]]))

(defalias ! complement)

;; TODO why is this extra ' needed!?!
(defmacro defcomp [name doc args & fs]
  (let [fnmeta {:doc doc :arglists `'(~args)}]
    `(def ~(with-meta name fnmeta) (comp ~@fs))))

(defn iterate-until
  [pred f start]
  (take-while (complement pred)
              (iterate f start)))

(defn trim-seq "Trim a sequence at the first nil element"
  [s]
  (take-while (complement nil?) s))

(defcomp iterations
  "Return a sequence of (f start), (f (f start))...until nil is
  encountered. Like clojure.core/iterate, but doesn't include the
  original element and doesn't go on forever."
  [f start]
  trim-seq rest iterate)

(defn decorate
  "Return a function f such that (f x) => [x (f1 x) (f2 x) ...]."
  [& fs]
  (apply juxt identity fs))

(defn annotate
  "A vector of [x (f1 x) (f2 x)]."
  [x & fs]
  ((apply decorate fs) x))

(defmacro keywordize
  "Create a map in which, for each symbol S in vars, (keyword S) is a
  key mapping to the value of S in the current scope."
  [vars]
  (into {} (map (juxt keyword identity)
                vars)))

(defn verify
  "Return x, unless (pred x) is logical false, in which case return nil."
  [pred x]
  (when (pred x)
    x))

(defn validator
  [pred]
  (partial verify pred))

(defn invoke
  "Like clojure.core/apply, but doesn't expand/splice the last
argument."
  ([f] (f))
  ([f x] (f x))
  ([f x & more] (apply f x more)))
