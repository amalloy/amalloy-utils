(ns amalloy.utils)

(def ^{:arglists (:arglists (meta #'complement))}
  ! complement)

(defn trim-seq "Trim a sequence at the first nil element"
  [s]
  (take-while (complement nil?) s))

(defn iterations
  "Return a sequence of (f start), (f (f start))...until nil is
  encountered. Like clojure.core/iterate, but doesn't include the
  original element and doesn't go on forever."
  [f start]
  (trim-seq (rest (iterate f start))))

(defn decorate
  "Return a f such that (f x) => [x (f1 x) (f2 x) ...]."
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
