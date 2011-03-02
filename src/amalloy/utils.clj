(ns amalloy.utils)

(def ^{:arglists (:arglists (meta #'complement))}
  ! complement)

(defn trim-seq "Trim a sequence at the first falsey element"
  [s]
  (take-while identity s))

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

(defmacro on-thread
  "Run the body in an anonymous, new thread. Very much like
  clojure.core/future, but with Java's default error-handling
  semantics instead of those of (future)."
  [& body]
  `(.start (Thread. (fn [] ~@body))))