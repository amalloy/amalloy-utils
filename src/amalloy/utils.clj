(ns amalloy.utils
  (:use [clojure.contrib.def :only [defalias]]))

(defalias ! complement)

;; TODO why is this extra ' needed!?!
(defmacro defcomp [name doc args & fs]
  (let [fnmeta {:doc doc :arglists `'(~args)}]
    `(def ~(with-meta name fnmeta) (comp ~@fs))))

(defn iterate-until
  [pred f start]
  (take-while (! pred)
              (iterate f start)))

(defn trim-seq "Trim a sequence at the first nil element"
  [s]
  (take-while (! nil?) s))

(defn decorate
  "Return a function f such that (f x) => [x (f1 x) (f2 x) ...]."
  [& fs]
  (apply juxt identity fs))

(defn annotate
  "A vector of [x (f1 x) (f2 x) ...]."
  [x & fs]
  ((apply decorate fs) x))

(defmacro keywordize
  "Create a map in which, for each symbol S in vars, (keyword S) is a
  key mapping to the value of S in the current scope."
  [vars]
  (into {} (map (juxt keyword identity)
                vars)))

(defn verify
  "Return x, unless (pred x) is logical false, in which case return
nil."
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

(defn rand-in-range
  "Produce a random integer in the range [start, end)."
  ([] (rand-in-range 0 2))
  ([end] (rand-in-range 0 end))
  ([start end]
   (+ start (rand-int (- end start)))))

(defn update
  "Like update-in, but interpret keys as a set of top-level keys which
  should be modified by applying f to them, rather than a seq of keys
  indicating how to drill down into the target map. For example,
  (update {:a 1 :b 2} [:a :b] inc) yields {:a 2 :b 3}."
  [m [& keys] f & args]
  (reduce (fn [m k]
            (apply update-in m [k] f args))
          m keys))

(defalias foldl reduce)
(defn foldr [f start coll]
  (foldl #(f %2 %1) start (reverse coll)))

(comment
  (foldl + 0 [1 2 3 4]) => (+ (+ (+ (+ 0 1) 2) 3) 4)
  (foldr + 0 [1 2 3 4]) => (+ 1 (+ 2 (+ 3 (+ 4 0))))
)
