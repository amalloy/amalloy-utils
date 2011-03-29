(ns amalloy.utils.seq
  (:use amalloy.utils
        [clojure.walk :only [postwalk-replace]]))

(defmacro lazy-loop [bindings & body]
  (let [inner-fn 'lazy-recur
        names (take-nth 2 bindings)
        values (take-nth 2 (rest bindings))]
    `((fn ~inner-fn
        ~(vec names)
        (lazy-seq
         ~@body))
      ~@values)))

(defn unfold
  "Next and done? are functions that operate on a seed. next should
  return a pair, [value new-seed]; the value half of the pair is
  inserted into the resulting list, while the new-seed is used to
  continue unfolding. Notably, the value is never passed as an
  argument to either next or done?.

  If done? is omitted, the sequence will be unfolded forever."
  ([next seed]
     (unfold next (constantly false) seed))
  ([next done? seed]
     (lazy-loop [seed seed]
       (when-not (done? seed)
         (let [[value new-seed] (next seed)]
           (cons value
                 (lazy-recur new-seed)))))))
