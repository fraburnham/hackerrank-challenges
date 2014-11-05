(ns hackerrank-fib.core)

(defn fib
  ([n] (fib 0 1N n))
  ([a b n]
     (if (= n 0) a
         (recur b (+ a b) (dec n)))))

(defn fib-mod-exp [n]
  (mod (fib n) 100000007))

(loop
    [lines  (Integer/parseInt (read-line))]
  (if (= 0 lines) nil
      (do
        (println (int (fib-mod-exp (Integer/parseInt (read-line)))))
        (recur (dec lines)))))
