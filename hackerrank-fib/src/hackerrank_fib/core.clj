(ns hackerrank-fib.core)

;start with the first two values of the fib seq
;map adding them together and cat that onto fib
(def fib (lazy-cat [0 1N] (map + (rest fib) fib)))

(defn fib-mod-exp [n]
  (mod (nth fib n) 100000007))

(loop
    [lines  (Integer/parseInt (read-line))]
  (if (= 0 lines) nil
      (do
        (println (int (fib-mod-exp (Integer/parseInt (read-line)))))
        (recur (dec lines)))))
