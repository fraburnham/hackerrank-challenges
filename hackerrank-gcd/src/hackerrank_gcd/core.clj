(ns hackerrank-gcd.core)

;;euclid GCD
(defn gcd [x y]
  (cond
   (= x y) x
   (> x y) (recur (- x y) y)
   (< x y) (recur (- y x) x)))
