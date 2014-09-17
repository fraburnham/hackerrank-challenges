(ns hackerrank-sum-of-odd-elements.core)

(defn foo [lst]
  (loop [l lst
         r 0]
    (cond
     (empty? l) r
     (odd? (first l)) (recur (rest l) (+ r (first l)))
     :else (recur (rest l) r))))
