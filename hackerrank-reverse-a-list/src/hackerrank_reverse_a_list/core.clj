(ns hackerrank-reverse-a-list.core)

(defn foo [lst]
  (loop [l lst
         r '()]
    (cond 
     (empty? l) r
     :else (recur (rest l) (conj r (first l))))))
