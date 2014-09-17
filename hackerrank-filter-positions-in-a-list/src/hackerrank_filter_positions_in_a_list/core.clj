(ns hackerrank-filter-positions-in-a-list.core)

(defn foo [lst]
  (loop [l lst
         ret []
         i 0]
    (cond 
     (empty? l) ret
     (odd? i) (recur (rest l) (conj ret (first l)) (+ i 1))
     :else (recur (rest l) ret (+ i 1)))))

;;an alternate solution to study
(defn bar [lst] 
  (map second 
       (filter 
        (fn [v] (if (odd? (first v)) true false)) 
        (map-indexed (fn [x y] [x y]) lst))))
