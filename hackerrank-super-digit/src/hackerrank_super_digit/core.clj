(ns hackerrank-super-digit.core)
;;super digit for 9875
;; 9+8+7+5=29
;; 2+9=11
;; 1+1 =2
;; 2

;;each step 
(defn super-digit [n] 
  (if (< n 10) n
      (let [x (reduce + (map #(bigint (str %)) (seq (str n))))]
        (recur x))))

(let [[n k] (map bigint (clojure.string/split (read-line) #" "))
      p (bigint (apply str (repeat k n)))]
  (println (int (super-digit p))))
;;not fast enough!
