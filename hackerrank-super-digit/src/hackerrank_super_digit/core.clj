(ns hackerrank-super-digit.core)

(defn make-num [n]
  (map (fn [x] (Integer/parseInt (str x))) n))

(defn super-digit [n]
  (cond
   (= 1 (count n)) (Integer/parseInt (str (first n)))
   :else
     (recur (make-num (str (reduce + n))))))

(def d (clojure.string/split (read-line) #" "))
(super-digit (make-num (str (* (reduce + (make-num (first d))) (Integer/parseInt (last d))))))
