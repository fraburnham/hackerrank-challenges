(ns hackerrank-rotate-string.core)


(defn rotate [s]
  (let [d (seq s)]
    (apply str (conj (vec (rest d)) (first d)))))

(loop [test-cases (Integer/parseInt (read-line))]
  (if (= test-cases 0) nil
      (do
        (loop [case (read-line)
               len (count case)]
          (if (= len 0) (println)
              (let [s (rotate case)]
                (printf "%s " s)
                (recur s (dec len)))))
        (recur (dec test-cases)))))
