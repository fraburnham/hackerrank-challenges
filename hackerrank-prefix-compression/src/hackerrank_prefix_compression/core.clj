(ns hackerrank-prefix-compression.core)

(let [stra (read-line)
      strb (read-line)
      prefix-len (reduce + (first (partition-by #(= 0 %)
                                         (map #(if (= %1 %2) 1 0) stra strb))))]
  (println prefix-len (apply str (take prefix-len stra)))
  (println (- (count stra) prefix-len) (apply str (drop prefix-len stra)))
  (println (- (count strb) prefix-len) (apply str (drop prefix-len strb))))
