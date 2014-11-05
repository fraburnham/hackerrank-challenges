(ns hackerrank-service-lane.core)

(defmacro subseq [x y s]
  `(drop ~x (take (inc ~y) ~s)))

(let [T (Integer/parseInt (last (clojure.string/split (read-line) #" ")))
      lane (map #(Integer/parseInt %)
                (clojure.string/split (read-line) #" "))]
  (loop [n T]
    (if (= 0 n) nil
        (let [[i j] (map #(Integer/parseInt %)
                         (clojure.string/split (read-line) #" "))]
          (println (apply min (subseq i j lane)))
          (recur (dec n))))))
