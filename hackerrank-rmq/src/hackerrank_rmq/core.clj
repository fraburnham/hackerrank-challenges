(ns hackerrank-rmq.core)
(require '[clojure.string :as s])

(defn parse-int-seq [s]
  (map (fn [x] (Integer/parseInt x)) s))

(defn subseq [s start end]
  (take (+ 1 (- end start)) (drop start s)))

(loop [m (Integer/parseInt (last (s/split (read-line) #" ")))
       s (parse-int-seq (s/split (read-line) #" "))]
  (if (= 0 m) nil
      (do
        (let [pis (parse-int-seq (s/split (read-line) #" "))]
          (println (apply min (subseq s (first pis) (last pis))))
          (recur (dec m) s)))))
;still too slow how to make it faster?
;inlining didn't help much if any

;a lazy sequence would be faster?
