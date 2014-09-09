(ns hackerrank-string-compression.core)

(defn string-compression [s & 
                          {:keys [prev count ret] 
                           :or {prev "" count 1 ret ""}}]
  (cond
   (empty? s) 
     (do ;;this is a kludgy way to flush...how to improve the algo?
       (cond
        (= count 1) (str ret prev)
        :else (str ret prev count)))
   (= (first s) prev) (recur (rest s) {:prev (first s) 
                                       :count (+ count 1)
                                       :ret ret})
   :else ;first time we encounter a char
     (do
       (cond
        (= count 1)
          (recur (rest s) {:prev (first s)
                           :count 1
                           :ret (str ret prev)})
        (> count 1)
          (recur (rest s) {:prev (first s)
                           :count 1
                           :ret (str ret prev count)})))))
   
