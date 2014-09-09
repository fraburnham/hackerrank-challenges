(ns hackerrank-pascals-triangle.core)

(defn ! [n & {:keys  [r] :or {r (bigint 1)}}]
  (cond
   (= n 0) r
   :else
   (recur (dec n) {:r (* r n)})))

;;The value at nthrow and rth column of the triangle is equal to 
;;n! / (r! * (n-r)!) where indexing start from 0

;;And, you need to accomplish this without directly defining any local variables.
;;For example, var and val have been blocked in Scala; def and defn are blocked in Clojure.
;;
;;Fucking constraints. It makes the code less readable. looping for factorials? dumb
;;nested loops in functional languages? not the right way
;;something involving map will be the right way probably. Sigh.

(defn expand [x]
  (cons x (lazy-seq (expand (dec x)))))

;;another way to get the factorial
(defn ! [n]
  (reduce * (take n ((fn exp [x] (cons x (lazy-seq (exp (dec x))))) n))))

;;once we've returned a row of pascals triangle the below will add spaces to the seq
(map (fn [x] (str x " ")) '(1 2 3))

;;ok so we want to build a seq that has (n r (- n r)) so we can factorial over
;;each value we have to build a seq of seqs one for each n = row r = column
;;fuck this indian guy for not realizing r = row. IDIOCY


;;NICE! this appears to be working and can be used as an anonymous
;;need to turn K into n and r seqs so that we can generate a number for each n r pair
(defn make-row-step-one [K] 
  (let [n (- K 1)]
    ;;now we have to calculate the r for each position up to n
    (loop [x n
           y n
           ret '()]
      ;;build the seq to return of ((n1 r1) (n1 r2) (etc))
      (cond
       (< y 0) ret
       :else
       (recur x (dec y) (cons (list x y) ret))))))

;;now that we have n and r we need to use those two values in a map
;;to generate the (n r (- n r) that we want
(defn make-row-step-two [[n r]]
  (list n r (- n r)))

;;now we factorial
(defn make-row-step-three [[x y z]]
  (map (fn [n]
         (reduce * 
                 (take n 
                       ((fn exp [x] (cons x (lazy-seq (exp (dec x))))) 
                        n))))
       (list x y z)))

;; x / (y * z)
(defn make-row-step-four [[x y z]]
  (/ x (* y z)))

;;the math is done. apply the args to an anony to print them
(defn print-row [& args]
  (loop [s (first args)]
    (cond
     (empty? s) nil
     :else
     (do
       (print (str (first s) " "))
       (recur (rest s))))))

(defn pascals-triangle [K]
  (loop [x 1
         K K]
    (cond 
     (> x K) nil
     :else
     (do
       (print-row
        (map make-row-step-four
             (map make-row-step-three
                  (map make-row-step-two
                       (make-row-step-one x)))))
       (println "")
       (recur (inc x) K)))))

;;link it all together nice and anonymous
((fn [K]
   (loop [x 1
          k K]
     (cond
      (> x k) nil
      :else
      (do
        ((fn [& args] ;print-row
           (loop [s (first args)]
             (cond
              (empty? s) nil
              :else
              (do
                (print (str (first s) " "))
                (recur (rest s)))))) 
         (map (fn [[x y z]] (/ x (* y z))) ;make-row-step-four
              (map (fn [[x y z]] 
                     (map (fn [n]
                            (reduce * 
                                    (take n 
                                          ((fn exp [x] (cons x (lazy-seq (exp (dec x))))) 
                                           n))))
                          (list x y z)))
                   (map (fn [[n r]] (list n r (- n r))) ;make-row-step-two
                        ((fn [K] ;make-row-step-one
                           (let [n (- K 1)]
                             (loop [x n
                                    y n
                                    ret '()]
                               (cond
                                (< y 0) ret
                                :else
                                (recur x (dec y) (cons (list x y) ret)))))) 
                         x)))))
        (println "")
        (recur (inc x) k)))))
 (Integer/parseInt (read-line)))
