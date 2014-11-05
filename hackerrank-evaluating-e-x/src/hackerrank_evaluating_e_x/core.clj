(ns hackerrank-evaluating-e-x.core)

;;the below functions are not very clojure. they are pure
;;which is nice but they don't think in terms of transforming
;;lists so let's think about the list transformations and redo
;;some of this mess

;;this factorial is a bit quicker with smaller numbers
;;compared to the solution using map
(defn ! 
  ([n] (! n 1N))
  ([n ret]
     (if (= n 0) ret
         (recur (dec n) (* ret n)))))
;;(time (! 100000N))
;;time taken: "Elapsed time: 23860.643874 msecs"

;;bam
(defn ! [n]
  (reduce * (range 1N (inc n))))
;;(time (! 100000N))
;;time taken: "Elapsed time: 21521.075593 msecs"
;;it's almost too close to care about the speed
;;it is more clojure

(defn pow 
  ([n x] (pow n x 1))
  ([n x ret]
     (if (= x 0) ret
         (recur n (dec x) (* n ret)))))
;;(time (pow 35N 198370N))
;;"Elapsed time: 20416.585042 msecs"
;;fancy that a more sophisticated test needs to be
;;made up to profile these two functions the more verbose
;;single list method is obviously quicker

(defn pow [base exp]
  (reduce * (repeat exp base)))
;;(time (pow 35N 198370N))
;;"Elapsed time: 31361.801696 msecs"

;holy shit that needs an update let's try this for readability
(defmacro read-float []
  `(Float/parseFloat (read-line)))

(let [N (read-float)
      facs (map #(reduce * (range 1N (inc %))) (range 2 10))]
  (loop [count N]
    (if (< count 1) nil
        (let [x (read-float)
              pows (map #(reduce * (repeat %2 %1)) (repeat x) (range 2 10))]
          (println (float (+ 1 x (reduce + (map / pows facs)))))
          (recur (dec count))))))
 
;so with some cleanup the resulting code is more concise and I think
;easier to read. all you have to think about is how a few lists
;are going to behave instead of having to step line by line
;through the old example

;) Isn't lisp sexy ;)
