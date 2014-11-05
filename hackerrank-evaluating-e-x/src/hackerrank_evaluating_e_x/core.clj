(ns hackerrank-evaluating-e-x.core)

;;e^x is given by
;;1 + x + x^2/2! + x^3/3! + x^n/n!
;;in this problem we will only use the first 10 terms

;;this factorial is a bit quicker with smaller numbers
;;compared to the solution using map
(defn ! 
  ([n] (! n 1))
  ([n ret]
     (if (= n 0) ret
         (recur (dec n) (* ret n)))))

(defn pow 
  ([n x] (pow n x 1))
  ([n x ret]
     (if (= x 0) ret
         (recur n (dec x) (* n ret)))))

(defn e-expansion 
  ([x] (e-expansion x 2 (+ 1.0 x)))
  ([x n ret]
     (if (= 10 n) (println ret)
         (recur x (inc n) (+ ret
                             (/ (pow x n) (! n)))))))

;;now we need a loop to read the number of inputs and run the expansions
(defn get-inputs
  ([] (get-inputs (Integer/parseInt (read-line))))
  ([num-lines]
     ;;call e-expansion once for each line of input
     (e-expansion (Float/parseFloat (read-line)))
     (if (= 0 num-lines) nil
         (recur (dec num-lines)))))

;) after that...make it anonymous and submit ;)
((fn get-inputs [num-lines]
   (if (= 0 num-lines) nil
       (do
         ((fn e-expansion 
            ([x] (e-expansion x 2 (+ 1.0 x)))
            ([x n ret]
               (if (= 10 n) (println ret)
                   (recur x (inc n) (+ ret
                                       (/ 
                                        ((fn pow 
                                           ([n x] (pow n x 1))
                                           ([n x ret]
                                              (if (= x 0) ret
                                                  (recur n (dec x) (* n ret)))))
                                         x n) 
                                        ((fn ! 
                                           ([n] (! n 1))
                                           ([n ret]
                                              (if (= n 0) ret
                                                  (recur (dec n) (* ret n))))) 
                                         n)))))))
          (Float/parseFloat (read-line)))
         (recur (dec num-lines)))))
       (Integer/parseInt (read-line)))
