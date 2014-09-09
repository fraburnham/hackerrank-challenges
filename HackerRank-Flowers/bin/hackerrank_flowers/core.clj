(ns hackerrank-flowers.core)

;;given a number of flowers to buy (n)
;;a number of buyers (k)
;;and a list of multipliers (c[n])
;;current flower (i)
;;when flower price = ((i-1) + 1)*c[i]

;;the logic:
;;divide n by k
;;each person buying as few flowers as possible
;;should guarantee the best price given c[i] < c[i+1]
;;which is implied by the problem.

(defn price-one [i c]
  (* (+ i 1) c))

(defn price 
"Return the price our buyer will pay for n flowers using
   the list of multipliers c. p = i = 0"
  [n c i p]
  (cond
    (= n 0) p
    :else (recur (- n 1) (rest c) (+ 1 i) (+ p (price-one i (first c))))))

;;p should be set as 0 initially
(defn flowers [n k c p]
  (let [x (/ n k)]
    (price 
  ;;divide n by k to get x
  ;;buy x flowers for each k
  ;;if this is the last buyer then buy the remaining flowers
  ;;n - (k*x) = remaining flowers
  ;;return the sum of prices 
  