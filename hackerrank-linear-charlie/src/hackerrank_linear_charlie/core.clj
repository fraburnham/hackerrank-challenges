(ns hackerrank-linear-charlie.core)

(defn square [x]
  (* x x))

(defn squared-diff [x y]
  (square (- x y)))

(defn hypothesis [thetas features]
  (reduce + (map * thetas features)))

(defn costfn [hypo-ys actual-ys]
  (let [m (count hypo-ys)]
    (/ 1 (* 2 m) (reduce + (map squared-diff hypo-ys actual-ys)))))

(defn batch-gradient-descent [thetas alpha features hypo-ys actual-ys]
  (let [malpha (* alpha (/ 1 (count hypo-ys)))
        diffs (map #(- %1 %2) hypo-ys actual-ys)
        sums (reduce #(map + %1 %2) (map #(map (partial * %1) %2) diffs features))]
    (map (fn [tj sum] (- tj (* malpha sum))) thetas sums)))

(defn linear-regression [alpha training-inputs training-outputs]
  (loop [thetas
         (cons 1 (repeatedly (dec (count (first training-inputs))) (constantly 0)))
         cost 0]
    (let [hypo-ys (map (partial hypothesis thetas) training-inputs)
          new-thetas (batch-gradient-descent thetas alpha
                                             training-inputs hypo-ys training-outputs)
          new-cost (costfn hypo-ys training-outputs)]
      (if (or (> cost new-cost) ;this seems backward... old cost should be bigger than new cost...
              (= thetas new-thetas))
        thetas
        (recur new-thetas new-cost)))))


(let [[training-in training-out]
      (let [[features num-training] (clojure.string/split (read-line) #" ")]
        (loop [c (Integer/parseInt num-training)
               training-in '()
               training-out '()]
          (if (= 0 c) 
            [training-in training-out]
            (let [training-data (clojure.string/split (read-line) #" ")
                  in (cons 1 (map #(Float/parseFloat %) (take (Integer/parseInt features) training-data)))
                  out (Float/parseFloat (last training-data))]
              (recur (dec c) (cons in training-in) (cons out training-out))))))
      thetas (linear-regression 0.1 training-in training-out)]
  (loop [c (Integer/parseInt (read-line))]
    (if (= 0 c)
      nil
      (let [features (cons 1 (map #(Float/parseFloat %) (clojure.string/split (read-line) #" ")))]
        (println (hypothesis thetas features))
        (recur (dec c))))))
      
