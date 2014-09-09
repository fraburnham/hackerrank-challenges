(ns HackerRank-update-list.core)

;;return a list with absolute values of what you were given

;;you'll need cleanup later
(defn cleanup [s])

(defn abs [x]
  (cond
    (neg? x) (- x)
    :else x))

(defn update-list [l ret]
  (cond
    (empty? l) ret
    :else
      (recur (rest l) (concat ret (list (abs (first l)))))))