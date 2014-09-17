(ns hackerrank-list-replication.core)

(defn dup [n lst]
  (flatten (map (fn [x] (repeat 10 x)) lst)))
