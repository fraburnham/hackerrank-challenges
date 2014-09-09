(ns hackerrank-ape-war.core)

;;this looks like the way to do the tree
(def *simple-tree* '((1 ((2 ()) (3 ())))))

;;nodes 2 and 3 are on the same level
;;as are 4 5 6
;;7 8 9 are at the highest depth
(def *my-tree* '(1 ((2 ((4 ()) (5 ((7 ()) (8 ()) (9 ()))))) (3 ((6 ()))))))

;;a tree looks like
;;((n ((c1 ()) (c2 ()))))
;;a node looks like
;;(c1 ()) or (n ((c1 ()) (c2 ())))
;;start with this-node to get the current root node
;;value

(defmacro child-tree [node]
  `(first (rest ~node)))

(defmacro next-node [tree]
  `(first ~tree))

;;rest-nodes returns a tree of the remaining nodes
(defmacro rest-nodes [tree]
  `(rest ~tree))

(defmacro this-node [node]
  `(first ~node))

(defn tree? [tree]
  (let [v (this-node tree)]
    (cond
     (number? v) false
     :else true)))

;;this is the basic inner loop it won't work for
;;lots of other cases I wonder how I can generalize it
(defn inner-loop [c-t parent-fn]
  (cond
   (empty? c-t) nil
   :else
     (do
       (parent-fn (next-node c-t))
       (recur (rest-nodes c-t) parent-fn))))

;;print-nodes is a simple depth-first walk of a tree
(defn print-nodes [node]
  (println (this-node node))
  (inner-loop (child-tree node) print-nodes))

(defn print-path 
  ([node] (print-path node " "))
  ([node path]
     (println (str path " " (this-node node)))
     (loop [c-t (child-tree node)
            p (str path " " (this-node node))]
       (cond
        (empty? c-t) nil
        :else
          (do
            (print-path (next-node c-t) p)
            (recur (rest-nodes c-t) p))))))

;;so? we can get a list of two paths
;;find the first in common node and solve for the path from there
(defn first-common-element [list-a list-b]
  ;;for elem in list-a: for elemb in list-b: if elem == elemb
  (cond
    (empty? list-a) nil
    :else
    (do
     (let [elem-a (first list-a)]
       (cond
        (loop [b (rest list-b)
	       elem-b (first list-b)]
          (cond
	   (not (number? elem-b)) false
	   (= elem-b elem-a) true
	   :else
	     (recur (rest b) (first b)))) 
	elem-a ;;if the loop returns true instead of false
	:else
	 (recur (rest list-a) list-b))))))

;; a modified print-path
(defn find-path 
  ([node find] (find-path node find '()))
  ([node find path]
     (let [t-n (this-node node)]
       (cond
        (empty? node) nil
        (= t-n find) (concat path (list t-n))
        :else
        (do
          (loop [c-t (child-tree node)
                 p (concat path (list t-n))]
            (cond
             (empty? c-t) nil
             :else
              (do
                (let [ret (find-path (next-node c-t) find p)]
                  (cond
                   (not= nil ret) ret
                   :else (recur (rest-nodes c-t) p)))))))))))
  ;([node a b] (find-path node a b '()))
  ;;the code should be similar but we're looking for two paths
  ;;we can still do the joining logic after, but this should
  ;;save some time down the road, nvm this is complicated.
  ;;do it if we're too slow
  ;([node a b path]))

(defn path-reduce 
  ([a b] (path-reduce a b nil)) 
  ([a b last]
     (cond
      (= (first a) (first b)) (recur (rest a) (rest b) (first a))
      :else (list (conj a last) (conj b last)))))

;;takes two already reduced paths and joins them on the first element
(defmacro join-paths [a b]
  ;;reverse the rest of a and concat onto b
  `(concat (reverse (rest ~a)) ~b))


;;it would be faster to build the tree from the bottom, you wouldn't have
;;keep walking down the tree to add stuff
;;reverse the list of values and take the count +1 (offset for the boss ape)

;;stop worring about speed. make it work the slow way then find ways to
;;speed it up. You'll need (leaf-add node leaf) it'll walk the tree
;;find the node modify it and return the new tree

;;we'll also need to build a tree from a string we're given
;;default i is 2 (chief is 1)
;;in the string the current i is your id number
;;the current number is your superior
;;so we'll need functions like node-insert
;;for tree manipulation
(defn build-tree [str-description i])
