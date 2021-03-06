(ns hackerrank-ape-war.core)

(defmacro child-tree [node]
  `(first (rest ~node)))

(defmacro next-node [tree]
  `(first ~tree))

;;rest-nodes returns a tree of the remaining nodes
(defmacro rest-nodes [tree]
  `(rest ~tree))

(defmacro this-node [node]
  `(first ~node))

(defmacro make-node [value]
  `'(~value ()))

;;takes two already reduced paths and joins them on the first element
(defmacro join-paths [a b]
  ;;reverse the rest of a and concat onto b
  `(concat (reverse (rest ~a)) ~b))

(defn tree? [tree]
  (let [v (this-node tree)]
    (cond
     (number? v) false
     :else true)))

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

(defn path-reduce 
  ([a b] (path-reduce a b nil)) 
  ([a b last]
     (cond
      (= (first a) (first b)) (recur (rest a) (rest b) (first a))
      :else (list (conj a last) (conj b last)))))

(defn append-node
  "Append node to root-node"
  [root-node node]
  (let [t-n (this-node root-node)]
    (cons t-n (list (cons node (first (rest root-node)))))))

(defn build-tree [str-description i root-node]
  ;;start by making the current node
  ;;find the parent node
  (let [(make-node (first i))
        parent (first str-description)
        ret-tree '()] ;;dunno what to default ret-tree
    ;;search for the parent
    ;;once we find it modify the child nodes and return the whole thing
))



;;---------------
(def *simple-tree* '((1 ((2 ()) (3 ())))))
(def *my-node* '(1 ((2 ((4 ()) (5 ((7 ()) (8 ()) (9 ()))))) (3 ((6 ()))))))

;;a tree looks like
;;((n ((c1 ()) (c2 ()))))
;;a node looks like
;;(c1 ()) or (n ((c1 ()) (c2 ())))
;;start with this-node to get the current root node
;;value

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

(defn get-node [node find]
  (let [t-n (this-node node)]
    (cond
     (empty? node) nil
     (= t-n find) node
     :else
     (do
       (loop [c-t (child-tree node)]
         (cond
          (empty? c-t) nil
          :else
          (do
            (let [ret (find-node (next-node c-t) find)]
              (cond
               (not= nil ret) ret
               :else (recur (rest-nodes c-t)))))))))))

;;so don't stop working on this. understanding trees is super critical
;;but maybe zippers can be used?

(require '[clojure.zip :as zip])

