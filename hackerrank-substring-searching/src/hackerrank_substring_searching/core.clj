(ns hackerrank-substring-searching.core)

;forcing types is supposed to improve execution time
;I'm slow on the last two test cases, let's see if this works as a fix
;still timeout on the last two test cases sadface
(defn substring? [sourcestr findstr]
  (loop [sstr (seq sourcestr)
         fstr (seq findstr)
         matchloc (int 1)
         match (boolean false)]
    (if (or (empty? sstr) (empty? fstr))
      (if (empty? fstr) match false)
      (if (= (first sstr) (first fstr))
        (recur (rest sstr) (rest fstr) matchloc (boolean true))
        (if match
          (recur (drop matchloc sourcestr) findstr
                 (+ 1 matchloc) (boolean false))
          (recur (rest sstr) fstr 
                 (inc matchloc) (boolean false)))))))
       

(loop [testcount (Integer/parseInt (read-line))]
  (let [sourcestr (read-line)
        findstr (read-line)]
    (if (= 0 testcount) 
      nil
      (do
        (if (substring? sourcestr findstr) (println "YES") (println "NO"))
        (recur (dec testcount))))))
