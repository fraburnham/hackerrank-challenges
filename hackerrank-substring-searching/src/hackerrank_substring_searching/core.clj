(ns hackerrank-substring-searching.core)

;substring search
;if the current char matches the first search char
;then (rest) both of them and recur and set match to true
;(seems like having to pass a value forward is an example of when
;we could HOF a different more clojurey solution)
;if they don't match set match to false
;if (nil? search-string-seq) match

(defn substring? [sourcestr findstr]
  (loop [sstr sourcestr
         fstr findstr
         match false]
    (if (or (empty? sstr) (empty? fstr)) match
        (if (= (first sstr)
               (first fstr))
          (recur (rest sstr) (rest fstr) true)
          (if match
            (recur (rest sstr) findstr false)
            (recur (rest sstr) fstr false))))))
       

(loop [testcount (Integer/parseInt (read-line))]
  (let [sourcestr (read-line)
        findstr (read-line)]
    (if (= 0 testcount) 
      nil
      (do
        (if (substring? sourcestr findstr) (println "YES") (println "NO"))
        (recur (dec testcount))))))
      
