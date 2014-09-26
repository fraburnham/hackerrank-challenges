(ns hackerrank-string-mingling.core)

(def random (java.util.Random.))
;define characters list to use to generate string
(def chars 
   (map char (concat (range 48 58) (range 66 92) (range 97 123))))
;generates 1 random character
(defn random-char [] 
  (nth chars (.nextInt random (count chars))))
; generates random string of length characters
(defn random-string [length]
  (apply str (take length (repeatedly random-char))))

(def stra (random-string 100000))
(def strb (random-string 100000))
(println (reduce str (map str stra strb)))

;just gotta make it read two lines from std in and pass them
;it's too slow. try to make your own recursive?

(defn string-mingling [stra strb]
  (if (empty? stra) nil
      (do
        (print (first stra))
        (print (first strb))
        (recur (rest stra) (rest strb)))))

;as it turns out the map reduce was slower than just
;outputting it. Could have guessed that probably.
;two steps are slower than one.
