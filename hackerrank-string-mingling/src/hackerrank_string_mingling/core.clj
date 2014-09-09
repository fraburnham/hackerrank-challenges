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
(println (reduce str (pmap str stra strb)))
