(ns hackerrank-string-mingling.core)

;;woah broah (interleave) is a builtin
(println (apply str (interleave (seq (read-line)) (seq (read-line)))))
;;2.78s longest HR.com test case
