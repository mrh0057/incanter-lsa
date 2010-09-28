(ns #^{:doc "Contains math functions useful for LSA analysis."}
  incanter-lsa.stats)

(defn- numerator-cos [vec1 vec2]
  (loop [vec1 vec1
         vec2 vec2
         numer 0]
    (if (empty? vec1)
      numer
      (recur (rest vec1) (rest vec2) (+ numer (* (first vec1) (first vec2)))))))

(defn- squared [a]
  (* a a))

(defn- denumerator-cos [vec1 vec2]
  (loop [vec1 vec1
         vec2 vec2
         vec1-sum 0
         vec2-sum 0]
    (if (empty? vec1)
      (* (Math/sqrt vec1-sum) (Math/sqrt vec2-sum))
      (recur (rest vec1)
             (rest vec2)
             (+ vec1-sum (squared (first vec1)))
             (+ vec2-sum (squared (first vec2)))))))

(defn cos-similarity [vec1 vec2]
  (/ (numerator-cos vec1 vec2) (denumerator-cos vec1 vec2)))
