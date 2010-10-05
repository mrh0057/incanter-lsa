(ns #^{:doc "Core functions to calculate the documents."} 
      incanter-lsa.core)

(defn find-lowest-word [sorted-documents]
  "Finds the lowest word in a list of words."
  (loop [sorted-docs (rest sorted-documents)
         word (first (first sorted-documents))]
    (if (empty? sorted-docs)
      word
      (let [curr-word (first (first sorted-docs))]
        (if ( < (. curr-word compareTo word) 0)
          (recur (rest sorted-docs) curr-word)
          (recur (rest sorted-docs) word))))))
