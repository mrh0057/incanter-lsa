(ns incanter-lsa.lsa
  (:use incanter.core
    incanter-lsa.core
    incanter-lsa.term-weighting))

(defn create-word-map [documents]
  "Creates a hashmap with the key being the work and value being the position."
  (loop [documents documents
         current-doc (first documents)
         word-map {}
         i 0]
    (if (empty? documents)
      word-map
      (if (empty? current-doc)
        (recur (rest documents) (first (rest documents)) word-map i)
        (if (contains? word-map (first current-doc))
          (recur documents
                 (rest current-doc)
                 word-map
                 i)
          (recur documents
                 (rest current-doc)
                 (assoc word-map (first current-doc) i)
                 (inc i)))))))

(defn- increment-count [counts word]
  "Used to incrment a word."
  (if (contains? counts word)
    (assoc counts word (inc (get counts word)))
    (assoc counts word 1)))

(defn get-document-count [document]
  "Returns the count for the document."
  (loop [document document
         counts {}]
    (if (empty? document)
      counts
      (recur (rest document) (increment-count counts (first document))))))

(defn set-values [matrix col word word-map word-counts weight-function]
  (. matrix setQuick (get word-map word) col (weight-function (get word-counts word) word)))

(defn create-document-matrix [documents weight-function]
  (let [word-map (create-word-map documents)
        doc-matrix (matrix 0 (count word-map) (count documents))]
    (loop [documents documents
           word-counts (get-document-count (first documents))
           word (keys word-counts)
           col 0]
      (println word-counts)
      (if (empty? documents)
        doc-matrix
        (if (empty? word)
          (let [word-counts (get-document-count (first (rest documents)))]
              (recur (rest documents)
                     word-counts
                     (keys word-counts)
                     (inc col)))
          (do
            (set-values doc-matrix col (first word) word-map word-counts weight-function)
            (recur documents
                   word-counts
                   (rest word)
                   col)))))))

(defn create-sigma [S]
  (let [s-matrix (matrix 0 (count S) (count S))]
    (loop [i 0
           S S]
      (if (> i 3)
        s-matrix
        (do
          (. s-matrix setQuick i i (first S))
          (recur (inc i) (rest S)))))))

(defn lsa [documents weight-function]
  "Returns the SVD matrix.

documents - The sorted document words.
weight-function - The function used to calculate the weights.
  parameters - param1 word-count, param2 word

The list documents and then a list of tokens."
  (let [doc-matrix (create-document-matrix documents weight-function)
        svd (decomp-svd doc-matrix)
        S (:S svd)]
    (mmult (sel (:U svd) :cols (range (count S))) (create-sigma S) (:V svd))))
