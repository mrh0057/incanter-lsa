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

(defn- create-sigma-matrix [S start dems]
  (let [s-matrix (matrix 0 dems dems)]
    (loop [i start
           S S]
      (if (>= i dems)
        s-matrix
        (do
          (. s-matrix setQuick i i (first S))
          (recur (inc i) (rest S)))))))

(defn- create-u-matrix [U start dems]
  "Used to create the u matrix with the reduction of the specifid demenisions.

U - The U matrix to use.
start - The starting point of the dems to remove.
dems - The dems to keep after the starting point.

returns U'"
  (sel U :cols (range start dems)))

(defn- create-v-matrix [V start dems]
  "Used to create the v' matrix with the reduction of the specified demenisions.

V - The V matrix to use.
start - The startin point of the dems to remove.
dems - The number of dems to keep after the starting point."
  (trans (sel V :cols (range start dems))))

(defn get-dems [S]
  "USed to create the sigma matrix.

S - The sigma list."
  (if (< (count S) 200)
    (let [dem (/ (count S) 2)]
      (list 0 dem))
    (list 2 100)))

(defn lsa [documents weight-function]
  "Returns the SVD matrix.

documents - The sorted document words.
weight-function - The function used to calculate the weights.
  parameters - param1 word-count, param2 word

The list documents and then a list of tokens."
  (let [doc-matrix (create-document-matrix documents weight-function)
        svd (decomp-svd doc-matrix)
        S (:S svd)
        dems (get-dems S)]
    (mmult (create-u-matrix (:U svd) (first dems) (second dems))
           (create-sigma-matrix S (first dems) (second dems))
           (create-v-matrix (:V svd) (first dems) (second dems)))))

(defn document-confusion-matrix [lsa-matrix stat-func]
  "Returns the confusion matrix using the stats function.

lsa-matrix - The lsa-matrix used to calculate the similarity.
stat-func - The stat function.

returns The confusion matrix.
"
  (loop [x 0
         y 0
         matrix (matrix (ncol lsa-matrix) (ncol lsa-matrix))]
    (if (>= x (ncol lsa-matrix))
      matrix
      (if (>= y (ncol lsa-matrix))
        (recur (inc x) 0 matrix)
        (do
          (. matrix setQuick x y (stat-func (sel lsa-matrix :cols x) (sel lsa-matrix :cols y)))
          (recur x (inc y) matrix))))))
