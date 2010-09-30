(ns #^{:doc "Has document term weight functions useful in make lsa analysis more accurate.

The main focus of the functions is to reduce the amount of memory used."}
    incanter-lsa.term-weighting
  (:use incanter.core
    incanter-lsa.core)
    (:import org.apache.commons.math.util.MathUtils))

(defn log-lij [tij]
  (log2 (+ tij 1)))

(defn entropy [sum-log-eij num-of-docs]
  "Used to calculate the entropy."
  (/ sum-log-eij (log2 num-of-docs)))

(defn g-log [sum-log-eij num-of-docs]
  (- 1 (entropy sum-log-eij num-of-docs)))

(defn doc-prob [doc-count all-docs-count]
  "Calculates the probability of the word in a document."
  (/ doc-count all-docs-count))

(defn word-weight [tij g-value]
  "
tij - Is the word frequency.
g-value - The g-value.
Returns the word rate."
  (* (log-lij tij) g-value))

(defn log-eij [doc-count all-docs-count]
  (let [prob (doc-prob doc-count all-docs-count)]
    (if (= prob 0)
      0
      (* prob (log2 prob)))))

(defn word-count-list [word document]
  "Returns the list and word count for the document"
  (loop [document document
         counts 0]
    (if (or (not= (first document) word) (empty? document))
      (list counts document)
      (recur (rest document) (inc counts)))))

(defn- conj-not-nil [col col2]
  "Used to conj two collections as long as they aren't null
col - the collection to join with col2
col2 - the collection not to join if nil

returns

if col2 nil returns col
if col not nil return the conj col col2"
  (if (empty? col2)
    col
    (conj col col2)))

(defn total-word-count [word documents docs-count]
  "Finds the total word count for a word in the documents.

word - The current word to find.
documents - The sorted documents.

returns a list.
first - word count
second - new list of documents."
  (loop [documents documents
         new-docs '()
         word-count 0]
    (if (empty? documents)
      (list word-count new-docs)
      (if (empty? (first documents))
        (recur (rest documents)
               (conj new-docs nil)
               word-count)
        (let [word-count-results (word-count-list word (first documents))]
          (recur (rest documents)
                 (conj-not-nil new-docs (second word-count-results))
                 (+ (first word-count-results) word-count)))))))

(defn calculate-word-counts [documents]
  "Calculates the word counts for the documents

documents - The documents where all of the words are sorted.

returns - The word count of the words."
  (loop [docs-sorted documents
         word-counts {}]
    (if (empty? docs-sorted)
      word-counts
      (let [word (find-lowest-word docs-sorted)
            word-counts-results (total-word-count word docs-sorted '())]
        (recur (second word-counts-results)
               (assoc word-counts word (first word-counts-results)))))))

(defn word-count-doc [document word]
  "Used to get the word count in a document.

document - the document to get the word count from.
word - the word to count.

returns A list:
 first is the count
 second is the new document word list."
  (loop [document document
         num 0]
    (let [current-word (first document)]
      (if (not= current-word word)
        (list num document)
        (recur (rest document) (inc num))))))

(defn word-g [docs-sorted word word-counts doc-count]
  "Used to get the word g values from the documents.

docs-sorted - The sorted documents
word - The current word.

returns A list:
 first is the entropy for the word
 second is the new doc list"
  (loop [docs-sorted docs-sorted
         new-docs ()
         current-prob 0]
    (if (empty? docs-sorted)
      (list (g-log current-prob doc-count) new-docs)
      (let [word-count (word-count-doc (first docs-sorted) word)]
        (recur 
          (rest docs-sorted) 
          (conj-not-nil new-docs (second word-count)) 
          (+ current-prob (log-eij (first word-count) word-counts)))))))

(defn calculate-word-g [docs-sorted]
  "Calculates the g value for each word and returns a map of the g.

sorted-documents - The sorted documents.

returns - 
The g value for all of the words in the documents.
   key - word
   value - the entropy
"
  (let [word-count (calculate-word-counts docs-sorted)
        doc-count (count docs-sorted)]
    (loop [docs-sorted docs-sorted
           entropy {}
           word (find-lowest-word docs-sorted)]
      (if (empty? docs-sorted)
        entropy
        (let [word-entro (word-g docs-sorted word (get word-count word) doc-count)
              docs-sorted (second word-entro)]
          (recur docs-sorted 
            (assoc entropy word (first word-entro))
            (find-lowest-word docs-sorted)))))))

(defn probs [documents])
