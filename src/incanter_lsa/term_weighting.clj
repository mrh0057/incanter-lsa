(ns #^{:doc "Has document term weight functions useful in make lsa analysis more accurate.

The focus of the way the functions work is try to reduce the amount of memory they uses."}
    incanter-lsa.term-weighting
    (:import org.apache.commons.math.util.MathUtils))

(defn- log2 [a]
  (MathUtils/log a 2))

(defn log-lij [tij]
  (log2 (+ tij 1)))

(defn entropy [tij term-probs]
  )

(defn log-eij [pij doc-count]
  )

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

(defn word-porbs-list [docs-sorted word]
  "Used to get the word probabilities from a document.

docs-sorted - The sorted documents
word - The current word.")

(defn calculate-word-entropy [docs-sorted]
  "Calculates the entropy for each word and returns a map of the entropy.

sorted-documents - The sorted documents.

returns - The entropy for all of the words in the documents."

  (let [word-count (calculate-word-counts docs-sorted)]
    (loop [docs-sorted docs-sorted
           entropy {}]
      (if (empty? docs-sorted)
        entropy))))

(defn probs [documents])
