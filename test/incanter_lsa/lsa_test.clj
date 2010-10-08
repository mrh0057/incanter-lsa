(ns incanter-lsa.lsa-test
  (:use incanter-lsa.lsa
        incanter-lsa.term-weighting
        incanter.core
        incanter-lsa.stats
        clojure.test))

(deftest create-word-map-test
  (let [docs (list (list "1" "2" "3") (list "3" "2" "4" "5"))
        word-map (create-word-map docs)]
    (is (= (count word-map) 5))))

(deftest create-document-matrix-test
  (let [docs (list (list "1" "2" "3") (list "3" "2" "4" "5" "5"))
        doc-matrix (create-document-matrix docs (fn [word-count word]
                                                  word-count))]))

(deftest get-document-count-test
  (let [document (list "3" "2" "4" "5" "5")
        counts (get-document-count document)]
    (is (= (get counts "5") 2))
    (is (= (get counts "2") 1))
    (is (= (get counts "3") 1))))

(deftest lsa-test
  (let [docs (list (list "1" "2" "3" "10" "25" "50") (list "1" "2" "3" "5" "9" "13") (list "1" "3" "6" "9") (list "1" "3" "6" "9") (list "1" "4" "8" "16") (list "4" "10" "20" "40" "45"))
        g-weights (calculate-word-g docs)
        matrix (lsa docs (fn [word-count word]
                           (word-weight word-count (get g-weights word))))]
    (println matrix)
    (println (document-confusion-matrix matrix cos-similarity))))

(run-tests)

