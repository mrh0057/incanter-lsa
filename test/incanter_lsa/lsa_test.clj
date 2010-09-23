(ns incanter-lsa.lsa-test
  (:use incanter-lsa.lsa
        incanter.core
        incanter.stats
        clojure.test))

(deftest create-word-map-test
  (let [docs (list (list "1" "2" "3") (list "3" "2" "4" "5"))
        word-map (create-word-map docs)]
    (is (= (count word-map) 5))))

(deftest create-document-matrix-test
  (let [docs (list (list "1" "2" "3") (list "3" "2" "4" "5" "5"))
        doc-matrix (create-document-matrix docs)]))

(deftest get-document-count-test
  (let [document (list "3" "2" "4" "5" "5")
        counts (get-document-count document)]
    (is (= (get counts "5") 2))
    (is (= (get counts "2") 1))
    (is (= (get counts "3") 1))))

(deftest lsa-test
  (let [docs (list (list "1" "2" "3") (list "3" "2" "4" "5" "5") (list "3" "2" "4" "5" "5"))
        matrix (lsa docs)]
    (println (jaccard-index [2 4 3 1 6]
			   [3 5 1 2 5]))))

(run-tests)

