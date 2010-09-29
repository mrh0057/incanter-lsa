(ns incanter-lsa.term-weighting-test
  (:use clojure.test
        incanter-lsa.term-weighting))

(deftest log-lij-test
  (= 2 (log-lij 1)))

(deftest find-lowest-word-test
  (let [lowest (find-lowest-word (list (list "2") (list "1") (list "1")))]
    (is (= lowest "1"))))

(deftest word-count-list-test
  (let [word-count (word-count-list "1" (list "1" "1" "1" "2" "2"))]
    (is (= 3 (first word-count)))
    (is (= (list "2" "2") (second word-count)))))

(deftest calculate-word-counts-test
  (let [word-counts (calculate-word-counts (map sort (list (list "1" "4" "3" "5" "2")
                                                           (list "2" "5" "4" "5")
                                                           (list "6" "5" "2" "10"))))]
    (is (= 1 (get word-counts "1")))
    (is (= 3 (get word-counts "2")))
    (is (= 1 (get word-counts "3")))
    (is (= 2 (get word-counts "4")))
    (is (= 4 (get word-counts "5")))))

(deftest total-word-count-test
  (let [word-counts-results (total-word-count
                             "1"
                             (list (list "1" "1" "2") (list "2" "6" "8") (list "1"))
                             '())
        documents (second word-counts-results)]
    (is (= 3 (first word-counts-results)))
    (is ( = documents (list (list "2" "6" "8") (list "2"))))))

(deftest word-count-doc-test
  (let [word-count (word-count-doc (list "1" "1" "2" "3") "1")]
    (is (= (first word-count) 2))
    (is (= (list "2" "3") (second word-count)))))

(run-tests)
