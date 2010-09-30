(ns incanter-lsa.core-test
  (:use clojure.test
    incanter-lsa.core))

(deftest find-lowest-word-test
  (let [lowest (find-lowest-word (list (list "2") (list "1") (list "1")))]
    (is (= lowest "1"))))

(run-tests)

