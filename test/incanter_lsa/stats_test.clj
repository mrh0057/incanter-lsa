(ns incanter-lsa.stats-test
  (:use clojure.test
        incanter-lsa.stats))

(deftest cos-similarity-test
    (let [value (cos-similarity [2 4 3 1 6] [3 5 1 2 5])]
      (= value 0.938572618717)))

(run-tests)
