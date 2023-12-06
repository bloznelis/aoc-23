(ns aoc.day-05-test
  (:require [aoc.day-05 :as day5]
            [clojure.test :refer [is deftest run-tests]]))

(deftest range-diff
  (is (= (day5/subract-range [50 100] [20 50]) [[51 100]]))
  (is (= (day5/subract-range [50 100] [20 60]) [[61 100]]))
  (is (= (day5/subract-range [50 100] [60 70]) [[50 59] [71 100]]))
  (is (= (day5/subract-range [50 100] [60 100]) [[50 59]]))
  (is (= (day5/subract-range [50 100] [100 100]) [[50 99]]))
  (is (= (day5/subract-range [50 100] [1000 1000]) [[50 100]]))
  (is (= (day5/subract-range [50 100] [1 1]) [[50 100]]))
  (is (= (day5/subract-range [50 100] [100 102]) [[50 99]]))
  (is (= (day5/subract-range [50 100] [50 100]) [])))

(run-tests)
