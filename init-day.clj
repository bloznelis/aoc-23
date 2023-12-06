#!/usr/bin/bb
(def day (read-string (first *command-line-args*)))

(def src-file "(ns aoc.day-%02d
  (:require [clojure.string :as str]
            [aoc.utils :as utils]))

(defn part-1 [input] \"todo\")
(defn part-2 [input] \"todo\")

(comment
  (part-1 (slurp \"input/input-day-%02d.txt\"))
  (part-2 (slurp \"input/input-day-%02d.txt\"))
  ;;
)")

(def test-file "(ns aoc.day-%02d-test
  (:require [aoc.day-%02d :as day%d]
            [clojure.test :refer [is run-tests deftest]]))

(deftest solutions
  (is (= (day%d/part-1 \"todo\") \"todo\"))
  (is (= (day%d/part-2 \"todo\") \"todo\"))
  )

(run-tests)
")

(spit (format "./src/aoc/day_%02d.clj" day) (apply format src-file (repeat 3 day)))
(spit (format "./test/aoc/day_%02d_test.clj" day) (apply format test-file (repeat 5 day)))
(spit (format "./input/input-day-%02d.txt" day) "")
