(ns aoc.day-03-test
  (:require [aoc.day-03 :as day3]
            [clojure.test :refer [is run-tests deftest]]))

(deftest read-matrix
  (is (= (day3/read-matrix "467..114..\n...*......\n..35..633.")
         [["4" "6" "7" "." "." "1" "1" "4" "." "."]
          ["." "." "." "*" "." "." "." "." "." "."]
          ["." "." "3" "5" "." "." "6" "3" "3" "."]])))

(deftest append-num
  (is (= (day3/append-num "123" "1") "1231"))
  (is (= (day3/append-num nil "1") "1"))
  (is (= (day3/append-num "123" "*") nil))
  (is (= (day3/append-num "11" "4") "114")))

(deftest find-row-nums
  (is (= (day3/find-row-nums ["4" "6" "7" "." "." "1" "1" "4" "." "."] 0) [["467" [[0 0] [1 0] [2 0]]] ["114" [[5 0] [6 0] [7 0]]]]))
  (is (= (day3/find-row-nums ["1"] 0) [["1" [[0 0]]]]))
  (is (= (day3/find-row-nums ["." "1"] 0) [["1" [[1 0]]]])))

(deftest find-all-nums
  (is (= (day3/find-all-nums [["4" "6" "7" "." "." "1" "1" "4" "." "."]
                              ["." "." "." "*" "." "." "." "." "." "."]
                              ["." "." "3" "5" "." "." "6" "3" "3" "."]]) [["467" [[0 0] [1 0] [2 0]]]
                                                                           ["114" [[5 0] [6 0] [7 0]]]
                                                                           ["35" [[2 2] [3 2]]]
                                                                           ["633" [[6 2] [7 2] [8 2]]]])))

(deftest get-at
  (is (= (day3/get-at [0 4] [[0 1 2]
                             [3 4 5]]) nil))
  (is (= (day3/get-at [1 1] [[0 1 2]
                             [3 4 5]]) 4))
  (is (= (day3/get-at [1 0] [[0 1 2]
                             [3 4 5]
                             [6 7 8]]) 1)))
(deftest valid-neighbors
  (is (= (day3/valid-neighbors [0 0] [[0 1 2]
                                      [3 4 5]]) [3 1 4]))
  (is (= (sort (day3/valid-neighbors [1 1] [[0 1 2]
                                            [3 4 5]
                                            [6 7 8]])) [0 1 2 3 5 6 7 8])))

(deftest final
  (def input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

  (is (= (day3/part-1 input) 4361))
  (is (= (day3/part-2 input) 467835)))

(run-tests 'aoc.day-03-test)
