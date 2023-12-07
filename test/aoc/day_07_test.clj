(ns aoc.day-07-test
  (:require [aoc.day-07 :as day7]
            [clojure.test :refer [is run-tests deftest]]))

(deftest solutions
  (is (= (day7/part-1 (slurp "input/input-day-07.txt")) 248750699)))

(deftest detect-hand-type
  (is (= (day7/detect-hand-type [:1 :1 :1 :1 :1]) :five-of-kind))
  (is (= (day7/detect-hand-type [:2 :1 :1 :1 :1]) :four-of-kind))
  (is (= (day7/detect-hand-type [:1 :1 :1 :1 :2]) :four-of-kind))
  (is (= (day7/detect-hand-type [:1 :1 :2 :1 :1]) :four-of-kind))
  (is (= (day7/detect-hand-type [:1 :1 :2 :1 :2]) :full-house))
  (is (= (day7/detect-hand-type [:1 :1 :1 :6 :2]) :three-of-kind))
  (is (= (day7/detect-hand-type [:1 :1 :2 :2 :3]) :two-pair))
  (is (= (day7/detect-hand-type [:1 :3 :2 :2 :1]) :two-pair))
  (is (= (day7/detect-hand-type [:1 :1 :T :T :A]) :two-pair))
  (is (= (day7/detect-hand-type [:1 :1 :2 :3 :4]) :one-pair))
  (is (= (day7/detect-hand-type [:A :A :2 :3 :4]) :one-pair))
  (is (= (day7/detect-hand-type [:3 :Q :Q :T :8]) :one-pair))
  (is (= (day7/detect-hand-type [:1 :2 :3 :4 :5]) :high-card))
  (is (= (day7/detect-hand-type [:A :A :A :7 :K]) :three-of-kind))
  (is (= (day7/detect-hand-type [:A :A :J :7 :K]) :three-of-kind))

  (is (= (day7/detect-hand-type [:K :T :J :J :T]) :four-of-kind))
  (is (= (day7/detect-hand-type [:1 :2 :3 :J :J]) :three-of-kind))
  (is (= (day7/detect-hand-type [:J :3 :3 :4 :K]) :three-of-kind))

  (is (= (day7/detect-hand-type [:J :3 :A :9 :K]) :one-pair))
  (is (= (day7/detect-hand-type [:Q :2 :Q :Q :J]) :four-of-kind))

  (is (= (day7/detect-hand-type [:8 :8 :8 :J :8]) :five-of-kind))
  (is (= (day7/detect-hand-type [:Q :Q :3 :J :3]) :full-house))
  (is (= (day7/detect-hand-type [:A :4 :J :5 :7]) :one-pair))

  (is (= (day7/detect-hand-type [:J :J :J :3 :4]) :four-of-kind))
  (is (= (day7/detect-hand-type [:J :J :J :J :4]) :five-of-kind))
  (is (= (day7/detect-hand-type [:J :J :J :4 :4]) :five-of-kind))
  (is (= (day7/detect-hand-type [:J :4 :4 :4 :4]) :five-of-kind))
  (is (= (day7/detect-hand-type [:4 :4 :4 :J :J]) :five-of-kind))

  (is (= (day7/detect-hand-type [:J :1 :1 :2 :3]) :three-of-kind))
  (is (= (day7/detect-hand-type [:J :J :1 :2 :3]) :three-of-kind))

  (is (= (day7/detect-hand-type [:J :6 :9 :A :A]) :three-of-kind))
  (is (= (day7/detect-hand-type [:Q :Q :Q :J :A]) :four-of-kind)))

(run-tests)
