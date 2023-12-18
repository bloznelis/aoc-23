(ns aoc.day-18
  (:require [clojure.string :as str]
            [aoc.utils :as utils]))

(defn parse-1 [line]
  (-> #"(R|L|D|U)\s+([0-9][0-9]?)"
      (re-find line)
      rest
      vec
      (update 1 read-string)))

(defn parse-2 [line]
  (let [[_ hex] (re-find #"\(\#(.+)\)" line)
        dist (Integer/parseInt (str/join (drop-last hex)) 16)
        dir (Integer/parseInt (str/join (drop 5 hex)) 16)
        dir (case dir
              0 "R"
              1 "D"
              2 "L"
              3 "U")]

    [dir dist]))

(defn move [[direction steps] [y x]]
  (case direction
    "U" [(- y steps) x]
    "D" [(+ y steps) x]
    "R" [y (+ x steps)]
    "L" [y (- x steps)]))

(defn move-all [instructions]
  (loop [[instruction & tail] instructions
         acc [[0 0]]
         perimeter-acc 0]
    (if (nil? instruction)
      [acc perimeter-acc]
      (recur tail (conj acc (move instruction (peek acc))) (+ perimeter-acc (second instruction))))))

(defn det [[[y1 x1]
            [y2 x2]]]
  (- (* y1 x2) (* x1 y2)))

(defn solve [input parse-fn]
  (let [[trench-coords perimeter] (move-all (map parse-fn (str/split-lines input)))
        area (abs (/ (->> trench-coords
                          (partition 2 1)
                          (map (comp det utils/transpose))
                          (reduce +)) 2))
        boundary (inc (/ perimeter 2))] ;; why +1 tho? https://en.wikipedia.org/wiki/Pick%27s_theorem
    (+ area boundary)))

(defn part-1 [input] (solve input parse-1))
(defn part-2 [input] (solve input parse-2))

(comment
  (time (part-1 (slurp "input/input-day-18.txt"))) ;; 95356
  (time (part-2 (slurp "input/input-day-18.txt"))) ;; 92291468914147
  )
