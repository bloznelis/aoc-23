(ns aoc.day-01
  (:require [clojure.string :as s]))

(def fix-map {"one" "1", "two" "2", "three" "3", "four" "4", "five" "5", "six" "6", "seven" "7", "eight" "8", "nine" "9"})

(defn fix [str]
  (reduce (fn [acc [key value]] (s/replace acc key (str value key))) str (vec fix-map)))

(defn fix-input-line [line]
  (loop [[head & tail] (s/split line #"")
         acc ""]
    (if (nil? head)
      acc
      (recur tail (fix (str acc head))))))

(defn solve [input fix-fn]
  (->> (s/split-lines input)
       (map (fn [line]
              (->>
               (s/split (fix-fn line) #"")
               (map read-string)
               (filter number?)
               ((fn [col] (read-string (str (first col) (last col))))))))
       (apply +)))

(defn part-1 [input] (solve input identity))
(defn part-2 [input] (solve input fix-input-line))

