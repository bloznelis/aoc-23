(ns aoc.day-04
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(defn matches->winnings [matches]
  (if (zero? matches) 0 (bit-shift-left 1 (dec matches))))

(defn count-matches [lines]
  (map (fn [line]
         (->> (str/split (second (str/split line #": ")) #"\s+\|\s+")
              (map #(into #{} (str/split % #"\s+")))
              (reduce s/intersection)
              count)) lines))

(defn winnings->cards [card-idx card-multiplier winnings]
  (->> (repeat card-multiplier)
       (map vector (range (inc card-idx) (+ 1 card-idx winnings)))
       (into {})))

(defn fill-cards-table [matches-list]
  (loop [[head & tail] matches-list
         idx 0
         cards-table (into {} (map-indexed (fn [idx _] [idx 1]) matches-list))]
    (if (nil? head)
      cards-table
      (recur tail (inc idx) (merge-with + cards-table (winnings->cards idx (cards-table idx) head))))))

(defn part-1 [input]
  (->> (str/split-lines input)
       count-matches
       (map matches->winnings)
       (reduce +)))

(defn part-2 [input]
  (->> (str/split-lines input)
       count-matches
       fill-cards-table
       vals
       (reduce +)))
