(ns aoc.day-09
  (:require [clojure.string :as str]))

(defn sub-seqs [seq]
  (loop [acc '() seq' seq]
    (if (every? zero? seq')
      (conj acc seq')
      (recur (conj acc seq') (map (fn [[x y]] (- y x)) (partition 2 1 seq'))))))

(defn extrapolate [seqs] (reduce (fn [acc elem] (+ acc (last elem))) 0 seqs))
(defn extrapolate-back [seqs] (reduce (fn [acc elem] (- (first elem) acc)) 0 seqs))

(defn solve [input extrapolate-fn]
  (->> (str/split-lines input)
       (map #(map read-string (str/split % #" ")))
       (map (comp extrapolate-fn sub-seqs))
       (reduce +)))

(defn part-1 [input] (solve input extrapolate))
(defn part-2 [input] (solve input extrapolate-back))
