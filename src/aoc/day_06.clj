(ns aoc.day-06
  (:require [clojure.string :as str]))

(defn win? [btn-hold-time race-time dist]
  (< dist (* btn-hold-time (- race-time btn-hold-time))))

(defn count-possible-wins [[time dist]]
  (loop [wins 0 btn-time 0]
    (if (< time btn-time)
      wins
      (recur (if (win? btn-time time dist) (inc wins) wins) (inc btn-time)))))

(defn part-1 [input]
  (->> (str/split-lines input)
       (map #(map read-string (re-seq #"\d+" %)))
       (apply mapv vector)
       (map count-possible-wins)
       (reduce *)))

(defn part-2 [input]
  (->> (str/split-lines input)
       (map (partial re-seq #"\d"))
       (map str/join)
       (map read-string)
       count-possible-wins))
