(ns aoc.day-13
  (:require [clojure.string :as str]
            [aoc.utils :as utils]))

(defn reflection? [[col-a col-b]]
  (= col-a col-b))

(defn check-at [rows at]
  (let [[start end] (split-at at rows)
        [start end] [(reverse start) end]]

    (every? true? (map reflection? (map vector start end)))))

(defn lines-above-reflection [rows]
  (let [possible-reflection-points (->> rows
                                        (partition 2 1)
                                        (map-indexed (fn [idx elem] (if (reflection? elem) (inc idx) nil)))
                                        (filter identity))]

    (or (->> possible-reflection-points
             (filter (partial check-at rows))
             seq) :no-reflection)))

(defn rate [rows]
  (let [horizontal (lines-above-reflection rows)
        vertical (lines-above-reflection (utils/transpose rows))]
    (if (= horizontal :no-reflection)
      (if (= vertical :no-reflection)
        nil
        vertical)
      (map (partial * 100) horizontal))))

(defn rate2 [rows]
  (let [horizontal (lines-above-reflection rows)
        horizontal (if (keyword? horizontal) nil (map (partial * 100) horizontal))
        vertical (lines-above-reflection (utils/transpose rows))
        vertical (if (keyword? vertical) nil vertical)]

    [horizontal vertical]))

(defn parse [input]
  (mapv (fn [block] (mapv #(str/split % #"") (str/split-lines block))) (str/split input #"\n\n")))

(defn possible-fixes [matrix]
  (for [x (range (count (first matrix)))
        y (range (count matrix))]
    (update-in matrix [y x] (fn [old] (case old
                                        "#" "."
                                        "." "#")))))
(defn possible-solutions [matrix]
  (let [original-solution (flatten (rate2 matrix))
        solutions (into #{} (flatten (mapcat rate2 (possible-fixes matrix))))
        disjoined-solutions (apply disj solutions original-solution)]

    (or (first disjoined-solutions) (first (filter some? original-solution)))))

(defn part-1 [input]
  (->> input
       parse
       (map rate)
       (reduce +)))

(defn part-2 [input]
  (->> input
       parse
       (map possible-solutions)
       (reduce +)))
