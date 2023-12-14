(ns aoc.day-14
  (:require [clojure.string :as str]))

(def B 1000000000)

(defn zip-with-index [col] (map-indexed #(vector %1 %2) col))
(defn parse [input] (mapv vec (str/split-lines input)))

(defn roll-row [y coords-fn matrix]
  (reduce-kv (fn [acc x cell]
            (if (and (= cell \O) (= \. (get-in acc (coords-fn [y x]) nil)))
              (-> acc
                  (assoc-in [y x] \.)
                  (assoc-in (coords-fn [y x]) \O))
              acc)) matrix (matrix y)))

(defn roll [roll-fn matrix]
  (loop [previous matrix]
    (let [rolled (reduce-kv (fn [acc y _] (roll-fn y acc)) previous matrix)]
      (if (= previous rolled)
        rolled
        (recur rolled)))))

(defn roll-row-north [y matrix] (roll-row y (fn [[y x]] [(dec y) x]) matrix))
(defn roll-row-west [y matrix] (roll-row y (fn [[y x]] [y (dec x)]) matrix))
(defn roll-row-south [y matrix] (roll-row y (fn [[y x]] [(inc y) x]) matrix))
(defn roll-row-east [y matrix] (roll-row y (fn [[y x]] [y (inc x)]) matrix))

(defn roll-north [matrix] (roll roll-row-north matrix))
(defn roll-west [matrix] (roll roll-row-west matrix))
(defn roll-south [matrix] (roll roll-row-south matrix))
(defn roll-east [matrix] (roll roll-row-east matrix))

(defn roll-cycle [matrix]
  (-> matrix roll-north roll-west roll-south roll-east))

(defn billion-cycles [matrix]
  (loop [previous matrix
         cnt 0
         results {matrix cnt, cnt matrix}]
    (let [new-cycle (roll-cycle previous)]
      (if-let [same-result-at (results new-cycle)]
        (results (dec (+ same-result-at (mod (- B same-result-at) (- cnt same-result-at)))))
        (recur new-cycle (inc cnt) (conj results [new-cycle cnt] [cnt new-cycle]))))))

(defn score-rocks [matrix]
  (->> matrix
       (map vector (reverse (range 1 (inc (count matrix)))))
       (map (fn [[mul row]] (* mul (count (filter #(= % \O) row)))))
       (reduce +)))

(defn part-1 [input] (-> input parse roll-north score-rocks))
(defn part-2 [input] (-> input parse billion-cycles score-rocks))

(comment
  (time (part-1 (slurp "input/input-day-14.txt"))) ;; 109385
  (time (part-2 (slurp "input/input-day-14.txt"))) ;; 93102
  )



