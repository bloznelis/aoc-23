(ns aoc.day-11
  (:require [clojure.string :as str]
            [aoc.utils :as utils]))

(defn find-galaxy-coords [matrix]
  (for [x (range (count (first matrix)))
        y (range (count matrix))
        :when (= "#" (get-in matrix [y x]))]
    [x y]))

(defn super-manhatan [[x1 y1] [x2 y2] {x-voids :x-voids y-voids :y-voids} multiplier]
  (let [add-fn #(->> (range (min %1 %2) (max %1 %2))
                     (filter (partial contains? %3))
                     count
                     (* multiplier))]

    (+ (abs (- x1 x2)) (abs (- y1 y2)) (add-fn x1 x2 x-voids) (add-fn y1 y2 y-voids))))

(defn sum-manhatans [coords voids mul]
  (loop [[head & tail] coords
         acc 0]
    (if (nil? head)
      acc
      (recur tail (+ acc (reduce #(+ %1 (super-manhatan head %2 voids mul)) 0 tail))))))

(defn find-voids [coords]
  (let [void-fn (fn [coords] (into #{} (comp (map-indexed #(vector %1 %2))
                                             (filter (fn [[_ elem]] (every? #(= % ".") elem)))
                                             (map first)) coords))
        y-voids (void-fn coords)
        x-voids (void-fn (utils/transpose coords))]
    {:y-voids y-voids, :x-voids x-voids}))

(defn solve [input void-multiplier]
  (let [matrix (->> input
                    str/split-lines
                    (mapv #(str/split % #"")))]

    (sum-manhatans (find-galaxy-coords matrix) (find-voids matrix) void-multiplier)))

(defn part-1 [input]
  (solve input 1))

(defn part-2 [input]
  (solve input 999999))
