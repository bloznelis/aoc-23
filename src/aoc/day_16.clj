(ns aoc.day-16
  (:require [clojure.string :as str]
            [clojure.set :as setops]))

(defn parse [input]
  (mapv vec (str/split-lines input)))

(defn top [[y x]] [(dec y) x])
(defn bot [[y x]] [(inc y) x])
(defn right [[y x]] [y (inc x)])
(defn left [[y x]] [y (dec x)])

(defn next-pos [pos direction]
  ((case direction
     :top top
     :bot bot
     :right right
     :left left) pos))

(defn right-mirror [direction]
  (case direction
    :top :right
    :bot :left
    :right :top
    :left :bot))

(defn left-mirror [direction]
  (case direction
    :top :left
    :bot :right
    :right :bot
    :left :top))

(def energized-set (atom #{}))
(defn update-energized [elem]
  (swap! energized-set conj elem))

(declare collect-energized-mem)

(defn collect-energized [matrix start-pos start-dir]
  (loop [pos start-pos
         direction start-dir]
    (if-let [cell (get-in matrix pos nil)]
      (if (contains? @energized-set [pos direction])
        @energized-set
        (do (update-energized [pos direction])
            (case cell
              \. (recur (next-pos pos direction) direction)
              \/ (recur (next-pos pos (right-mirror direction)) (right-mirror direction))
              \\ (recur (next-pos pos (left-mirror direction)) (left-mirror direction))
              \|
              (cond
                (or (= direction :left) (= direction :right))
                (setops/union (collect-energized-mem matrix (top pos) :top) (collect-energized-mem matrix (bot pos) :bot))
                :else (recur (next-pos pos direction) direction))
              \-
              (cond
                (or (= direction :top) (= direction :bot))
                (setops/union (collect-energized-mem matrix (right pos) :right) (collect-energized-mem matrix (left pos) :left))
                :else (recur (next-pos pos direction) direction)))))

      @energized-set)))

(def DP (atom {}))
(defn collect-energized-mem [matrix start-pos start-dir]
  (if-let [cached (@DP [matrix start-pos start-dir])]
    cached
    (let [answer (collect-energized matrix start-pos start-dir)]
      (swap! DP assoc [matrix start-pos start-dir] answer)
      answer)))

(defn solve [matrix [start-pos start-dir]]
  (reset! energized-set #{})
  (reset! DP {})
  (let [energized (collect-energized matrix start-pos start-dir)
        unique-energized (distinct (map first energized))]
    (count unique-energized)))

(defn prepare-possible-starts [matrix]
  (let [max-x (count (first matrix))
        max-y (count matrix)
        all-x (range 0 max-x)
        all-y (range 0 max-y)
        left (mapv #(vector [% 0] :right) all-y)
        right (mapv #(vector [% (dec max-x)] :left) all-y)
        top (mapv #(vector [0 %] :bot) all-x)
        bot (mapv #(vector [(dec max-y) %] :top) all-x)]

    (concat left top right bot)))

(defn part-1 [input]
  (solve (parse input) [[0 0] :right]))

(defn part-2 [input]
  (let [matrix (parse input)
        starts (prepare-possible-starts matrix)]

    (apply max (map (partial solve matrix) starts))))
