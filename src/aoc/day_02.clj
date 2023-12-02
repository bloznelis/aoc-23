(ns aoc.day-02
  (:require [clojure.string :as str]))

(defn parse-round [round-str]
  (update-vals
   (->> (str/split round-str #", ")
        (map #(str/split % #" "))
        (map reverse)
        (map vec)
        (into {})) read-string))

(defn parse-game [line]
  (let [[game rounds] (str/split line #": ")
        [_ id] (str/split game #" ")
        rounds (map parse-round (str/split rounds #"; "))]
    {:id (read-string id), :rounds rounds}))

(defn round-valid? [round]
  (let [check (fn [color limit] (<= (round color 0) limit))]
    (and (check "red" 12) (check "green" 13) (check "blue" 14))))

(defn game-valid? [{rounds :rounds}]
  (every? round-valid? rounds))

(defn merge-rounds [round-1 round-2]
  (loop [acc round-1
         [[key value] & tail] round-2]
    (if (nil? key)
      acc
      (recur (if (< (acc key 0) value) (conj acc {key value}) acc) tail))))

(defn game-power [{rounds :rounds}]
  (->> (reduce merge-rounds rounds)
       vals
       (reduce *)))

(defn part-1 [input]
  (->> (str/split-lines input)
       (map parse-game)
       (filter game-valid?)
       (map :id)
       (reduce +)))

(defn part-2 [input]
  (->> (str/split-lines input)
       (map parse-game)
       (map game-power)
       (reduce +)))
