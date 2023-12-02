(ns aoc.day-02
  (:require [clojure.string :as str]))

(defn parse-round [round-str]
  (->> (str/split round-str #", ")
       (into {} (comp
                 (map #(str/split % #" "))
                 (map reverse)
                 (map vec)))
       (#(update-vals % read-string))))

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

(defn game-power [{rounds :rounds}]
  (->> (apply merge-with max rounds)
       vals
       (reduce *)))

(defn part-1 [input]
  (->> (str/split-lines input)
       (transduce (comp
                   (map parse-game)
                   (filter game-valid?)
                   (map :id)) +)))

(defn part-2 [input]
  (transduce (comp (map parse-game) (map game-power)) + (str/split-lines input)))
