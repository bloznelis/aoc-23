(ns aoc.day-08
  (:require [clojure.string :as str]))

(defn read-direction [line]
  (let [[key left right] (re-seq #"[A-Z0-9][A-Z0-9][A-Z0-9]" line)]
    {key {:left left :right right}}))

(defn parse [input]
  (let [[directions mapping] (str/split input #"\n\n")
        mapping (reduce (fn [acc line] (conj acc (read-direction line))) {} (str/split-lines mapping))
        directions (map (fn [itm] (case itm
                                    "L" :left
                                    "R" :right)) (str/split directions #""))]
    {:directions directions, :mapping mapping}))

(defn find-repetition [{directions :directions mapping :mapping} start]
  (loop [cnt 0
         [direction & tail] (flatten (repeat directions))
         location start]
    (if (str/ends-with? location "Z")
      cnt
      (recur (inc cnt) tail (get-in mapping [location direction])))))

(defn gcd [a b]
  (if (zero? b) a (recur b, (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn part-1 [input]
  (find-repetition (parse input) "AAA"))

(defn part-2 [input]
  (let [parsed (parse input)]
    (->> (parsed :mapping)
         keys
         (filter #(str/ends-with? % "A"))
         (map #(find-repetition parsed %))
         (reduce lcm))))
