(ns aoc.day-15
  (:require [clojure.string :as str]))

(defn -hash [s]
  (reduce #(rem (* 17 (+ %1 (int %2))) 256) 0 s))

(defn parse-instruction [s]
  (let [[_ label op focal-length] (re-find #"([a-z]+)([\=|\-])([0-9]?)" s)
        base {:label label :label-hash (-hash label) :op op}]
    (if (empty? focal-length)
      base
      (assoc base :focal-length (Integer/parseInt focal-length)))))

(defn replace-lense [lense-stack {label :label :as lense}]
  (loop [[head & tail] lense-stack
         acc []]
    (if (nil? head)
      (conj acc lense)
      (if (= label (:label head))
        (concat (conj acc lense) tail)
        (recur tail (conj acc head))))))

(defn remove-lense [lense-stack {label :label}]
  (loop [[head & tail] lense-stack
         acc []]
    (if (nil? head)
      acc
      (if (= label (:label head))
        (concat acc tail)
        (recur tail (conj acc head))))))

(defn apply-instruction [boxes {op :op at :label-hash :as lense}]
  (update boxes at #((case op "=" replace-lense "-" remove-lense) % lense)))

(defn score-lense-stack [box-idx lense-stack]
  (loop [acc 0
         stack-slot 1
         [head & tail] lense-stack]
    (if (nil? head)
      acc
      (recur (+ acc (* (inc box-idx) stack-slot (:focal-length head))) (inc stack-slot) tail))))

(defn score-boxes [boxes]
  (reduce-kv (fn [acc idx box] (+ acc (score-lense-stack idx box))) 0 boxes))

(defn part-1 [input]
  (reduce + (map -hash (str/split (str/trim input) #","))))

(defn part-2 [input]
  (->> (str/split (str/trim input) #",")
       (map parse-instruction)
       (reduce apply-instruction (into [] (repeat 256 [])))
       score-boxes))

(comment
  (part-1 (slurp "input/input-day-15.txt")) ;; 510792
  (part-2 (slurp "input/input-day-15.txt")) ;; 269410
  )
