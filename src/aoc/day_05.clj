(ns aoc.day-05
  (:require [clojure.string :as str]
            [aoc.utils :as u]))

(defn parse [input]
  (let [[seeds & maps] (str/split input #"\n\n")
        seeds (map read-string (re-seq #"\d+" seeds))
        maps (map (fn [block] (map (partial u/split-map #" " read-string) (rest (str/split-lines block)))) maps)]

    {:seeds seeds :maps maps}))

(defn try-mapping [from [dest src range]]
  (if (<= src from (+ src range))
    (+ dest (- from src))))

(defn mapping [from mappings]
  (loop [[head & tail] mappings]
    (if (nil? head)
      from
      (if-let [mapped (try-mapping from head)] mapped (recur tail)))))

(defn chain [from all-mappings]
  (loop [[head & tail] all-mappings
         num from]
    (if (nil? head)
      num
      (recur tail (mapping num head)))))

(defn lowest [seeds all-mappings]
  (reduce min (map #(chain % all-mappings) seeds)))

(defn part-1 [input]
  (#(lowest (:seeds %) (:maps %)) (parse input)))

;;;;p2

(defn intersect-range [[a-start a-end] [b-start b-end]]
  (if (or (> b-start a-end) (> a-start b-end))
    nil
    [(max a-start b-start) (min a-end b-end)]))

(defn intersect-and-remap [seed [dest src len]]
  (let [remap-diff (- dest src)]
    (mapv (partial + remap-diff) (intersect-range seed [src (dec (+ src len))]))))

(defn intersect-mapping [seed [_ src len]]
  (intersect-range seed [src (dec (+ src len))]))

;; Probably could get away with less checks here, but I'm afraid to touch it
(defn subract-range [[a-start a-end] [b-start b-end]]
  (cond
    (and (< b-start a-start) (< a-start b-end a-end)) [[(inc b-end) a-end]]
    (and (= a-start b-start) (< b-end a-end)) [[(inc b-end) a-end]]
    (and (< a-start b-start) (= b-end a-end)) [[a-start (dec b-start)]]
    (and (< b-start a-start) (= b-end a-start)) [[(inc a-start) a-end]]
    (and (= a-start b-start) (= a-end b-end)) []
    (< a-start b-start b-end a-end) [[a-start (dec b-start)] [(inc b-end) a-end]]
    (= a-end b-start)  [[a-start (dec a-end)]]
    (< a-start b-start a-end) [[a-start (dec b-start)]]
    :else [[a-start a-end]]))

(defn subtract-all [original all-to-diff]
  (loop [acc [original]
         [head & tail] all-to-diff]
    (if (nil? head)
      acc
      (recur (mapcat #(subract-range % head) acc) tail))))

(defn apply-maps [seed iteration-maps]
  (let [intersections (filter seq (mapv #(intersect-mapping seed %) iteration-maps))
        remapped-intersections (filter seq (mapv #(intersect-and-remap seed %) iteration-maps))
        passtrough-diffs (subtract-all seed intersections)]

    (into remapped-intersections passtrough-diffs)))

(defn part-2 [input]
  (let [{seeds :seeds maps :maps} (parse input)
        seed-ranges (map (fn [[start size]] [start (dec (+ start size))]) (partition 2 seeds))]

    (loop [ranges seed-ranges
           [head & tail] maps]
      (if (nil? head)
        (apply min (map first ranges))
        (recur (mapcat #(apply-maps % head) ranges) tail)))))

(comment
  (time (part-1 (slurp "input/input-day-05.txt"))) ;; 331445006 (2ms)
  (time (part-2 (slurp "input/input-day-05.txt"))) ;; 6472060 (11ms)
  )
