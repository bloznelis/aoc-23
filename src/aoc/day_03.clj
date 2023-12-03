(ns aoc.day-03
  (:require [clojure.string :as str]))

(defn read-matrix [input]
  (mapv #(str/split % #"") (str/split-lines input)))

(defn get-at [[x y] matrix]
  (nth (nth matrix y nil) x nil))

(defn safe-read-string [a]
  (try (read-string a) (catch Exception _ nil)))

(defn valid-neighbors [[x y] matrix]
  (for [[x-mul y-mul] [[0 1] [1 0] [-1 0] [0 -1] [1 -1] [-1 1] [1 1] [-1 -1]]
        :let [neigh-coords [(+ x x-mul) (+ y y-mul)]
              neigh (get-at neigh-coords matrix)]
        :when (some? neigh)]
    (get-at neigh-coords matrix)))

(defn star-neighbors [[x y] matrix]
  (for [[x-mul y-mul] [[0 1] [1 0] [-1 0] [0 -1] [1 -1] [-1 1] [1 1] [-1 -1]]
        :let [neigh-coords [(+ x x-mul) (+ y y-mul)]
              neigh (get-at neigh-coords matrix)]
        :when (and (some? neigh) (= neigh "*"))]
    neigh-coords))

(defn append-num [current-num new-char]
  (if (number? (safe-read-string new-char))
    (str current-num new-char)))

(defn find-row-nums [row y]
  (loop [[head & tail] row
         x 0
         current-num nil
         current-num-coords []
         acc []]
    (if (nil? head)
      (if (nil? current-num) acc (conj acc [current-num current-num-coords]))
      (if-let [new-num (append-num current-num head)]
        (recur tail (inc x) new-num (conj current-num-coords [x y]) acc)
        (recur tail (inc x) nil [] (if (nil? current-num) acc (conj acc [current-num current-num-coords])))))))

(defn find-all-nums [matrix]
  (->> matrix
       (map-indexed (fn [idx itm] (find-row-nums itm idx)))
       (mapcat identity)
       (into [])))

(defn validate-num [[_ coords] matrix]
  (->> coords
       (mapcat #(valid-neighbors % matrix))
       (filter #(and (not (number? (safe-read-string %))) (not= "." %)))
       seq))

(defn num-gear [[num coords] matrix]
  (->> coords
       (mapcat #(star-neighbors % matrix))
       distinct
       (vector num)))

(defn find-gear-pair [[_ stars-at] nums-with-gears]
  (map (fn [star-at]
         (filter (fn [[_ stars-at]]
                   (contains? (set stars-at) star-at)) nums-with-gears)) stars-at))

(defn part-1 [input]
  (let [matrix (read-matrix input)]
    (->> (find-all-nums matrix)
         (filter #(validate-num % matrix))
         (map first)
         (map read-string)
         (reduce +))))

(defn part-2 [input]
  (let [matrix (read-matrix input)
        nums-with-gears (->> (find-all-nums matrix)
                             (map #(num-gear % matrix))
                             (filter #(seq (second %))))]

    (->> (mapcat #(find-gear-pair % nums-with-gears) nums-with-gears)
         (filter #(= 2 (count %)))
         distinct
         (mapcat identity)
         (group-by second)
         vals
         (map (fn [x] (reduce * (map read-string (map first x)))))
         (reduce +))))
