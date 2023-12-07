(ns aoc.day-07
  (:require [clojure.string :as str]))

(defn rank-it [col]
  (into {} (map-indexed (fn [idx itm] [itm (inc idx)])) col))

(def card-str (rank-it [:J :2 :3 :4 :5 :6 :7 :8 :9 :T :Q :K :A]))
(def hand-str (rank-it [:high-card :one-pair :two-pair :three-of-kind :full-house :four-of-kind :five-of-kind]))

(defn value-exists? [m value]
  (seq (filter (fn [[_ v]] (= v value)) m)))

(defn value-count [m value]
  (count (filter (fn [[_ v]] (= v value)) m)))

(defn four-kind? [freqs joker-count]
  (or
   (value-exists? freqs 4)
   (and (value-exists? freqs 3) (= joker-count 1))
   (and (value-exists? freqs 1) (= joker-count 3))
   (and (< 1 (value-count freqs 2)) (= joker-count 2))))

(defn full-house? [freqs joker-count]
  (or
   (and (= 2 (value-count freqs 2)) (= joker-count 1))
   (and (= 1 (value-count freqs 2)) (= 1 (value-count freqs 3)))))

(defn three-kind? [freqs joker-count]
  (or
   (value-exists? freqs 3)
   (and (value-exists? freqs 2) (= joker-count 1))
   (and (value-exists? freqs 1) (= joker-count 2))))

(defn two-pair? [freqs joker-count]
  (or
   (and (value-exists? freqs 2) (= joker-count 1))
   (= 2 (count (filter #(= 2 %) (map second freqs))))))

(defn five-kind? [freqs joker-count]
  (or
   (value-exists? freqs 5)
   (= joker-count 4)
   (and (value-exists? freqs 4) (= joker-count 1))
   (and (value-exists? freqs 3) (= joker-count 2))
   (and (value-exists? freqs 2) (= joker-count 3))))

(defn one-pair? [freqs joker-count]
  (or
   (value-exists? freqs 2)
   (= 1 joker-count)))

(defn detect-hand-type [hand]
  (let [freqs (frequencies hand)
        joker-count (:J freqs)]
    (cond
      (five-kind? freqs joker-count) :five-of-kind
      (four-kind? freqs joker-count) :four-of-kind
      (full-house? freqs joker-count) :full-house
      (three-kind? freqs joker-count) :three-of-kind
      (two-pair? freqs joker-count) :two-pair
      (one-pair? freqs joker-count) :one-pair
      :else :high-card)))

(defn comp-cards [hand-a hand-b]
  (loop [[a-head & a-tail] hand-a
         [b-head & b-tail] hand-b]
    (if (= (card-str a-head) (card-str b-head))
      (recur a-tail b-tail)
      (< (card-str a-head) (card-str b-head)))))

(defn comp-hands [hand-a hand-b]
  (let [a-str (hand-str (detect-hand-type hand-a))
        b-str (hand-str (detect-hand-type hand-b))]

    (if (= a-str b-str) (comp-cards hand-a hand-b) (< a-str b-str))))

(defn parse [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (let [[cards bid] (str/split line #" ")
                    cards (mapv keyword (str/split cards #""))
                    bid (read-string bid)]
                [cards bid])))))

(defn sort-hands [all-hands]
  (sort-by first comp-hands all-hands))

(defn part-2 [input] (->> second
                          (update-keys (rank-it (sort-hands (parse input))))
                          (map (fn [[k v]] (* k v)))
                          (reduce +)))
