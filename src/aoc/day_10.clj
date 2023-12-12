(ns aoc.day-10
  (:require [clojure.string :as str]
            [aoc.utils :as utils]
            [clojure.set :as setops]
            [clojure.set :as set]))


;; HIC SUNT DRACONES

(defn get-at [[x y] matrix]
  (get (get matrix y :outside) x :outside))

(defn parse [input]
  (mapv #(str/split % #"") (str/split-lines input)))

(defn walk-cw [[start-x start-y] pre-visited matrix]
  (loop [[x y] [start-x start-y]
         visited pre-visited
         visited-set (into #{} visited)]
    (let [visited? #(contains? visited-set %)]
      (if (visited? [x y])
        {:loop-detected visited}
        (let [pipe-type (get-at [x y] matrix)]
          (case pipe-type
            :outside :not-a-loop
            "|" (if (visited? [x (inc y)])
                  (recur [x (dec y)] (conj visited [x y]) (conj visited-set [x y]))
                  (recur [x (inc y)] (conj visited [x y]) (conj visited-set [x y])))

            "-" (if (visited? [(inc x) y])
                  (recur [(dec x) y] (conj visited [x y]) (conj visited-set [x y]))
                  (recur [(inc x) y] (conj visited [x y]) (conj visited-set [x y])))

            "L" (if (visited? [x (dec y)])
                  (recur [(inc x) y] (conj visited [x y]) (conj visited-set [x y]))
                  (recur [x (dec y)] (conj visited [x y]) (conj visited-set [x y])))

            "J" (if (visited? [x (dec y)])
                  (recur [(dec x) y] (conj visited [x y]) (conj visited-set [x y]))
                  (recur [x (dec y)] (conj visited [x y]) (conj visited-set [x y])))

            "F" (if (visited? [(inc x) y])
                  (recur [x (inc y)] (conj visited [x y]) (conj visited-set [x y]))
                  (recur [(inc x) y] (conj visited [x y]) (conj visited-set [x y])))

            "7" (if (visited? [(dec x) y])
                  (recur [x (inc y)] (conj visited [x y]) (conj visited-set [x y]))
                  (recur [(dec x) y] (conj visited [x y]) (conj visited-set [x y])))

            "." :not-a-loop
            "S" {:animal-detected-at [x y] :visited visited}))))))

(defn look-for-animal [matrix]
  (let [len-x (count (first matrix))
        len-y (count matrix)
        to-check (for [x (range 0 len-x)
                       y (range 0 len-y)]
                   [x y])]
    (loop [[head & tail] to-check]
      (let [result (walk-cw head #{} matrix)]
        (if-let [animal-at (result :animal-detected-at)]
          animal-at
          (recur tail))))))

(defn look-for-animal-loop [[animal-x animal-y :as coords] matrix]
  (map :loop-detected (filter (partial not= :not-a-loop) [(walk-cw [(dec animal-x) animal-y] [coords] matrix)
                                                          (walk-cw [(inc animal-x) animal-y] [coords] matrix)
                                                          (walk-cw [animal-x (inc animal-y)] [coords] matrix)
                                                          (walk-cw [animal-x (dec animal-y)] [coords] matrix)])))

(defn part-1 [input]
  (let [matrix (parse input)
        animal-at (look-for-animal matrix)
        animal-loop-size (apply max (map count (look-for-animal-loop animal-at matrix)))]

    (/ animal-loop-size 2)))

(defn cell-on-left [[curr-x curr-y :as current] [prev-x prev-y :as previous] matrix]
  (cond
    ; (and (= "L" (get-at current matrix)) (< prev-y curr-y)) [(dec curr-x) curr-y]
    (and (= "L" (get-at current matrix)) (< curr-x prev-x)) [(dec curr-x) curr-y]
    (and (= "7" (get-at current matrix)) (< prev-x curr-x)) [(inc curr-x) curr-y]
    (< curr-x prev-x) [curr-x (inc curr-y)]
    (< prev-x curr-x) [curr-x (dec curr-y)]
    (< curr-y prev-y) [(dec curr-x) curr-y]
    (< prev-y curr-y) [(inc curr-x) curr-y]))


(def visited-atom (atom #{}))

(defn explore [[origin-x origin-y :as origin] visited-ref path-set]
  ; (println origin visited path-set)
  (let [top [origin-x (dec origin-y)]
        bot [origin-x (inc origin-y)]
        right [(inc origin-x) origin-y]
        left [(dec origin-x) origin-y]
        walls (setops/union @visited-ref path-set)]

    (setops/difference (if (contains? walls origin)
                         @visited-ref
                         (do
                           (swap! visited-atom conj origin)
                           (setops/union
                            (explore top visited-ref path-set)
                            (explore bot visited-ref path-set)
                            (explore right visited-ref path-set)
                            (explore left visited-ref path-set)))) path-set)))

(defn maybe-explore [coords visited path-set]
  (cond
    (contains? visited coords) nil
    (contains? path-set coords) nil
    :else (do (swap! visited-atom (constantly visited))
              (explore coords visited-atom path-set))))

(defn go-around [path matrix]
  (let [path-set (into #{} path)]
    (loop [[head & tail] path
           previous head
           explored #{}]
      (if (nil? head)
        explored
        (let [left-cell (cell-on-left head previous matrix)]
          (if (nil? left-cell)
            (recur tail head explored)
            (recur tail head (setops/union explored (maybe-explore left-cell explored path-set)))))))))

(defn color [color-code s]
  (str "\033[" color-code "m" s "\033[0m"))

(defn mark-path [matrix path]
  (reduce (fn [acc elem] (update-in acc (reverse elem)
                                    (constantly (color 91 (case (get-at elem matrix)
                                                            "\033[90mF\033[0m" "╔"
                                                            "\033[90mJ\033[0m" "╝"
                                                            "\033[90mL\033[0m" "╚"
                                                            "\033[90m7\033[0m" "╗"
                                                            "\033[90m-\033[0m" "═"
                                                            "\033[90m|\033[0m" "║"
                                                            "\033[90mS\033[0m" "S"
                                                            (get-at elem matrix))))))

          matrix path))

(defn mark-enclosed [matrix enclosed]
  (reduce (fn [acc elem] (update-in acc (reverse elem) (constantly (color 94 "@")))) matrix enclosed))

(defn darken-all [matrix]
  (mapv #(mapv (partial color 90) %) matrix))

(defn print-matrix [matrix]
  ; (spit "out.txt" (str/join (map #(str/join %) matrix) "\n"))
  (spit "out.txt" (str/join "\n" (mapv str/join matrix))))

(defn part-2 [input]
  (let [matrix (parse (slurp "input/input-day-10.txt"))
        animal-at (look-for-animal matrix)
        animal-loop (apply max-key count (look-for-animal-loop animal-at matrix))
        ; went-around (go-around (reverse animal-loop) matrix)
        went-around (go-around animal-loop matrix)]

    (print-matrix (mark-enclosed (mark-path (darken-all matrix) animal-loop) went-around))
    (println (str/join "\n" (map str/join (mark-enclosed (mark-path (darken-all matrix) animal-loop) went-around))))
    (println (count went-around))))

(comment
  (conj #{[1 2]} [3 4])
  (conj #{[1 2]} #{[3 4]})
  (setops/union #{[1 2]} #{[3 4]} nil)
  ;; 397 is incorrect

  (let [matrix (parse (slurp "input/test-input-day-10.txt"))
        animal-at (look-for-animal matrix)
        animal-loop (apply max-key count (look-for-animal-loop animal-at matrix))
        explored (explore [4 4] visited-atom (into #{} (reverse animal-loop)))]
    explored

    (count explored))

  ; (cell-on-left [1 2] [1 1])

  (let [matrix (parse (slurp "input/input-day-10.txt"))
        animal-at (look-for-animal matrix)
        animal-loop (apply max-key count (look-for-animal-loop animal-at matrix))
        ; went-around (go-around (reverse animal-loop) matrix)
        went-around (go-around animal-loop matrix)]

    (print-matrix (mark-enclosed (mark-path matrix animal-loop) went-around))
    ;
    ; animal-loop
    (println "final answer" (count went-around))) ;; 105

;; 21
;; 105

  (part-1 (slurp "input/input-day-10.txt"))
  (part-2 (slurp "input/input-day-10.txt"))
  ;;
  )
