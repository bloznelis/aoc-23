(ns aoc.utils)

(defn transpose [m]
  (apply mapv vector m))

(defn parse-blocks [raw-input]
  (str/split raw-input #"\n\n"))

(defn update-existing
  "Updates a value in a map given a key and a function, if and only if the key
  exists in the map."
  ([m k f]
   (if-let [kv (find m k)] (assoc m k (f (val kv))) m)))
