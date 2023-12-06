(ns aoc.utils
  (:require [clojure.string :as str]))

(defn split-map [re f s]
  (map f (str/split s re)))

(defn transpose [m]
  (apply mapv vector m))

(defn parse-blocks [raw-input]
  (str/split raw-input #"\n\n"))

(defn update-existing
  "Updates a value in a map given a key and a function, if and only if the key
  exists in the map."
  ([m k f]
   (if-let [kv (find m k)] (assoc m k (f (val kv))) m)))

(defn group-map-by
  "Groups by group-fn and maps the grouped values with map-fn"
  [group-fn map-fn col]
  (update-vals (group-by group-fn col) (partial map map-fn)))

