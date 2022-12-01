(ns day01
  (:require
   clojure.string))

(defn parse-input [input]
  (->> input
       (clojure.string/split-lines)
       (map parse-long)
       (partition-by nil?)
       (remove (comp nil? first))
       (map (partial reduce +))))

(defn part1 [input]
  (->> input
       (parse-input)
       (reduce max)))

(defn part2 [input]
  (->> input
       (parse-input)
       (sort)
       (reverse)
       (take 3)
       (reduce +)))
