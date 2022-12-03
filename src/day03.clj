(ns day03
  (:require
    clojure.set
    clojure.string))

(defn split-in-half [xs]
  (split-at (/ (count xs) 2) xs))

(defn priority [c]
  (- (int c) (if (>= (int c) 97) 96 38)))

(defn find-common [groups]
  (->> groups
       (map set)
       (apply clojure.set/intersection)
       first))

(defn part1 [input]
  (->> input
       clojure.string/split-lines
       (map (fn [line]
              (->> line
                   split-in-half
                   find-common
                   priority)))
       (reduce +)))

(defn part2 [input]
  (->> input
       clojure.string/split-lines
       (partition 3)
       (map (fn [groups]
              (->> groups
                   find-common
                   priority)))
       (reduce +)))
