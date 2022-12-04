(ns day04
  (:require
    clojure.string))

(defn parse-input [input]
  (map (fn [line]
         (let [[_ x1 y1 x2 y2] (re-find #"(\d+)-(\d+),(\d+)-(\d+)" line)]
           [(map parse-long [x1 y1]) (map parse-long [x2 y2])]))
       (clojure.string/split-lines input)))

(defn fully-contained? [[[x1 y1] [x2 y2]]]
  (or (<= x1 x2 y2 y1)
      (<= x2 x1 y1 y2)))

(defn overlaps? [[[x1 y1] [x2 y2]]]
  (or (<= x1 x2 y1)
      (<= x1 y2 y1)
      (<= x2 x1 y1 y2)))

(defn part1 [input]
  (->> input
       parse-input
       (filter fully-contained?)
       count))

(defn part2 [input]
  (->> input
       parse-input
       (filter overlaps?)
       count))
