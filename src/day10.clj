(ns day10
  (:require
    clojure.string))

(defn parse-input [input]
  (map #(clojure.string/split % #" ") (clojure.string/split-lines input)))

(defn execute-instruction [x [op n]]
  (case op
    "noop" x
    "addx" (+ x (parse-long n))))

(defn x-values [instructions]
  (->> instructions
       (mapcat (fn [[op :as instruction]]
                 (if (= "addx" op)
                   [["noop"] instruction]
                   [instruction])))
       (reductions execute-instruction 1)))

(defn signal-strengths [xs]
  (map (fn [n]
         (* n (first (drop (dec n) xs))))
       (range 20 221 40)))

(defn part1 [input]
  (->> (parse-input input)
       x-values
       signal-strengths
       (reduce +)))

(defn part2 [input]
  (->> (parse-input input)
       x-values
       (map-indexed (fn [i x]
                      (if (<= (dec x) (mod i 40) (inc x))
                        \#
                        \.)))
       (partition 40)
       (map (partial apply str))))
