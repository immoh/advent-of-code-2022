(ns day06)

(defn find-marker-position [s n]
  (->> s
       (partition n 1)
       (take-while #(not= n (count (set %))))
       count
       (+ n)))

(defn part1 [input]
  (find-marker-position input 4))

(defn part2 [input]
  (find-marker-position input 14))
