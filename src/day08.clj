(ns day08
  (:require
   clojure.string))

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)]
    {:grid (into {} (for [[i line] (map-indexed vector lines)
                          [j c] (map-indexed vector line)]
                      [[i j] (parse-long (str c))]))
     :rows (count lines)
     :columns (count (first lines))}))

(def dirs [[1 0] [0 1] [-1 0] [0 -1]])

(defn trees-in-dir [grid position dir]
  (->> position
       (iterate (partial mapv + dir))
       (map grid)
       (take-while identity)
       (rest)))

(defn visible-in-dir? [grid position dir]
  (every? (partial > (grid position)) (trees-in-dir grid position dir)))

(defn visible? [grid position]
  (some (partial visible-in-dir? grid position) dirs))

(defn part1 [input]
  (let [{:keys [grid rows columns]} (parse-input input)]
    (->> (for [x (range rows)
               y (range columns)]
           [x y])
         (filter (partial visible? grid))
         (count))))

;; Part 2

(defn viewing-distance [height trees]
  (->> trees
       (partition 2 1)
       (take-while (fn [[x y]] (< x (max height y))))
       count
       inc))

(defn viewing-distance-in-dir [grid position dir]
  (viewing-distance (grid position) (trees-in-dir grid position dir)))

(defn scenic-score [grid position]
  (->> dirs
       (map (partial viewing-distance-in-dir grid position))
       (reduce *)))

(defn part2 [input]
  (let [{:keys [grid rows columns]} (parse-input input)]
    (->> (for [x (range 1 (dec rows))
               y (range 1 (dec columns))]
           [x y])
         (map (partial scenic-score grid))
         (reduce max))))