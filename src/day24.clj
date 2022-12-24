(ns day24
  (:require
    clojure.string))

(def blizzard-dirs {\< [0 -1]
                    \> [0 1]
                    \^ [-1 0]
                    \v [1 0]})

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)
        rows (- (count lines) 2)
        cols (- (count (first lines)) 2)]
    {:rows      rows
     :cols      cols
     :blizzards (for [[i line] (map-indexed vector lines)
                      [j c] (map-indexed vector line)
                      :let [dir (blizzard-dirs c)]
                      :when dir]
                  {:position [(dec i) (dec j)]
                   :dir      dir})}))

(defn find-shortest-path* [start end-pred neighbors-fn]
  (loop [wip #{start}
         visited #{start}]
    (when-let [node (first (sort-by :i wip))]
      (if (end-pred node)
        node
        (let [neighbors (remove visited (neighbors-fn node))]
          (recur (into (disj wip node) neighbors)
                 (conj visited node)))))))

(defn move-blizzard [{:keys [rows cols]} {[y x] :position [dy dx] :dir :as blizzard}]
  (assoc blizzard :position [(mod (+ y dy) rows)
                             (mod (+ x dx) cols)]))

(defn move-blizzards [world]
  (update world :blizzards #(map (partial move-blizzard world) %)))

(defn neighbor-positions [{:keys [rows cols]} [y x]]
  (cond
    (= [0 0] [y x])
    [[-1 0] [0 1] [1 0]]

    (= [(dec rows) (dec cols)] [y x])
    [[(dec y) x] [y (dec x)] [(inc y) x]]

    :else
    (->> [[-1 0]
          [1 0]
          [0 -1]
          [0 1]]
         (map (fn [[dy dx]] [(+ y dy) (+ x dx)]))
         (filter (fn [[y x]] (and (<= 0 y (dec rows))
                                  (<= 0 x (dec cols))))))))

(defn blizzard-positions* [world i]
  (->> world
       (iterate move-blizzards)
       (drop i)
       (first)
       :blizzards
       (map :position)
       set))

(def blizzard-positions (memoize blizzard-positions*))

(defn next-states [world {:keys [i position]}]
  (->> position
       (neighbor-positions world)
       (into [position])
       (remove (blizzard-positions world (inc i)))
       (map (fn [position] {:i (inc i) :position position}))))


(defn find-shortest-path [world to-position start]
  (find-shortest-path* start
                       #(= to-position (:position %))
                       (partial next-states world)))

(defn part1 [input]
  (let [{:keys [rows cols] :as world} (parse-input input)]
    (->> {:i 0 :position [-1 0]}
         (find-shortest-path world [rows (dec cols)])
         :i)))


(defn part2 [input]
  (let [{:keys [rows cols] :as world} (parse-input input)]
    (->> {:i 0 :position [-1 0]}
         (find-shortest-path world [rows (dec cols)])
         (find-shortest-path world [-1 0])
         (find-shortest-path world [rows (dec cols)])
         :i)))
