(ns day12)

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)]
    (into {} (for [[i line] (map-indexed vector lines)
                   [j c] (map-indexed vector line)]
               [[i j] c]))))

(defn find-shortest-path [start end neighbors-fn dist-fn]
  (loop [wip {start 0}
         visited #{start}]
    (when-let [[node dist] (first (sort-by val wip))]
      (if (= node end)
        dist
        (let [neighbors (remove visited (neighbors-fn node))]
          (recur (merge-with min
                             (dissoc wip node)
                             (zipmap neighbors
                                     (map (fn [node2]
                                            (+ dist (dist-fn node node2)))
                                          neighbors)))
                 (conj visited node)))))))

(defn find-positions [grid c]
  (->> grid (filter #(= c (val %))) keys))

(defn elevation [c]
  (int (get {\S \a \E \z} c c)))

(defn neighbors [grid pos]
  (->> [[0 1] [0 -1] [1 0] [-1 0]]
       (map (partial mapv + pos))
       (filter grid)
       (filter (fn [dest]
                 (<= (- (elevation (grid dest)) (elevation (grid pos))) 1)))))

(defn part1 [input]
  (let [grid (parse-input input)]
    (find-shortest-path (first (find-positions grid \S))
                        (first (find-positions grid \E))
                        (partial neighbors grid)
                        (constantly 1))))

(defn part2 [input]
  (let [grid (parse-input input)]
    (->> (find-positions grid \a)
         (keep #(find-shortest-path %
                                    (first (find-positions grid \E))
                                    (partial neighbors grid)
                                    (constantly 1)))
         (reduce min))))
