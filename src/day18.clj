(ns day18
  (:require
    clojure.string))

(defn parse-input [input]
  (->> input
       clojure.string/split-lines
       (map (fn [line]
              (->> line
                   (re-seq #"\d+")
                   (mapv parse-long))))
       set))

(def dirs [[1 0 0]
           [-1 0 0]
           [0 1 0]
           [0 -1 0]
           [0 0 1]
           [0 0 -1]])

(defn neighbors [pos]
  (map (partial mapv + pos) dirs))

(defn part1 [input]
  (let [droplet (parse-input input)]
    (->> droplet
         (map (fn [pos]
                (->> pos
                     neighbors
                     (remove droplet)
                     (count))))
         (reduce +))))

;; Part 2

(defn path-exists? [start end neighbors-fn]
  (loop [wip #{start}
         visited #{start}]
    (when-let [node (first (sort-by (fn [[x y z]]
                                      (+ (* (abs x) (abs x))
                                         (* (abs y) (abs y))
                                         (* (abs z) (abs z))))
                                    wip))]
      (if (= node end)
        true
        (let [neighbors (remove visited (neighbors-fn node))]
          (recur (-> wip
                     (into neighbors)
                     (disj node))
                 (conj visited node)))))))

(defn exterior?* [droplet pos]
  (path-exists? pos [0 0 0] (fn [pos] (->> pos neighbors (remove droplet)))))

(def exterior? (memoize exterior?*))

(defn exterior-sides [droplet pos]
  (->> pos
       neighbors
       (remove droplet)
       (filter (partial exterior? droplet))
       count))

;; Elapsed time: 16586.304583 msecs
(defn part2 [input]
  (let [droplet (parse-input input)]
    (->> droplet
         (map (partial exterior-sides droplet))
         (reduce +))))
