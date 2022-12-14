(ns day14
  (:require
   clojure.string))

(defn parse-input [input]
  (map (fn [line]
         (->> line
              (re-seq #"\d+")
              (map parse-long)
              (partition 2)))
       (clojure.string/split-lines input)))

(defn sign [n]
  (cond
    (zero? n) 0
    (pos? n) 1
    :else -1))

(defn points [path]
  (mapcat (fn [[start end]]
            (let [dir (mapv (comp sign -) end start)]
              (concat (take-while #(not= end %) (iterate (partial mapv + dir) start))
                      [end])))
          (partition 2 1 path)))

(defn world [paths]
  (let [rocks (set (mapcat points paths))]
    {:rocks rocks
     :sand #{}
     :bottom (reduce max (map second rocks))}))

(defn next-sand-pos-1 [{:keys [rocks sand]} pos]
  (->> [[0 1] [-1 1] [1 1]]
       (map (partial mapv + pos))
       (drop-while #(or (rocks %) (sand %)))
       first))

(defn next-sand-pos [{:keys [bottom] :as world}]
  (loop [[x y :as pos] [500 0]]
    (when (< y bottom)
      (if-let [next-pos (next-sand-pos-1 world pos)]
        (recur next-pos)
        pos))))

(defn final-world [world]
  (if-let [sand-pos (next-sand-pos world)]
    (recur (update world :sand conj sand-pos))
    world))

(defn part1 [input]
  (->> (parse-input input)
       world
       final-world
       :sand
       count))

;; Part 2

(defn next-sand-pos2 [{:keys [bottom] :as world}]
  (loop [[x y :as pos] [500 0]]
    (if (= y (inc bottom))
      pos
      (if-let [next-pos (next-sand-pos-1 world pos)]
        (recur next-pos)
        pos))))

(defn final-world2 [{:keys [sand] :as world}]
  (if (sand [500 0])
    world
    (recur (update world :sand conj (next-sand-pos2 world)))))

(defn part2 [input]
  (->> (parse-input input)
       world
       final-world2
       :sand
       count))
