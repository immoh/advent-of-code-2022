(ns day20
  (:require
   clojure.string))

(defn parse-input [input]
  (into {} (map-indexed (fn [i v]
                          [i {:i i :v (parse-long v)}])
                        (clojure.string/split-lines input))))

(defn find-index-and-value [xs i]
  (first (keep (fn [[j item]]
                 (when (= i (:i item))
                   [j (:v item)]))
               xs)))

(defn sign [n]
  (cond
    (pos? n) 1
    (neg? n) -1
    :else 0))

(defn swaps [m j v]
  (partition 2 1 (take (inc (abs v)) (iterate #(mod (+ % (sign v)) m) j))))

(defn swap-items [xs [i j]]
  (let [vi (xs i)
        vj (xs j)]
    (assoc xs i vj j vi)))

(defn move [xs i]
  (let [[j v] (find-index-and-value xs i)]
    (reduce swap-items xs (swaps (count xs) j (* (sign v) (mod (abs v) (dec (count xs))))))))

(defn mix [xs]
  (reduce move xs (range 0 (count xs))))

(defn as-list [xs]
  (->> xs
       (sort-by key)
       (map (comp :v val))))

(defn grove-coordinates [xs]
  (let [i0 (->> xs (take-while #(not= 0 %)) count)]
    (map #(nth xs (mod (+ % i0) (count xs))) [1000 2000 3000])))

(defn part1 [input]
  (->> (parse-input input)
       mix
       as-list
       grove-coordinates
       (reduce +)))

(defn part2 [input]
  (->> (parse-input input)
       (map (fn [[i item]]
              [i (update item :v * 811589153)]))
       (into {})
       (iterate mix)
       (drop 10)
       first
       as-list
       grove-coordinates
       (reduce +)))
