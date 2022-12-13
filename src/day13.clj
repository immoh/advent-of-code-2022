(ns day13
  (:require
   clojure.string))

(defn parse-input [input]
  (map #(mapv read-string (clojure.string/split-lines %)) (clojure.string/split input #"\n\n")))

(defn packet-compare [l r]
  (cond
    (and (sequential? l) (int? r)) (recur l [r])
    (and (int? l) (sequential? r)) (recur [l] r)
    (and (sequential? l) (sequential? r)) (loop [l l r r]
                                            (cond
                                              (= l r) 0
                                              (not (seq l)) -1
                                              (not (seq r)) 1
                                              :else (let [n (packet-compare (first l) (first r))]
                                                      (if (zero? n)
                                                        (recur (rest l) (rest r))
                                                        n))))
    :else (compare (or l -1) (or r -1))))

(defn part1 [input]
  (->> (parse-input input)
       (keep-indexed (fn [i pair]
                       (when (= -1 (apply packet-compare pair))
                         (inc i))))
       (reduce +)))

(defn part2 [input]
  (->> (parse-input input)
       (mapcat identity)
       (into [[[2]] [[6]]])
       (sort packet-compare)
       (keep-indexed (fn [i packet]
                       (when (#{[[2]] [[6]]} packet)
                         (inc i))))
       (reduce *)))
