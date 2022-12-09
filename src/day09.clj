(ns day09
  (:require
   clojure.math
   clojure.string))

(defn parse-input [input]
  (map (fn [line]
         (let [[d n] (clojure.string/split line #" ")]
           [d (parse-long n)]))
       (clojure.string/split-lines input)))

(def dirs {"R" [0 1]
           "L" [0 -1]
           "U" [1 0]
           "D" [-1 0]})

(defn follow [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (if (and (<= (abs dx) 1) (<= (abs dy) 1))
      [x1 y1]
      [(+ x1 (long (clojure.math/signum dx)))
       (+ y1 (long (clojure.math/signum dy)))])))

(defn move-body [rope]
  (reduce
   (fn [rope i]
     (update rope i follow (rope (dec i))))
   rope
   (range 1 (count rope))))

(defn move-head [rope d]
  (update rope 0 #(mapv + % (dirs d))))

(defn move-rope [rope d]
  (-> rope
      (move-head d)
      move-body))

(defn rope-positions [rope instructions]
  (reductions
   move-rope
   rope
   (mapcat (fn [[d n]] (repeat n d)) instructions)))

(defn distinct-tail-position-count [rope-length instructions]
  (->> instructions
       (rope-positions (vec (repeat rope-length [0 0])))
       (map last)
       set
       count))

(defn part1 [input]
  (distinct-tail-position-count 2 (parse-input input)))

(defn part2 [input]
  (distinct-tail-position-count 10 (parse-input input)))
