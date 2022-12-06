(ns day05
  (:require
    clojure.string))

(defn parse-stacks [lines]
  (->> lines
       drop-last
       (map (fn [line]
              (map second (partition-all 4 line))))
       (apply map vector)
       (mapv (partial remove #{\space}))))

(defn parse-procedure [lines]
  (map (fn [line]
         (map parse-long (re-seq #"\d+" line)))
       lines))

(defn parse-input [input]
  (let [[stacks _ procedure] (partition-by #{""} (clojure.string/split-lines input))]
    {:stacks (parse-stacks stacks)
     :procedure (parse-procedure procedure)}))

(defn apply-instruction [transform-fn stacks [n from to]]
  (let [[moved remaining] (split-at n (stacks (dec from)))]
    (-> stacks
        (assoc (dec from) remaining)
        (update (dec to) #(concat (transform-fn moved) %)))))

(defn run-procedure [transform-fn {:keys [stacks procedure]}]
  (reduce (partial apply-instruction transform-fn) stacks procedure))

(defn solve [transform-fn input]
  (->> input
       parse-input
       (run-procedure transform-fn)
       (map first)
       (apply str)))

(defn part1 [input]
  (solve reverse input))

(defn part2 [input]
  (solve identity input))
