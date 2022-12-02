(ns day02
  (:require
   clojure.set
   clojure.string))

(defn parse-input [input]
  (map #(clojure.string/split % #" ") (clojure.string/split-lines input)))

(def input-mapping1 {"A" :rock
                     "B" :paper
                     "C" :scissors

                     "X" :rock
                     "Y" :paper
                     "Z" :scissors})

(def shape-scoring {:rock 1
                    :paper 2
                    :scissors 3})

(def result-scoring {:lose 0
                     :draw 3
                     :win 6})

(def winner-for-shape {:rock :paper
                       :paper :scissors
                       :scissors :rock})

(defn score [result my-shape]
  (+ (result-scoring result) (shape-scoring my-shape)))

(defn game-result [[opponent-shape my-shape]]
  (cond
    (= my-shape (winner-for-shape opponent-shape)) :win
    (= my-shape opponent-shape) :draw
    :else :lose))

(defn score1 [[_ my-shape :as game]]
  (score (game-result game) my-shape))

(defn part1 [input]
  (->> (parse-input input)
       (map (partial map input-mapping1))
       (map score1)
       (reduce +)))

;; Part 2

(def input-mapping2 {"A" :rock
                     "B" :paper
                     "C" :scissors

                     "X" :lose
                     "Y" :draw
                     "Z" :win})

(def loser-for-shape (clojure.set/map-invert winner-for-shape))

(defn deduct-shape [[opponent-shape result]]
  (case result
    :win (winner-for-shape opponent-shape)
    :draw opponent-shape
    :lose (loser-for-shape opponent-shape)))

(defn score2 [[_ result :as game]]
  (score result (deduct-shape game)))

(defn part2 [input]
  (->> (parse-input input)
       (map (partial map input-mapping2))
       (map score2)
       (reduce +)))
