(ns day23
  (:require
    clojure.string))

(defn parse-input [input]
  (set (for [[i line] (map-indexed vector (clojure.string/split-lines input))
             [j c] (map-indexed vector line)
             :when (= \# c)]
         [i j])))

(def check-dirs {"N" [[-1 -1] [-1 0] [-1 1]]
                 "S" [[1 -1] [1 0] [1 1]]
                 "W" [[-1 -1] [0 -1] [1 -1]]
                 "E" [[-1 1] [0 1] [1 1]]})

(def adjacent-dirs (for [y [-1 0 1]
                         x [-1 0 1]
                         :when (not= 0 x y)]
                     [y x]))

(defn allowed-position [elves elf dir]
  (when (not-any? elves (map #(mapv + elf %) (check-dirs dir)))
    (mapv + elf (get-in check-dirs [dir 1]))))

(defn move [elves priority elf]
  [elf (or (and (some elves (map #(mapv + elf %) adjacent-dirs))
                (some (partial allowed-position elves elf) priority))
         elf)])

(defn round [elves i]
  (let [priority (->> ["N" "S" "W" "E"]
                      cycle
                      (drop i)
                      (take 4))
        suggested-moves (map (partial move elves priority) elves)
        valid-new-positions (->> suggested-moves
                                 (map second)
                                 frequencies
                                 (filter #(= 1 (val %)))
                                 keys
                                 set)]
    (set (map (fn [[old new]]
                (if (valid-new-positions new)
                  new
                  old))
              suggested-moves))))

(defn empty-ground-tiles-count [elves]
  (- (* (inc (- (reduce max (map first elves))
                (reduce min (map first elves))))
        (inc (- (reduce max (map second elves))
                (reduce min (map second elves)))))
     (count elves)))

(defn part1 [input]
  (-> (reduce
        round
        (parse-input input)
        (range 10))
      empty-ground-tiles-count))

(defn part2 [input]
  (->> {:elves (parse-input input) :i 0}
       (iterate (fn [{:keys [elves i]}]
                  {:elves (round elves i) :i (inc i)}))
       (partition 2 1)
       (drop-while (fn [[{elves1 :elves} {elves2 :elves}]]
                     (not= elves1 elves2)))
       first
       second
       :i))
