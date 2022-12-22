(ns day22
  (:require
    clojure.string))

(defn parse-grid [input]
  (let [lines (clojure.string/split-lines input)]
    (into {} (for [[i line] (map-indexed vector lines)
                   [j c] (map-indexed vector line)
                   :when (not= c \space)]
               [[i j] c]))))

(defn parse-path [path]
  (->> (partition-by (comp boolean (set (vec "1234567890"))) path)
       (map #(apply str %))
       (map #(or (parse-long %) %))))

(defn parse-input [input]
  (let [[grid path] (clojure.string/split input #"\n\n")]
    {:grid (parse-grid grid)
     :path (parse-path path)}))

(defn find-start-position [grid]
  (let [y (reduce min (map first (keys grid)))]
    [y (reduce min (map second (filter #(= y (first %)) (keys grid))))]))


(defn next-pos-in-grid [grid position dir]
  (let [[y x :as new-pos] (mapv + position dir)]
    (if (grid new-pos)
      new-pos
      (case dir
        [0 1] [y (reduce min (map second (filter #(= y (first %)) (keys grid))))]
        [0 -1] [y (reduce max (map second (filter #(= y (first %)) (keys grid))))]
        [1 0] [(reduce min (map first (filter #(= x (second %)) (keys grid)))) x]
        [-1 0] [(reduce max (map first (filter #(= x (second %)) (keys grid)))) x]))))

(defn move-1 [grid {:keys [position dir] :as state}]
  (let [new-pos (next-pos-in-grid grid position dir)]
    (case (grid new-pos)
      \. (assoc state :position new-pos)
      \# state)))

(defn move [grid {:keys [position dir] :as state} n]
  (->> state
       (iterate (partial move-1 grid))
       (drop n)
       (first)))

(def new-dir {[[0 1] "L"] [-1 0]
              [[0 1] "R"] [1 0]

              [[0 -1] "L"] [1 0]
              [[0 -1] "R"] [-1 0]

              [[1 0] "L"] [0 1]
              [[1 0] "R"] [0 -1]


              [[-1 0] "L"] [0 -1]
              [[-1 0] "R"] [0 1]})

(defn follow-instruction [grid state instruction]
  (case instruction
    ("L" "R") (update state :dir #(new-dir [% instruction]))
    (move grid state instruction)))

(defn password [{[y x] :position dir :dir}]
  (+ (* 1000 (inc y))
     (* 4 (inc x))
     ({[0 1] 0
       [1 0] 1
       [0 -1] 2
       [-1 0] 3} dir)))

(defn part1 [input]
  (let [{:keys [grid path]} (parse-input input)]
    (-> (reduce
          (partial follow-instruction grid)
          {:position (find-start-position grid) :dir [0 1]}
          path)
        password)))

;; Part 2

(defn tile-no [[y x]]
  (+ (* 3 (quot y 50)) (quot x 50)))


(defn next-state [grid {:keys [position dir]}]
  (let [[y x :as new-pos] (mapv + position dir)]
    (if (grid new-pos)
      {:position new-pos :dir dir}
      (case [(tile-no position) dir]

        ;; left, 6 upside down
        [1 [0 -1]] {:position [(- 149 y) 0] :dir [0 1]}

        ;; up, 9 pointing left
        [1 [-1 0]] {:position [(+ 100 x) 0] :dir [0 1]}

        ;; up, 9 normal pos
        [2 [-1 0]] {:position [199 (- x 100)] :dir dir}

        ;; right, 7 upside down
        [2 [0 1]] {:position [(- 149 y) 99] :dir [0 -1]}

        ;; down, 4 pointing left
        [2 [1 0]] {:position [(- x 50) 99] :dir [0 -1]}

        ;; left, 6 pointing right
        [4 [0 -1]] {:position [100 (- y 50)] :dir [1 0]}

        ;; right, 2 pointing right
        [4 [0 1]] {:position [49 (+ y 50)] :dir [-1 0]}

        ;; left, 1 upside down
        [6 [0 -1]] {:position [(- 49 (- y 100)) 50] :dir [0 1]}

        ;; up, 4 pointing left
        [6 [-1 0]] {:position [(+ x 50) 50] :dir [0 1]}

        ;; right, 2 upside down
        [7 [0 1]] {:position [(- 49 (- y 100)) 149] :dir [0 -1]}

        ;; down, 9 pointing left
        [7 [1 0]] {:position [(+ x 100) 49] :dir [0 -1]}

        ;; left, 1 pointing right
        [9 [0 -1]] {:position [0 (- y 100)] :dir [1 0]}

        ;; down, 2 normal pos
        [9 [1 0]] {:position [0 (+ x 100)] :dir dir}

        ;; right, 7 pointing right
        [9 [0 1]] {:position [149 (- y 100)] :dir [-1 0]}))))

(defn move-12 [grid state]
  (let [new-state (next-state grid state)]
    (case (grid (:position new-state))
      \. new-state
      \# state)))

(defn move2 [grid state n]
  (->> state
       (iterate (partial move-12 grid))
       (drop n)
       (first)))

(defn follow-instruction2 [grid state instruction]
  (case instruction
    ("L" "R") (update state :dir #(new-dir [% instruction]))
    (move2 grid state instruction)))

;; Only works with input of shape:
;;  12
;;  4
;; 67
;; 9
;; and 50x50 tiles
(defn part2 [input]
  (let [{:keys [grid path]} (parse-input input)]
    (-> (reduce
          (partial follow-instruction2 grid)
          {:position (find-start-position grid) :dir [0 1]}
          path)
        password)))
