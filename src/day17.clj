(ns day17
  (:require
    clojure.set))

(def rock-shapes
  [;;-
   #{[0 0] [0 1] [0 2] [0 3]}

   ;;+
   #{[0 1] [1 0] [1 1] [1 2] [2 1]}

   ;; L (pointing left)
   #{[0 0] [0 1] [0 2] [1 2] [2 2]}

   ;; |
   #{[0 0] [1 0] [2 0] [3 0]}

   ;; square
   #{[0 0] [0 1] [1 0] [1 1]}])

(defn move-rock [rocks rock dir]
  (let [new-rock (set (map #(mapv + dir %) rock))]
    (if (and (every? #(<= 0 % 6) (map second new-rock))
             (not-any? rocks new-rock))
      new-rock
      rock)))

(def jet-dir {\< [0 -1]
              \> [0 1]})

(defn new-height-and-rocks [height rocks rock]
  (let [rocks (clojure.set/union rocks rock)
        max-height (reduce max (map first rocks))]
    {:rocks (->> rocks
                 (map (fn [[y x]] [(- y max-height) x]))
                 ;; It is enough to keep last 35 rows
                 (remove (fn [[y x]] (< y -35)))
                 set)
     :height (+ height max-height)}))

(defn drop-rock [{:keys [jets jet-index rock-shape-index rocks height]}]
  (loop [jet-index jet-index
         rock (move-rock rocks (rock-shapes rock-shape-index) [4 2])]
    (let [rock (move-rock rocks rock (jet-dir (jets jet-index)))
          new-rock (move-rock rocks rock [-1 0])]
      (if (= rock new-rock)
        (merge
          {:jets             jets
           :jet-index        (mod (inc jet-index) (count jets))
           :rock-shape-index (mod (inc rock-shape-index) (count rock-shapes))}
          (new-height-and-rocks height rocks new-rock))
        (recur (mod (inc jet-index) (count jets)) new-rock)))))

(defn find-loop [input]
  (loop [i 0
         cache {}
         state {:jets             (vec input)
                :jet-index        0
                :rock-shape-index 0
                :height           0
                :rocks            (set (for [x (range 7)] [0 x]))}]
    (let [cache-key (select-keys state [:jet-index :rock-shape-index :rocks])]
      (if-let [loop-start-state (cache cache-key)]
        {:start-state (dissoc loop-start-state :i)
         :start-index (:i loop-start-state)
         :start-height (:height loop-start-state)
         :length (- i (:i loop-start-state))
         :height-addition (- (:height state) (:height loop-start-state))}
        (recur (inc i)
               (assoc cache cache-key (merge state {:i i}))
               (drop-rock state))))))

(defn height-after-iterations [input iterations]
  (let [{:keys [start-state start-index start-height length height-addition]} (find-loop input)]
    (->> (merge start-state
                {:height (+ start-height
                            (* (quot (- iterations start-index) length) height-addition))})
         (iterate drop-rock)
         (drop (rem (- iterations start-index) length))
         first
         :height)))

(defn part1 [input]
  (height-after-iterations input 2022))

(defn part2 [input]
  (height-after-iterations input 1000000000000))
