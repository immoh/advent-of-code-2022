(ns day15
  (:require
   clojure.string))

(defn parse-input [input]
  (map (fn [line]
         (->> line
              (re-seq #"-?\d+")
              (map parse-long)
              (partition 2)))
       (clojure.string/split-lines input)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn sensor-xs-1 [[[sx sy :as sensor] beacon]]
  (let [d (manhattan-distance sensor beacon)
        r (- d (abs (- sy 2000000)))]
    (when (>= r 0)
      (set (range (- sx r) (+ sx r 1))))))

(defn sensor-xs [sensor-report]
  (reduce into #{} (map sensor-xs-1 sensor-report)))

(defn beacon-xs [sensor-report]
  (->> sensor-report
       (map second)
       (filter (fn [[x y]] (= y 2000000)))
       (map first)
       set))

(defn part1 [input]
  (let [sensor-report (parse-input input)]
    (count (remove (beacon-xs sensor-report) (sensor-xs sensor-report)))))

;; Part2

(defn cut-1 [[a1 a2 :as r] [b1 b2]]
  (cond
    ;; on the left side
    (< b2 a1) [r]

    ;; on the right side
    (> b1 a2) [r]

    ;; cuts part on the left
    (and (<= b1 a1) (< b2 a2)) [[(inc b2) a2]]

    ;; cuts part on the right
    (and (< a1 b1) (<= a2 b2)) [[a1 (dec b1)]]

    ;; cuts part in the middle
    (and (< a1 b1 a2) (< a1 b2 a2)) [[a1 (dec b1)] [(inc b2) a2]]))

(defn cut [rs r]
  (mapcat #(cut-1 % r) rs))

(defn cut-ranges [remaining to-cut]
  (reduce (fn [remaining [i r]]
            (if (seq (remaining i))
              (update remaining i cut r)
              (dissoc remaining i)))
          remaining
          to-cut))

(defn known-ranges [[[sx sy :as sensor] beacon]]
  (let [d (manhattan-distance sensor beacon)]
    (merge (into {} (map-indexed (fn [i y]
                                [y [(- sx i) (+ sx i)]])
                              (range (- sy d) (inc sy))))
           (into {} (map-indexed (fn [i y]
                               [y [(- sx i) (+ sx i)]])
                             (range (+ sy d) sy -1))))))

(defn find-beacon-position* [sensor-report y-range x-range]
  (when-let [[y xs] (->> (reduce
                          cut-ranges
                          (into {} (map (fn [y] [y [x-range]])
                                        (range (first y-range) (inc (second y-range)))))
                          (map known-ranges sensor-report))
                         (filter (comp seq val))
                         first)]
    [y (ffirst xs)]))

(defn find-beacon-position [sensor-report]
  (loop [ranges (for [y-range (partition 2 1 (range 0 4000001 1000000))
                      x-range (partition 2 1 (range 0 4000001 1000000))]
                  [y-range x-range])]
    (when-let [[y-range x-range] (first ranges)]
      (if-let [beacon-position (find-beacon-position* sensor-report y-range x-range)]
        beacon-position
        (recur (rest ranges))))))

(defn tuning-frequency [[y x]]
  (+ y (* 4000000 x)))

;; Elapsed time: 503515.814333 msecs
(defn part2 [input]
  (-> input
      parse-input
      find-beacon-position
      tuning-frequency))
