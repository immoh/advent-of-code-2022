(ns day16
  (:require
    clojure.math.combinatorics
    clojure.string))

(defn parse-input [input]
  (into {} (map
            (fn [line]
              (let [[from & to] (re-seq #"[A-Z]{2}" line)]
                [from {:to to
                       :rate (parse-long (re-find #"\d+" line))}]))
            (clojure.string/split-lines input))))

(defn find-shortest-path [start end neighbors-fn dist-fn]
  (loop [wip {start 0}
         visited #{start}]
    (when-let [[node dist] (first (sort-by val wip))]
      (if (= node end)
        dist
        (let [neighbors (remove visited (neighbors-fn node))]
          (recur (merge-with min
                             (dissoc wip node)
                             (zipmap neighbors
                                     (map (fn [node2]
                                            (+ dist (dist-fn node node2)))
                                          neighbors)))
                 (conj visited node)))))))

(defn pos-valves-with-shortest-paths [world]
  (let [pos-valves (->> world
                        (filter (comp pos? :rate val))
                        (map key)
                        set)]
    (into {}
          (map (fn [from]
                 [from {:rate (get-in world [from :rate])
                        :to   (into {}
                                    (keep (fn [to]
                                            (when-let [d (find-shortest-path from
                                                                             to
                                                                             #(get-in world [% :to])
                                                                             (constantly 1))]
                                              [to d]))
                                          (disj pos-valves from)))}])
               (conj pos-valves "AA")))))

(defn calculate-pressure [world opened]
  (->> opened
       (map (fn [[valve t]]
              (* t (get-in world [valve :rate]))))
       (reduce +)))

(defn next-states [world {:keys [position time opened]}]
  (keep (fn [[to d]]
          (when (and (not (opened to))
                     (>= time (inc d)))
            {:position to
             :time (- time (inc d))
             :opened (assoc opened to (- time (inc d)))}))
        (get-in world [position :to])))

(declare find-max-pressure)

(defn find-max-pressure* [world {:keys [time opened] :as state}]
  (let [next-states (next-states world state)]
    (if (or (<= time 1)
            (= (count opened) (count world))
            (not (seq next-states)))
      (calculate-pressure world opened)
      (reduce max 0 (map (partial find-max-pressure world)
                         next-states)))))

(def find-max-pressure (memoize find-max-pressure*))

(defn part1 [input]
  (-> (parse-input input)
      (pos-valves-with-shortest-paths)
      (find-max-pressure {:position "AA"
                          :time     30
                          :opened   {}})))

;; Part 2

(defn partial-world [world target-valves]
  (->> (select-keys world (conj target-valves "AA"))
       (map (fn [[from {:keys [to rate]}]]
              [from {:to (select-keys to target-valves)
                     :rate rate}]))
       (into {})))

;; Elapsed time: 7269.184 msecs
(defn part2 [input]
  (let [world (-> input parse-input pos-valves-with-shortest-paths)]
    (->> (clojure.math.combinatorics/partitions (remove #{"AA"} (keys world)) :min 2 :max 2)
         (filter #(= 7 (count (first %))))                  ;; Lucky guess
         (pmap (fn [partitions]
                 (->> partitions
                      (pmap (fn [partition]
                             (find-max-pressure (partial-world world partition)
                                                 {:position "AA"
                                                  :time 26
                                                  :opened {}}))))))
         (map (partial apply +))
         (reduce max))))
