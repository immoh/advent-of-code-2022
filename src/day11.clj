(ns day11
  (:require
    clojure.string))

(defn parse-input [input]
  (mapv (fn [monkey]
          (let [[_ items op test-divider if-true if-false] (clojure.string/split-lines monkey)]
            {:items        (mapv parse-long (re-seq #"\d+" items))
             :op           (take-last 2 (clojure.string/split op #" "))
             :test-divider (parse-long (last (clojure.string/split test-divider #" ")))
             :if-true      (parse-long (last (clojure.string/split if-true #" ")))
             :if-false     (parse-long (last (clojure.string/split if-false #" ")))}))
        (clojure.string/split input #"\n\n")))

(defn apply-op [[operand n :as instruction] x]
  (cond
    (= ["*" "old"] instruction) (* x x)
    (= "*" operand) (* x (parse-long n))
    (= "+" operand) (+ x (parse-long n))))

(defn round [monkeys]
  (reduce (fn [monkeys i]
            (let [{:keys [items op test-divider if-true if-false]} (get monkeys i)]
              (-> (reduce (fn [monkeys item]
                            (let [result (quot (apply-op op item) 3)
                                  target (if (zero? (mod result test-divider)) if-true if-false)]
                              (update-in monkeys [target :items] conj result)))
                          monkeys
                          items)
                  (assoc-in [i :items] [])
                  (update-in [i :inspections] (fnil + 0) (count items)))))
          monkeys
          (range (count monkeys))))


(defn monkey-business-level [monkeys round-fn rounds]
  (->> monkeys
       (iterate round-fn)
       (drop rounds)
       first
       (map :inspections)
       (sort >)
       (take 2)
       (reduce *)))

(defn part1 [input]
  (monkey-business-level (parse-input input) round 20))

;; Part 2

(defn apply-op-2 [instruction item dividers]
  (mapv (fn [x m]
          (mod (apply-op instruction x) m))
        item
        dividers))

(defn round-2 [monkeys]
  (reduce (fn [monkeys i]
            (let [{:keys [items op if-true if-false]} (get monkeys i)]
              (-> (reduce (fn [monkeys item]
                            (let [result (apply-op-2 op item (map :test-divider monkeys))
                                  target (if (zero? (get result i)) if-true if-false)]
                              (update-in monkeys [target :items] conj result)))
                          monkeys
                          items)
                  (assoc-in [i :items] [])
                  (update-in [i :inspections] (fnil + 0) (count items)))))
          monkeys
          (range (count monkeys))))

(defn items-modulo-test-dividers [monkeys]
  (mapv (fn [monkey]
          (update monkey
                  :items
                  (fn [items]
                    (mapv (fn [item]
                            (mapv (fn [{:keys [test-divider]}]
                                    (mod item test-divider))
                                  monkeys))
                          items))))
        monkeys))

(defn part2 [input]
  (monkey-business-level (->> (parse-input input)
                              (items-modulo-test-dividers))
                         round-2
                         10000))
