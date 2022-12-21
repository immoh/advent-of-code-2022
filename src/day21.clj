(ns day21
  (:require
    clojure.string))

(def s->f {"*" *
           "+" +
           "-" -
           "/" /})

(defn parse-input [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (let [[name job] (clojure.string/split line #": ")]
                [name (let [[p1 p2 p3 :as job-params] (clojure.string/split job #" ")]
                        (if (= 1 (count job-params))
                          [:int (parse-long p1)]
                          [:op (s->f p2) p1 p3]))])))
       (into {})))

(defn evaluate [monkeys [t a1 & args]]
  (case t
    :int a1
    :op (apply a1 (map #(evaluate monkeys (monkeys %)) args))))

(defn part1 [input]
  (let [monkeys (parse-input input)]
    (evaluate monkeys (monkeys "root"))))

;; Part 2

(defn substitute [monkeys [t a1 & args]]
  (case t
    :int a1
    :op (let [[v1 v2] (map #(substitute monkeys (monkeys %)) args)]
          (cond
            (and (int? v1) (int? v2))
            (a1 v1 v2)

            (int? v1)
            (let [[_ x p] v2]
              (condp = a1
                * [:humn (* x v1) (* p v1)]
                + [:humn x (+ p v1)]
                - [:humn (- x) (- v1 p)]))

            (int? v2)
            (let [[_ x p] v1]
              (condp = a1
                * [:humn (* x v2) (* p v2)]
                + [:humn x (+ p v2)]
                - [:humn x (- p v2)]
                / [:humn (/ x v2) (/ p v2)]))))))

(defn part2 [input]
  (let [monkeys (-> (parse-input input) (assoc "humn" [:humn 1 0]))
        [_ x p] (substitute monkeys (into [:op -] (drop 2 (monkeys "root"))))]
    (/ (- p) x)))
