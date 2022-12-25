(ns day25
  (:require
    clojure.math
    clojure.set
    clojure.string))

(defn parse-input [input]
  (clojure.string/split-lines input))

(def char->dec {\2 2
                \1 1
                \0 0
                \- -1
                \= -2})

(def dec->char (clojure.set/map-invert char->dec))

(defn pow [x n]
  (long (clojure.math/pow x n)))

(defn snafu->dec [n]
  (reduce +
          (map
            (fn [c n]
              (* (char->dec c) (pow 5 n)))
            (reverse n)
            (range))))

(defn dec->quinary [n]
  (loop [i 0
         cs []
         remaining n]
    (if (pos? remaining)
      (let [c (/ (mod remaining (pow 5 (inc i))) (pow 5 i))]
        (recur (inc i)
               (conj cs c)
               (- remaining (* c (pow 5 i)))))
      cs)))

(defn dec->snafu [n]
  (let [quinary-m (zipmap (range) (dec->quinary n))
        snafu-m (reduce
                  (fn [quinary-m i]
                    (case (quinary-m i)
                      3 (-> quinary-m
                            (update (inc i) (fnil inc 0))
                            (assoc i -2))
                      4 (-> quinary-m
                            (update (inc i) (fnil inc 0))
                            (assoc i -1))
                      5 (-> quinary-m
                            (update (inc i) (fnil inc 0))
                            (assoc i 0))
                      quinary-m))
                  quinary-m
                  (range (count (keys quinary-m))))]
    (->> (keys snafu-m)
         (sort)
         (map (comp dec->char snafu-m))
         reverse
         (apply str))))

(defn part1 [input]
  (->> input
       parse-input
       (map snafu->dec)
       (reduce +)
       (dec->snafu)))
