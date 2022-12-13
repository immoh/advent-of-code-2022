(ns day13
  (:require
   clojure.string))


(defn parse-input [input]
  (map #(mapv read-string (clojure.string/split-lines %)) (clojure.string/split input #"\n\n")))

(defn in-right-order? [[l r]]
  #_(prn :in-right-order? :l l :r r)
  (cond
    (and (int? l) (int? r) (< l r))
    :true

    (and (int? l) (int? r) (> l r))
    :false

    (and (sequential? l) (sequential? r))
    (loop [l l r r i 0]
      #_(prn :loop l r)
      (cond
        (and (seq l) (not (seq r))) :false
        (and (not (seq l)) (seq r)) :true
        (= :true (in-right-order? (map first [l r]))) :true
        (= :false (in-right-order? (map first [l r]))) :false
        (and (seq l) (seq r)) (recur (rest l) (rest r) (inc i))))

    (and (sequential? l) (int? r))
    (recur [l [r]])

    (and (int? l) (sequential? r))
    (recur [[l] r])))


(defn my-compare [l r]
  #_(prn :in-right-order? :l l :r r)
  (cond
    (and (int? l) (int? r) (< l r))
    1

    (and (int? l) (int? r) (> l r))
    -1

    (and (sequential? l) (sequential? r))
    (loop [l l r r i 0]
      #_(prn :loop l r)
      (cond
        (and (seq l) (not (seq r))) -1
        (and (not (seq l)) (seq r)) 1
        (= 1 (apply my-compare (map first [l r]))) 1
        (= -1 (apply my-compare (map first [l r]))) -1
        (and (seq l) (seq r)) (recur (rest l) (rest r) (inc i))))

    (and (sequential? l) (int? r))
    (recur l [r])

    (and (int? l) (sequential? r))
    (recur [l] r)))

(defn my-compare [l r]
  (prn :------)
  (prn l)
  (prn r)
  (cond
    (and (sequential? l) (int? r)) (recur l [r])
    (and (int? l) (sequential? r)) (recur [l] r)
    :else (compare l r)))

(defn part1 [input]
  (->> (parse-input input)
       (keep-indexed (fn [i x]
                       (when (= 1 (apply my-compare x))
                         (inc i))))
       (reduce +)))

;; Part 2

(defn part2 [input]
  (->> (parse-input input)
       (mapcat identity)
       (into [[[2]] [[6]]])
       (sort (fn [x y]
               (= :true (in-right-order? [x y]))))
       (keep-indexed (fn [i x]
                       (when (#{[[2]] [[6]]} x)
                         (inc i))))
       (reduce *)))


(assert (= 5208 (part1 input)))
(assert (= 25792 (part2 input)))
