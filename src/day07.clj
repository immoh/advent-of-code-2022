(ns day07
  (:require
   clojure.string))

(defn parse-line [{:keys [pwd] :as ctx} [s1 s2 s3]]
  (case s1
    "$" (case s2
          "cd" (case s3
                 "/" (assoc ctx :pwd [])
                 ".." (update ctx :pwd butlast)
                 (update ctx :pwd concat [s3]))
          ctx)
    "dir" ctx
    (assoc-in ctx (vec (concat [:files] pwd [s2])) (parse-long s1))))

(defn parse-file-tree [input]
  (->> input
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))
       (reduce parse-line {})
       :files))

(defn size [file-tree]
  (->> file-tree
       (tree-seq map? vals)
       (remove map?)
       (reduce +)))

(defn dir-sizes [input]
  (->> input
       parse-file-tree
       (tree-seq map? vals)
       (filter map?)
       (map size)))

(defn part1 [input]
  (->> input
       dir-sizes
       (filter #(<= % 100000))
       (reduce +)))

(defn part2 [input]
  (let [dir-sizes (dir-sizes input)
        to-delete (- (first dir-sizes) 40000000)]
    (->> dir-sizes
         (filter #(>= % to-delete))
         (reduce min))))
