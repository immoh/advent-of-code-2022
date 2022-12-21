(ns day19
  (:require
    clojure.string))

(defn parse-input [input]
  (map (fn [line]
         (let [[id
                ore-ore
                clay-ore
                obsidian-ore obsidian-clay
                geode-ore geode-obsidian]
               (map parse-long (re-seq #"\d+" line))]
           {:id id
            :costs {:ore {:ore ore-ore}
                    :clay {:ore clay-ore}
                    :obsidian {:ore obsidian-ore :clay obsidian-clay}
                    :geode {:ore geode-ore :obsidian geode-obsidian}}
            :holdings {:ore 0
                       :clay 0
                       :obsidian 0
                       :geode 0}
            :robots {:ore 1
                     :clay 0
                     :obsidian 0
                     :geode 0}}))
       (clojure.string/split-lines input)))

(defn can-buy? [state resource]
  (every? (fn [[t n]] (<= n (get-in state [:holdings t]))) (get-in state [:costs resource])))

(defn buy [state resource]
  (-> state
      (update-in [:robots resource] inc)
      (update :holdings #(merge-with - % (get-in state [:costs resource])))))

(defn next-states [i state]
  (map (fn [state2]
         (update state2 :holdings #(merge-with + % (:robots state))))
       (cond
         (can-buy? state :geode)
         [(buy state :geode)]

         (and (can-buy? state :obsidian)
              (< (get-in state [:robots :obsidian]) (get-in state [:costs :geode :obsidian])))
         [(buy state :obsidian)]

         :else
         (concat [state]
                 (when (and (can-buy? state :clay)
                            (< (get-in state [:robots :clay]) (get-in state [:costs :obsidian :clay])))
                   [(buy state :clay)])
                 (when (and (can-buy? state :ore)
                            (< (get-in state [:robots :ore]) (reduce max (map #(get-in state [:costs % :ore])
                                                                              [:ore :clay :obsidian :geode]))))
                   [(buy state :ore)])))))

(defn tick [states i]
  (set (mapcat (partial next-states i) states)))

(defn max-geodes [minutes blueprint]
  (->> (reduce tick #{blueprint} (range minutes))
       (map #(get-in % [:holdings :geode]))
       (reduce max)))

(defn quality-level [minutes {:keys [id] :as blueprint}]
  (* id (max-geodes minutes blueprint)))

(defn part1 [input]
  (->> input
       parse-input
       (map (partial quality-level 24))
       (reduce +)))

(defn part2 [input]
  (->> input
       parse-input
       (take 3)
       (map (partial max-geodes 32))
       (reduce *)))
