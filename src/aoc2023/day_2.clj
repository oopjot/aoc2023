(ns aoc2023.day-2)

(def input (clojure.string/split-lines (slurp "resources/day-2.txt")))

(def total-cubes {"red" 12
                  "green" 13
                  "blue" 14})

(defn game-possible [pairs]
  (let [reducer #(boolean (and %1 (<= (second %2) (get total-cubes (first %2)))))]
    (reduce reducer true pairs)))

(defn game-pairs [l]
  (let [parted (partition 2 (filter #(not (= "" %)) (clojure.string/split l #" |;|,|:")))
        game-id (Integer/parseInt (last (first parted)))]
    {:id game-id
     :pairs (map #(vector (second %1) (Integer/parseInt (first %1))) (rest parted))}))

(defn pairs-possible [pairs]
  (let [id (:id pairs)
        ps (:pairs pairs)]
    {:id id
     :possible (game-possible ps)}))

;; part 1
(reduce #(+ %1 (get %2 :id)) 0 (filter #(get %1 :possible) (map (comp pairs-possible game-pairs) input)))

;; part 2
(defn game-power [pairs]
  (let [grouped (group-by first pairs)
        reduced (map (fn [[_, v]] (last (sort (map second v)))) grouped)]
    (reduce * reduced)))

(reduce + (map (comp game-power :pairs game-pairs) input))

