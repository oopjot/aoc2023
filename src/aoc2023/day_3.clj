(ns aoc2023.day-3)

(use '[clojure.string :only [split-lines index-of]])

(def input (split-lines (slurp "resources/day-3.txt")))

(defn is-symbol? [c]
  (if (nil? c) false
  (boolean (re-find #"[^0-9a-zA-Z\.]" (str c)))))

(defn neighbors [point] (map (partial mapv + point) [[-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1]]))

(defn number-positions [l res i]
  (let [next-n (re-find #"\d+" l)]
    (if (nil? next-n) res
      (let [pos (index-of l next-n)
            len (+ pos (count next-n))
            tail (apply str (drop len l))
            start-pos (+ pos i)
            end-pos (+ pos i (count next-n))
            item (apply vector (Integer/parseInt next-n) (range start-pos end-pos))]
        (number-positions tail (conj res item) (+ i len))))))


(defn adjacent [board point pred]
  (some identity (map (fn [[x y]] 
               (pred (str (nth (nth board x []) y "."))))
             (neighbors point))))

(defn symbol-adjacent? [board point] (adjacent board point is-symbol?))

;; part 1

(defn part-number-sum [board]
  (->>
  (for [[row x] (map vector board (range (count board)))
        [n & ys] (number-positions row [] 0)
        :let [pts (map #(vector x %1) ys)
              adj? (boolean (some identity (map #(symbol-adjacent? board %1) pts)))]
        :when adj?]
      n)
  (reduce + 0)))

(part-number-sum input)

;; part 2

(defn is-gear? [c] (= c "*"))

(defn gear-adjacent? [board point] (adjacent board point is-gear?))

(defn find-gears [board point]
  (for [[x y] (neighbors point)
        :when (is-gear? (str (nth (nth board x []) y ".")))]
    [x y]))

(defn gear-ratio-sum [board]
  (->>
    (for [[row x] (map vector board (range (count board)))
          [n & ys] (number-positions row [] 0)
          :let [pts (map #(vector x %1) ys)
                adj? (boolean (some identity (map #(gear-adjacent? board %1) pts)))]
          :when adj?
          [g] (map #(find-gears board %1) pts)]
      {g n})
    (filter #(not (contains? %1 nil)))
    (apply merge-with (comp distinct flatten vector))
    (vals)
    (filter #(and (seq? %1) (= (count %1) 2)))
    (reduce (fn [acc, [a b]] (+ acc (* a b))) 0)))


(gear-ratio-sum input)
