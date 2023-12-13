(ns aoc2023.day-1)

(def input (clojure.string/split-lines (slurp "resources/day-1.txt")))

(defn extract-digits [s] (re-seq #"\d" s))

(defn line-to-cal-val [l]
  (let [digits (extract-digits l)]
    (Integer/parseInt (str (first digits) (last digits)))))

; part 1
(reduce + (map line-to-cal-val input))

(defn word-to-digit [w]
  (let [digit-words ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
        digit-map (into {} (map-indexed #(vector %2 (str (+ 1 %1))) digit-words))]
       (or (get digit-map w) w)))

(defn extract-digits-or-words [s] (map last (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))" s)))

(defn line-to-cal-val-2 [l]
  (let [digits-or-words (extract-digits-or-words l)
        fst (first digits-or-words)
        lst (last digits-or-words)]
    (Integer/parseInt (str (word-to-digit fst) (word-to-digit lst)))))

; part 2
(reduce + (map line-to-cal-val-2 input))

