(ns day-4)

(use '[clojure.string :only [split-lines]])

(def input (split-lines (slurp "resources/day-4.txt")))

(defn contains-s? [l s]
  (not (nil? (some #(= s %1) l))))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn partition-at [l pred]
  (loop [l' l
         acc []
         acc' []]
    (if (empty? l')
      (conj acc acc')
      (let [head (first l')
            tail (rest l')]
        (if (pred head)
          (recur tail (conj acc acc') [])
          (recur tail acc (conj acc' head)))))))

(defn read-card [l]
  (let [numbers (re-seq #"\d+|\|" l)
        [[id & left] right] (partition-at numbers #(= "|" %1))]
    {:id (Integer/parseInt id) :left (into [] left) :right right}))

(defn winning-numbers [{l :left r :right}]
  (filter #(contains-s? l %1) r))

;; part 1

(defn score [l]
  (if (empty? l)
    0
    (exp 2 (- (count l) 1))))

(reduce + 0
        (map (comp score winning-numbers read-card)
             input))

;; part 2

(defn copies-won [cards card]
  (for [c cards
        :let [n (count (winning-numbers card))]
        :when (< (:id card) (:id c) (+ n (:id card) 1))]
    c))

; StackOverflowError LUL
;(defn cards-won [cards]
;  (letfn [(cards-won' [[c & cs] acc]
;            (let [copies (copies-won cards c)]
;              (if (empty? copies)
;                (concat acc c)
;                (cards-won' cs (concat acc (cards-won' copies copies))))))] 
;  (cards-won' cards cards)))

(defn card-score [wt ids]
  (reduce
   (fn [acc i]
     (let [copies (get wt i)]
       (if (empty? copies)
         (+ acc 1)
         (+ acc 1 (card-score wt copies)))))
   0
   ids))

(defn winning-table [cards]
  (into {} (map #(vector (:id %1) (map :id (copies-won cards %1))) cards)))

(def wt (winning-table (map read-card input)))

(card-score wt (map (comp :id read-card) input))

