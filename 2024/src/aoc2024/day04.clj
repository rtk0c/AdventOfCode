(ns aoc2024.day04
  (:require [clojure.string :as str]))

(defn parse-input []
  (let [fc (slurp "inputs/day04.txt")]
    {:dim (str/index-of fc \newline)
     :map (->> fc
               (filter #(not= % \newline))
               (apply str))}))

(defn- c-at [{dim :dim map :map} x y]
  (if (and (< -1 x dim)
           (< -1 y dim))
    (nth map (+ (* y dim) x))
    nil))

(defn- check-word-along [data word x y dx dy]
  (->> (range)
       (map #(c-at data (+ x (* % dx)) (+ y (* % dy))))
       (map = word)
       (every? identity)))

(defn- count-occurneces [data word x y]
  (->> (for [dy [-1 0 1]
             dx [-1 0 1]]
         (check-word-along data word x y dx dy))
       (filter identity)
       (count)))

(defn part1 [{dim :dim :as input}]
  (->> (for [y (range dim)
             x (range dim)]
         (count-occurneces input "XMAS" x y))
       (reduce + 0)))

(defn- permutes [s t a b]
  (or (and (= s a) (= t b))
      (and (= s b) (= t a))))

(defn- check-x-mas [data x y]
  (if (= \A (c-at data x y))
    (let [tl (c-at data (- x 1) (- y 1))
          br (c-at data (+ x 1) (+ y 1))
          bl (c-at data (- x 1) (+ y 1))
          tr (c-at data (+ x 1) (- y 1))]
      (and (permutes tl br \M \S)
           (permutes bl tr \M \S)))
    false))

(defn part2 [{dim :dim :as input}]
  (->> (for [y (range dim)
             x (range dim)]
         (check-x-mas input x y))
       (filter identity)
       (count)))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)
     (part2 input)]))
