(ns aoc2024.day09
  (:require [clojure.string :as str]))
(import aoc2024.Day09)

(defn parse-input
  ([] (parse-input "inputs/day09.txt"))
  ([file]
   (map #(- (int %) (int \0))
        (str/trim (slurp file)))))

(defn part1 [input]
  (Day09/solvePart1 (int-array input)))

(defn part2 [input]
  (Day09/solvePart2 (int-array input)))

(defn solve []
  (let [input (parse-input)]
    [(part1 input) (part2 input)]))
