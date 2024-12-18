(ns aoc2024.day09)
(import aoc2024.Day09)

(defn parse-input
  ([] (parse-input "inputs/day09.txt"))
  ([file]
   (map #(- (int %) (int \0))
        (slurp file))))

(defn part1 [input]
  (Day09/solvePart1 (int-array input)))
