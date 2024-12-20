(ns aoc2024.day09
  (:require [clojure.string :as str]))

(import aoc2024.day09.Day09)

(defn parse-input
  ([] (parse-input "inputs/day09.txt"))
  ([file]
   (map #(- (int %) (int \0))
        (str/trim (slurp file)))))

(defn part1 [input]
  (Day09/solvePart1 (int-array input)))

(defn part2 [input]
  ;; TODO this doesn't work on the "naughty" input?
  ;; https://old.reddit.com/r/adventofcode/comments/1haauty/2024_day_9_part_2_bonus_test_case_that_might_make/
  ;;
  ;; expected 97898222299196
  ;; actual   21034411748353
  (Day09/solvePart2 (int-array input)))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)
     (part2 input)]))
