(ns aoc2024.main)

(defn- p [dayn [p1 p2]]
  (printf "day %02d: " dayn)
  (println p1 "|" p2))

(defn main []
  (p 1 (aoc2024.day01/solve))
  (p 2 (aoc2024.day02/solve))
  (p 3 (aoc2024.day03/solve))
  (p 4 (aoc2024.day04/solve))
  (p 5 (aoc2024.day05/solve))
  (p 7 (aoc2024.day07/solve))
  (p 9 (aoc2024.day09/solve))
  (p 14 (aoc2024.day14/solve))
  (p 17 (aoc2024.day17/solve))
  (p 18 (aoc2024.day18/solve))
  (p 19 (aoc2024.day19/solve)))
