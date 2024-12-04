(ns aoc2024.main)

(defn- print-solutions [lst]
  (println  "format: <part1> | <part2>")
  (doseq [[idx [p1 p2]] (map-indexed vector lst)]
    (printf "day %02d: %d | %d\n" (+ idx 1) p1 p2)))

(defn main []
  (print-solutions
   [(aoc2024.day01/solve)
    (aoc2024.day02/solve)
    (aoc2024.day03/solve)]))
