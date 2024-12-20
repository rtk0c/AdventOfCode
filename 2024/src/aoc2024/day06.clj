(ns aoc2024.day06
  (:require [aoc2024.utils :as u]
            [clojure.java.io :as io]))

(defn parse-input
  ([]
   (parse-input "inputs/day06.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (u/reduce-2d
      (fn [[start pts-x pts-y :as pts] x y c]
        (case c
          \^ [[x y] pts-x pts-y]
          \# [start
              (update pts-x x #(conj % [x y]))
              (update pts-y y #(conj % [x y]))]
          pts))
      [{} {}]
      (line-seq rdr)))))

(defn part1 [input]
  )

(defn solve []
  (let [input (parse-input)]
    [(part1 input)
     0]))
