(ns aoc2024.day03
  (:require [clojure.string :as str]))

(defn parse-input []
  ;; It seems like a mul(x,y) never spans across a line, so we don't have to
  ;; filter out the \n characters
  (map (fn [[_ x y]]
         [(Integer/parseInt x), (Integer/parseInt y)])
       (re-seq #"mul\((\d+),(\d+)\)" (slurp "inputs/day03.txt"))))

(defn part1 [input]
  (->> input
       (map #(apply * %))
       (reduce +)))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)
     0]))
