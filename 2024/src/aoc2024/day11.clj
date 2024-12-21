(ns aoc2024.day11
  (:require [aoc2024.utils :refer [pow10table]]
            [clojure.string :as str]))

(defn parse-input
  ([] (parse-input "inputs/day11.txt"))
  ([file]
   (map #(Long/parseLong %)
        (str/split (str/trim (slurp file)) #" "))))

(defn- digits [^long x]
  (+ 1 (long (Math/log10 x))))

(defn- chop [^long x]
  (let [mask10 (nth pow10table
                    (- (/ (digits x) 2) 1))]
    [(quot x mask10) (rem x mask10)]))

(defn- blink-once [xs]
  (mapcat
   (fn [x]
     (cond
       (= x 0) [1]
       (even? (digits x)) (chop x)
       :else [(* x 2024)]))
   xs))

(defn- blink-n [xs n]
  (nth (iterate blink-once xs) n))

(defn part1 [input]
  (count (blink-n input 25)))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)]))
