(ns aoc2024.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; AoC problem defines operation order as left-associative.
;;
;; We use right-associative here (reverse the input order), so that `first` and
;; `rest` work better for undoing each operation.

(defn parse-input
  ([] (parse-input "inputs/day07.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (map (fn [line]
            (let [[tot _ & rst] (str/split line #"[^\d]")]
              [(Long/parseLong tot)
               (reverse (map #(Long/parseLong %) rst))]))
          (doall (line-seq rdr))))))

(defn- solvable? [ops total opers]
  (let [c (first opers)
        rst (rest opers)]
    (if (empty? rst)
      (= total c)
      (->> ops
           (map #(% total c))
           (filter some?)
           (map #(solvable? ops % rst))
           (some identity)))))

(defn- count-solvable [ops input]
  (->> input
       (filter (fn [[tot opers]] (solvable? ops tot opers)))
       (map first)
       (reduce +)))

(defn part1 [input]
  (count-solvable [- /] input))

(def pow10table
  (mapv #(long (Math/pow 10 %)) (range 1 18)))

;; oops, wrong function
(defn- ||
  "Concat two numbers as decimal strings."
  [a b]
  (let [b-digits (int (Math/log10 b))
        shift (nth pow10table b-digits)]
    (long (+ (* shift a) b))))

(defn- |-
  "Break away the suffix b from a as a decimal string."
  [a b]
  (let [b-digits (int (Math/log10 b)) ;; digits "0-indexed", e.g. 12 => 1, 123 => 2
        mask (nth pow10table b-digits)]
    (if (= b (rem a mask))
      (quot a mask)
      nil)))

(defn part2 [input]
  (count-solvable [- / |-] input))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)
     (part2 input)]))
