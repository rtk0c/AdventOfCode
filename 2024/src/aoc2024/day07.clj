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
        r (rest opers)]
    (if (empty? r)
      (= total c)
      (->> ops
           (map #(solvable? ops (% total c) r))
           (some identity)))))

(defn- count-solvable [ops input]
  (->> input
       (filter (fn [[tot opers]] (solvable? ops tot opers)))
       (map first)
       (reduce +)))

(defn part1 [input]
  (count-solvable [- /] input))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)
     0]))
