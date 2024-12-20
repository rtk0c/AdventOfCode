(ns aoc2024.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(import aoc2024.day19.QndTrie)

(defn parse-input
  ([] (parse-input "inputs/day19.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (let [[towels _ & patterns] (doall (line-seq rdr))
           towels (str/split (str/trim towels) #", ")]
       {:towels towels :patterns patterns}))))

(defn- pa [arr]
  (println (seq arr)))

(defn- matches? [s i substr]
  (if (> (count substr)
         (- (count s) i))
    false
    (loop [j 0]
      (cond
        (= j (count substr)) true
        (not= (nth s (+ i j))
              (nth substr j)) false
        :else (recur (+ j 1))))))

(defn- pattern-makeable?
  ([^QndTrie tlas ^String pattern ^ints lut ^long i]
   (cond
     (>= i (count pattern)) true
     ;; LUT content: 0 -> not filled, 1/2 -> filled true/false
     (= 1 (aget lut i)) true
     (= 2 (aget lut i)) false
     :else
     (if (some #(when (matches? pattern i %)
                  (pattern-makeable? tlas pattern lut (+ i (count %))))
               (.prefixes tlas pattern i))
       (do
         (aset lut i 1)
         true)
       (do
         (aset lut i 2)
         false))))
  ([^QndTrie tlas ^String pattern]
   (let [lut (int-array (count pattern))]
     (pattern-makeable? tlas pattern lut 0))))

(defn part1 [{towels :towels patterns :patterns}]
  (let [tlas (new QndTrie (into-array String towels))]
    (->> patterns
         (map #(pattern-makeable? tlas %))
         (filter identity)
         (count))))
