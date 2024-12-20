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

(defn- pattern-makeable? [tlas pattern]
  (if (empty? pattern)
    true
    (some (fn [towel]
            (when (str/starts-with? pattern towel)
              (pattern-makeable? tlas (subs pattern (count towel)))))
          (.prefixes tlas pattern))))

(defn part1 [{towels :towels patterns :patterns}]
  (let [tlas (new QndTrie (into-array String towels))]
    (->> patterns
         (map #(do
                 (println %)
                 (pattern-makeable? tlas %)))
         (filter identity)
         (count))))
