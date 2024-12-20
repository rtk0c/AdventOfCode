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

(defn- pattern-arrangements
  ([^QndTrie tlas ^String pattern ^longs lut ^long i]
   (cond
     (>= i (count pattern)) 1
     (not= -1 (aget lut i)) (aget lut i)
     :else
     (let [res (->>
                (.prefixes tlas pattern i)
                (filter #(matches? pattern i %))
                (map #(pattern-arrangements tlas pattern lut (+ i (count %))))
                (reduce +))]
       (aset lut i res)
       res)))
  ([^QndTrie tlas ^String pattern]
   (let [lut (long-array (count pattern))]
     (java.util.Arrays/fill lut -1)
     (pattern-arrangements tlas pattern lut 0))))

(defn solve []
  (let [{towels :towels patterns :patterns} (parse-input)
        ;; Towel Look-up Accelaration Structure
        tlas (new QndTrie (into-array String towels))
        patterns (map #(pattern-arrangements tlas %)
                      patterns)]
    [(->> patterns (filter #(> % 0)) (count))
     (->> patterns (reduce +))]))
