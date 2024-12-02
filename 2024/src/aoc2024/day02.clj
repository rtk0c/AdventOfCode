(ns aoc2024.day02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input []
  (with-open [rdr (io/reader "inputs/day02.txt")]
    ;; Alternative, (map (fn [line] (map ...)) ...) but that involves either
    ;; writing nested fn, or at least a #(), which is ugly.
    (doall (for [line (line-seq rdr)]
             (for [item (str/split line #" ")]
               (Integer/parseInt item))))))

(defn- sign-of [x]
  (if (< x 0) -1 1))

(defn- report-safe? [report]
  (let [deltas (map #(- %1 %2) report (rest report))]
    (and (apply = (map sign-of deltas))
         (every? #(<= 1 (abs %) 3) deltas))))

(defn part1 [input]
  (count (filter report-safe? input)))

(defn solve []
  (let [input (parse-input)]
    (part1 input)))
