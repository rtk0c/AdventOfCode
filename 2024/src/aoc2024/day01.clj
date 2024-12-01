(ns aoc2024.day01
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input []
  ;; Bleh
  (let [left-list (transient [])
        right-list (transient [])]
    (with-open [rdr (io/reader "inputs/day01.txt")]
      (doseq [line (line-seq rdr)]
        (let [left (subs line 0 (str/index-of line \space))
              right (subs line (+ 1 (str/last-index-of line \space)))]
          (conj! left-list (Integer/parseInt left))
          (conj! right-list (Integer/parseInt right)))))
    [(persistent! left-list) (persistent! right-list)]))

(defn part1 [[llist rlist]]
  (let [llist (sort llist)
        rlist (sort rlist)]
    (reduce +
            (map #(abs (- %1 %2)) llist rlist))))

(defn- count-occurences [lst]
  (persistent!
   (reduce (fn [tbl e]
             (assoc! tbl e (+ 1 (get tbl e 0))))
           (transient {})
           lst)))

(defn part2 [[llist rlist]]
  (let [rtbl (count-occurences rlist)]
    (reduce + (map #(* % (get rtbl % 0)) llist))))

(defn solve []
  (let [input (parse-input)]
    (part1 input)
    (part2 input)))
