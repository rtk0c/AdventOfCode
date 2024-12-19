(ns aoc2024.day17
  (:require [aoc2024.utils :refer [remove-prefix]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(import aoc2024.day17.Processor)

(defn parse-input
  ([] (parse-input "inputs/day17.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (let [[reg-a reg-b reg-c _ prog] (line-seq rdr)
           pi (fn [s] (Integer/parseInt (re-find #"\d+" s)))]
       {:reg-a (pi reg-a)
        :reg-b (pi reg-b)
        :reg-c (pi reg-c)
        :prog (map #(Integer/parseInt %)
                   (str/split (remove-prefix prog "Program: ")
                              #","))}))))

(defn part1 [{a :reg-a b :reg-b c :reg-c prog :prog}]
  (let [pc (new Processor)]
    (set! (.-a pc) a)
    (set! (.-b pc) b)
    (set! (.-c pc) c)
    (str/join "," (.run pc (int-array prog)))))
