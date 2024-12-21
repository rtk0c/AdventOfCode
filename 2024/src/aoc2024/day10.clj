(ns aoc2024.day10
  (:require [aoc2024.utils :refer [cardinal-neighbors]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input
  ([] (parse-input "inputs/day10.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (let [lines (doall (line-seq rdr))
           l0 (first lines)]
       {:width (count l0)
        :height (count lines)
        :map (->> (str/join lines)
                  (mapv #(- (int %) (int \0)))
                  (int-array))}))))

(def max-h 9)

(defn part1 [{width :width height :height heightmap :map}]
  (letfn [(idx [x y]
            (+ x (* width y)))
          (f [x y]
            (let [i (idx x y)
                  h (aget heightmap i)]
              (if (= h 9)
                [[x y]]
                (->> (cardinal-neighbors x y)
                     (filter (fn [[x y]]
                               (and (< -1 x width)
                                    (< -1 y height)
                                    (= (+ h 1)
                                       (aget heightmap (idx x y))))))
                     (mapcat (fn [[x y]]
                               (f x y)))))))]
    (->> (for [y (range height)
               x (range width)
               :when (= 0 (aget heightmap (idx x y)))]
           (count (set (f x y))))
         (reduce +))))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)]))
