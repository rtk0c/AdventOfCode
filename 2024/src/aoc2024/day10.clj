(ns aoc2024.day10
  (:require [aoc2024.utils :refer [cardinal-neighbors print-2d-array]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input
  ([] (parse-input "inputs/day10.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (let [lines (vec (line-seq rdr))]
       {:width (count (first lines))
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

(defn part2 [{width :width height :height heightmap :map}]
  (let [scores (int-array (count heightmap))
        idx (fn [x y] (+ x (* width y)))
        calc-score
        (fn [h x y]
          (if (= h max-h)
            1
            (->> (for [[x' y'] (cardinal-neighbors x y)
                       :let [i (idx x' y')]]
                   (if (and (< -1 x' width)
                            (< -1 y' height)
                            (= (+ h 1) (aget heightmap i)))
                     (aget scores i) 0))
                 (reduce +))))]
    (doseq [h (reverse (range (+ 1 max-h)))
            y (range height)
            x (range width)
            :let [i (idx x y)]]
      (when (= h (aget heightmap i))
        (aset scores i (calc-score h x y))))
    ;(print-2d-array heightmap width height)
    ;(println)
    ;(print-2d-array scores width height)
    (->> (range (count scores))
         (filter #(= 0 (aget heightmap %)))
         (map #(aget scores %))
         (reduce +))))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)
     (part2 input)]))
