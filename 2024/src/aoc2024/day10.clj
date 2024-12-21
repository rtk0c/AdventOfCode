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

(defn- pa [arr]
  (println (seq arr)))

(defn print-array [a w h]
  (doseq [y (range h)]
    (doseq [x (range w)]
      (print (aget a (+ x (* y w))) \space))
    (println)))

(defn part1 [{width :width height :height heightmap :map}]
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
    (print-array heightmap width height)
    (println)
    (print-array scores width height)
    (->> (range (count scores))
         (filter #(= 0 (aget heightmap %)))
         (map #(aget scores %))
         (reduce +))))
