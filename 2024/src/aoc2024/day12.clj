(ns aoc2024.day12
  (:require [aoc2024.utils :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(import aoc2024.utils.Grid)

;; Pick any unvisited plot. Floodfill to count area (standard issue) and
;; perimeter (for each plot in the floodfill, count the number of not-same
;; neighbors, add up). Repeat until no plot is unvisited.
;;
;; NOTE: labels are reused for disconnected spanning forests, e.g. there exists
;; multiple regions of R (or I, or O, or Q, ...)

(defn parse-input
  (^Grid [] (parse-input "inputs/day12.txt"))
  (^Grid [file]
   (with-open [rdr (io/reader file)]
     (let [lines (vec (line-seq rdr))]
       (->Grid (->> (str/join lines)
                    (apply str)
                    (char-array))
               (count (first lines))
               (count lines))))))

(defn floodfill [^Grid g x0 y0]
  (let [label (grid-at g x0 y0)
        visited-label (Character/toLowerCase label) ;; assume all region labels are captial letters
        q (new java.util.ArrayDeque)]
    (if (= label visited-label)
      nil
      (with-local-vars [area 0, peri 0]
        (grid-set g x0 y0 visited-label)
        (.add q [x0 y0])
        (while-some [[x y] (.poll q)]
          (var-set area (inc @area))
          (doseq [[x' y'] (cardinal-neighbors x y)]
            (if (and (grid-in-bound? g x' y')
                     (= (grid-at g x' y') label))
              (do
                (grid-set g x' y' visited-label)
                (.add q [x' y']))
              (when (or (not (grid-in-bound? g x' y'))
                        (not= (grid-at g x' y') visited-label))
                (var-set peri (inc @peri))))))
        [@area @peri]))))

(defn part1 [^Grid g]
  (let [w (:width g) h (:height g)]
    (->> (for [y (range h)
               x (range w)]
           (floodfill g x y))
         (filter some?)
         (map (fn [[area peri]]
                (* area peri)))
         (reduce +))))
