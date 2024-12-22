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

;; Insight from https://ziggit.dev/t/aoc-2024-day-12/7288/2
;; Counting straight borders is the same as counting corners of a region

(defn floodfill [^Grid g x0 y0]
  (let [label (grid-at g x0 y0)
        visited-label (Character/toLowerCase label) ;; assume all region labels are captial letters
        q (new java.util.ArrayDeque)]
    (if (= label visited-label)
      nil
      (with-local-vars [area 0, peri 0, edges 0]
        (grid-set g x0 y0 visited-label)
        (.add q [x0 y0])
        (while-some [[x y] (.poll q)]
          (var-set area (inc @area))
          (doseq [[x' y'] (cardinal-neighbors x y)]
            (cond
              (= (grid-at g x' y' \u0000) label)
              (do
                (grid-set g x' y' visited-label)
                (.add q [x' y']))
              (not= (grid-at g x' y' \u0000) visited-label)
              (var-set peri (inc @peri))))
          (doseq [dy [-1 1]
                  dx [-1 1]
                  :let [corner (grid-at g (+ x dx) (+ y dy) \u0000)
                        c-= (or (= corner label) (= corner visited-label))
                        neigh-x (grid-at g (+ x dx) y \u0000)
                        neigh-y (grid-at g x (+ y dy) \u0000)
                        n-x-= (or (= neigh-x label) (= neigh-x visited-label))
                        n-y-= (or (= neigh-y label) (= neigh-y visited-label))]]
            ;; a corner cannot be both convex and concave, so this is safe
            (when (or (and (not n-x-=) (not n-y-=)) ;; convex corner
                      (and (not c-=) n-x-= n-y-=))  ;; concave corner
              (println x y "corner")
              (var-set edges (inc @edges)))))
        [@area @peri @edges]))))

(defn- part1-price [[area peri _]]
  (* area peri))

(defn- part2-price [[area _ edges]]
  (* area edges))

(defn solve- [^Grid g]
  (let [w (:width g) h (:height g)
        regions (->> (for [y (range h)
                           x (range w)]
                       (floodfill g x y))
                     (filter some?))]
    (doseq [[area peri edges] regions]
      (println area peri edges))
    [(->> regions (map part1-price) (reduce +))
     (->> regions (map part2-price) (reduce +))]))

(defn solve []
  (solve- (parse-input)))
