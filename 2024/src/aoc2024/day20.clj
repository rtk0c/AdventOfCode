(ns aoc2024.day20
  (:require [aoc2024.utils :refer :all]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;; Avoid having to type out the full name every time.
;; Apparently this is still needed even if we do :refer :all in requires.
(import aoc2024.utils.Grid)

;; Assume the maze doesn't have any 2-thick walls. Implying when robot uses wall
;; clipping, it can effectively only go through a single wall.

;; Floodfill from Start to get a distance field S[x,y]; do the same from End to
;; get E[x,y]. The regular cost is S[Sx,Sy] or E[Ex,Ey]. The cost for a cheat
;; for some (x,y) is S[x,y] + E[x',y'] + |x'-x| + |y'-y|, where (x',y') is any
;; point manhatten distance 2 from (x,y).

(defn- find-elm [^Grid g elm]
  (->> (grid-points g)
       (filter (fn [[_ _ cell]]
                 (= elm cell)))
       (first)))

(defn parse-input
  (^Grid [] (parse-input "inputs/day20.txt"))
  (^Grid [file]
   (with-open [rdr (io/reader file)]
     (let [lines (vec (line-seq rdr))
           g (->Grid (->> (str/join lines)
                          (apply str)
                          (char-array))
                     (count (first lines))
                     (count lines))]
       {:maze g
        :start (find-elm g \S)
        :end (find-elm g \E)}))))

(defn floodfill [^Grid g x0 y0]
  (let [grid (:grid g) w (:width g) h (:height g)
        dists (int-array (alength grid) -1)
        q (new java.util.ArrayDeque)]
    (.add q [x0 y0 0])
    (while-some [[x y d] (.poll q)]
      (aset dists (+ x (* w y)) d)
      (doseq [[x' y'] (cardinal-neighbors x y)
              :let [i' (+ x' (* w y'))]]
        (when (and (< -1 x' w) (< -1 y' h)
                   (not= (aget grid i') \#)
                   (= (aget dists i') -1))
          (.add q [x' y' (+ d 1)]))))
    (->Grid dists w h)))

;; 'm' for Maze, the input maze
;; 'df' for Distance Field, distance from the start point

(defn- cheats-time-saves [^Grid m ^Grid df reg-time cheat-length x y]
  (->> (manhattan-neighbors x y cheat-length)
       (filter (fn [[x' y']]
                 (and (grid-in-bound? m x' y')
                      (not= (grid-at m x' y') \#)
                      (>= (- reg-time
                             (+ (grid-at df x y)
                                (- reg-time (grid-at df x' y'))
                                (abs (- x x'))
                                (abs (- y y'))))
                          100))))
       (count)))

(defn count-useful-cheats
  [^Grid m ^Grid df reg-time cheat-length]
  (->> (for [y (range (:height m))
             x (range (:width m))
             :when (not= (grid-at m x y) \#)]
         (cheats-time-saves m df reg-time cheat-length x y))
       (reduce +)))

(defn solve []
  (let [{^Grid m :maze [x0 y0] :start [xf yf] :end} (parse-input)
        df (floodfill m x0 y0)
        reg-time (grid-at df xf yf)]
    [(count-useful-cheats m df reg-time 2)
     ;; This takes a while... about 1 minute on my machine. It should only be
     ;; O(whn^2) where w/h are the size of the grid, and n being 20 here. I
     ;; suspect the slowdown comes from lack of proper annotation causing lots
     ;; of reflection for type checking needs to happen.
     (count-useful-cheats m df reg-time 20)]))
