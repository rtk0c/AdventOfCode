(ns aoc2024.day20
  (:require [aoc2024.utils :refer [while-some cardinal-neighbors manhattan-neighbors print-2d-array]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;; Assume the maze doesn't have any 2-thick walls. Implying when robot uses wall
;; clipping, it can effectively only go through a single wall.

;; Floodfill from Start to get a distance field S[x,y]; do the same from End to
;; get E[x,y]. The regular cost is S[Sx,Sy] or E[Ex,Ey]. The cost for a cheat
;; for some (x,y) is S[x,y] + E[x',y'] + |x'-x| + |y'-y|, where (x',y') is any
;; point manhatten distance 2 from (x,y).

(defn grid-points [[grid w h]]
  (for [y (range h)
        x (range w)
        :let [i (+ x (* w y))]]
    [x y (aget grid i)]))

(defn print-grid [[grid w h]]
  (print-2d-array grid w h))

(defn- find-elm [g elm]
  (->> (grid-points g)
       (filter (fn [[_ _ cell]]
                 (= elm cell)))
       (first)))

(defn parse-input
  ([] (parse-input "inputs/day20.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (let [lines (vec (line-seq rdr))
           w (count (first lines))
           h (count lines)
           grid (->> (str/join lines)
                     (apply str)
                     (char-array))
           g [grid w h]]
       {:map g
        :start (find-elm g \S)
        :end (find-elm g \E)}))))

(defn grid-in-bound? [[_ w h] x y]
  (and (< -1 x w)
       (< -1 y h)))

(defn grid-at [[grid w h] x y]
  (aget grid (+ x (* w y))))

(defn grid-set [[grid w h] x y v]
  (aset grid (+ x (* w y)) v))

(defn floodfill [[grid w h] x0 y0]
  (let [dists (int-array (count grid) -1)
        stack (new java.util.ArrayDeque)
        idx (fn [x y] (+ x (* w y)))]
    (.add stack [x0 y0 0])
    (while-some [[x y d] (.poll stack)]
      (aset dists (idx x y) d)
      (doseq [[x' y'] (cardinal-neighbors x y)
              :let [i' (idx x' y')]]
        (when (and (< -1 x' w) (< -1 y' h)
                   (not= \# (aget grid i'))
                   (= -1 (aget dists i')))
          (.add stack [x' y' (+ d 1)]))))
    [dists w h]))

(defn- cheats-time-saves [g df reg-time cheat-length x y]
  (->> (manhattan-neighbors x y cheat-length)
       (filter (fn [[x' y']]
                 (and (grid-in-bound? g x' y')
                      (not= (grid-at g x' y') \#)
                      (>= (- reg-time
                             (+ (grid-at df x y)
                                (- reg-time (grid-at df x' y'))
                                (abs (- x x'))
                                (abs (- y y'))))
                          100))))
       (count)))

(defn count-useful-cheats [{[_ w h :as g] :map [x0 y0] :start [xf yf] :end} cheat-length]
  (let [df (floodfill g x0 y0)
        reg-time (grid-at df xf yf)]
    (->> (for [y (range h)
               x (range w)
               :when (not= (grid-at g x y) \#)]
           (cheats-time-saves g df reg-time cheat-length x y))
         (reduce +))))

(defn part1 [input]
  (count-useful-cheats input 2))

;; This takes a while... about 1 minute on my machine. It should only be
;; O(whn^2) where w/h are the size of the grid, and n being 20 here. I suspect
;; the slowdown comes from lack of proper annotation causing lots of reflection
;; for type checking needs to happen
(defn part2 [input]
  (count-useful-cheats input 20))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)
     (part2 input)]))
