(ns aoc2024.day18
  (:require [aoc2024.utils :refer [a*]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input
  ([] (parse-input "inputs/day18.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (for [line (doall (line-seq rdr))]
       (let [i (str/index-of line \,)
             x (Integer/parseInt (subs line 0 i))
             y (Integer/parseInt (subs line (+ 1 i)))]
         [x y])))))

(defn- grid-add-obstable [grid width [x y]]
  (assoc grid
         (+ (* y width) x)
         \#))

(defn- grid-show-path [grid width height path]
  (persistent!
   (reduce (fn [grid [x y]]
             (assoc! grid (+ x (* width y)) \O))
           (transient grid)
           path)))

(defn- make-grid [width height fill]
  (vec (take (* width height) (repeat fill))))

(defn- print-grid [grid width height]
  (doseq [y (range 0 height)]
    (println (subvec grid
                     (* y width)
                     (* (+ 1 y) width)))))

(defn- cardinal-neighbors [x y]
  [[(+ x 1) y]
   [x (+ y 1)]
   [x (- y 1)]
   [(- x 1) y]])

(defn- solve-maze [grid width height]
  (let [xf (- width 1)
        yf (- height 1)]
    (a* (fn [[x y]]
          (filter
           (fn [[x y]]
             (and (< -1 x width) (< -1 y height)
                  (= \. (get grid (+ x (* width y))))))
           (cardinal-neighbors x y)))
        (fn [_ _] 1)
        (fn [[x y]] (+ (abs (- x xf))
                       (abs (- y yf))))
        [0 0]
        [xf yf])))

(defn part1 [input]
  (let [dim 71
        grid (reduce #(grid-add-obstable %1 dim %2)
                     (make-grid dim dim \.)
                     (take 1024 input))
        path (solve-maze grid dim dim)]
    (print-grid (grid-show-path grid dim dim path) dim dim)
    (- (count path) 1)))

(defn part2 [input]
  (let [dim 71]
    (loop [grid (reduce #(grid-add-obstable %1 dim %2)
                        (make-grid dim dim \.)
                        (take 1024 input))
           obstacles (drop 1024 input)
           path (set (solve-maze grid dim dim))]
      (let [obs (first obstacles)
            grid' (grid-add-obstable grid dim obs)]
        (if (contains? path obs)
          ;; Debris landed on path, recalculate
          (if-some [path' (solve-maze grid' dim dim)]
            ;; Still pathable
            (recur grid' (rest obstacles) (set path'))
            ;; Blocked, return this debris
            obs)
          ;; Debris landed outside current path, doesn't change anything
          (recur grid' (rest obstacles) path))))))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)
     (part2 input)]))
