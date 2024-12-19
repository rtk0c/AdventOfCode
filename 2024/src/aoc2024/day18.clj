(ns aoc2024.day18
  (:require [aoc2024.utils :refer [whilex]]
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

(defn- reconstruct-path [predcessors goal]
  (loop [path (transient [])
         pos goal]
    (if-let [prev-pos (.get predcessors pos)]
      (recur (conj! path pos)
             prev-pos)
      (persistent! (conj! path pos)))))

(defn- a-star [grid width height fcost fallowed? x0 y0 xf yf]
  (let [pq (new java.util.PriorityQueue)
        costs (new java.util.HashMap)
        predcessor (new java.util.HashMap)
        pos0 [x0 y0]]
    (.offer pq [0 pos0])
    (.put costs pos0 0)
    (whilex (not (.isEmpty pq))
      (let [[_ [x y :as node]] (.remove pq)
            cost (.get costs node)]
        (if (and (= x xf) (= y yf))
          (reduced (reconstruct-path predcessor node))
          (doseq [[dx dy] [[-1 0] [0 1] [1 0] [0 -1]]
                  :when (fallowed? (+ x dx) (+ y dy))
                  :let [x' (+ x dx)
                        y' (+ y dy)
                        node' [x' y']
                        cost' (+ cost 1)]]
            (when (< cost' (.getOrDefault costs node' Integer/MAX_VALUE))
              (.put costs node' cost')
              (.put predcessor node' node)
              (.offer pq [(+ cost' (fcost x' y')) node']))))))))

(defn- make-grid [width height fill]
  (vec (take (* width height) (repeat fill))))

(defn- print-grid [grid width height]
  (doseq [y (range 0 height)]
    (println (subvec grid
                     (* y width)
                     (* (+ 1 y) width)))))

(defn- solve-maze [grid width height]
  (a-star grid width height
          (fn [x y] (+ (abs (- x (- width 1)))
                       (abs (- y (- height 1)))))
          (fn [x y] (and
                     (< -1 x width) (< -1 y height)
                     (= (get grid (+ x (* width y))) \.)))
          0 0
          (- width 1) (- height 1)))

(defn part1 [input]
  (let [dim 71
        grid (reduce #(grid-add-obstable %1 dim %2)
                     (make-grid dim dim \.)
                     (take 1024 input))]
    (- (count (solve-maze grid dim dim))
       1)))

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
