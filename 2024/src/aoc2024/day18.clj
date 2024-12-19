(ns aoc2024.day18
  (:require [clojure.java.io :as io]
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

(defn- apply-obstable [grid width [x y]]
  (assoc grid
         (+ (* y width) x)
         \#))

(defmacro whilex
  [condition & body]
  `(loop []
     (if (not ~condition)
       nil
       (let [res# (do ~@body)]
         (if (reduced? res#)
           (deref res#)
           (recur))))))

(defn- iterate-til-reduced
  [f x]
  (loop [v x]
    (if (reduced? v)
      (deref v)
      (recur (f v)))))

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

(defn- solve-maze [input dim]
  (let [grid0 (vec (take (* dim dim) (repeat \.)))
        grid (reduce #(apply-obstable %1 dim %2) grid0 input)]
    (print-grid grid dim dim)
    (a-star grid dim dim
            (fn [x y] (+ (abs (- x (- dim 1)))
                         (abs (- y (- dim 1)))))
            (fn [x y] (and
                       (< -1 x dim) (< -1 y dim)
                       (= (get grid (+ x (* dim y))) \.)))
            0 0
            (- dim 1) (- dim 1))))

(defn- print-grid [grid width height]
  (doseq [y (range 0 height)]
    (println (subvec grid
                     (* y width)
                     (* (+ 1 y) width)))))

(defn part1 [input]
  (- (count (solve-maze (take 1024 input) 71))
     1))
