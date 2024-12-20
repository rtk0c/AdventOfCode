(ns aoc2024.day14
  (:require [clojure.java.io :as io]))

;; The Mirror Trick:
;;
;; When simulating bouncing lights, it is perfectly ok to pretend the room is
;; duplicated infinitely many copies (mirrored along each edge) spanning the
;; whole R^2. And when light "reflects", it just goes into a duplicated copy of
;; the room. In the end, simply find the local room coordinate of the light ray.
;;
;; It's actually even simpler here, since we just want wrap-around: no mirroring
;; needed at all. Just take coordinates with wrap around.

(defn- wrap-coord [x cell]
  ;; We want `mod` instead of `rem`, because wraparound "local coordinate k"
  ;; should be "a positive k such that x = k + n*cell for some n". `rem` would
  ;; give us negative k for negative x.
  (mod x cell))

(defn parse-input
  ([] (parse-input "inputs/day14.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (for [line (doall (line-seq rdr))
           :let [nums (re-seq #"-?\d+" line)]]
       (map #(Long/parseLong %) nums)))))

(defn- fly-robot [[x y vx vy] t]
  [(+ x (* vx t))
   (+ y (* vy t))])

(defn- normalize-robot [[x y] width height]
  [(wrap-coord x width) (wrap-coord y height)])

(defn- which-quad [x y x-thres y-thres]
  (let [xi (cond (> x x-thres) 1 (< x x-thres) 0 :else nil)
        yi (cond (> y y-thres) 2 (< y y-thres) 0 :else nil)]
    (if (and xi yi)
      (+ xi yi)
      nil)))

(defn- calc-formation [robots width height t]
  (map #(-> (fly-robot % t)
            (normalize-robot width height))
       robots))

(defn- print-formation
  [robots width height]
  (let [m (char-array (* width height))]
    (java.util.Arrays/fill m \.)
    (doseq [[x y] robots]
      (aset m (+ x (* width y)) \#))
    (doseq [i (range 0 (* height width) width)]
      (doseq [j (range width)]
        (print (aget m (+ i j))))
      (println))))

(defn- calc-formations
  "Infinite seq of robot formations, presented as [<t> <f(t)>]"
  [robots width height skip]
  (map (fn [time robots]
         (let [t (+ time 1 skip)]
           [t, (calc-formation robots width height t)]))
       (range)
       (repeat robots)))

(defn part1 [input width height]
  (let [x50 (quot width 2) y50 (quot height 2)]
    (->>
     (calc-formation input width height 100)
     (reduce (fn [stats [x y]]
               (when-some [q (which-quad x y x50 y50)]
                 (aset stats q
                       (+ 1 (aget stats q))))
               stats)
             (int-array 4))
     (reduce * 1))))

;; Usage: just dump like 10000 snapshots into a file, and examine the file by hand
(defn save-robot-formations
  "Save the given list of formations to file. Get formations by running (->> (robot-formations ...) (take ...))"
  [width height formations file]
  (with-open [wtr (io/writer file)]
    (binding [*out* wtr]
      (doseq [[t robots] formations]
        (println "t = " t)
        (print-formation robots width height)
        (println)))))

(defn- continuous? [xs]
  (let [xs (sort xs)
        xs' (map - (rest xs) xs)]
    (> (count (filter #(= % 1) xs'))
       8))) ;; random constant that seems good

;; The christmas tree looks like a bunch of solid triangles stacked on top of
;; each other, surrounded by a frame. Located anywhere in the 101x103 grid.
;;
;; Try to detect it by looking for a lots of continuous stripes of robots.
;; Tested on personal data, does this in fact find the christmas tree.
(defn- likely-christmas-tree? [robots]
  (let [x-gp (-> (group-by first robots) (update-vals #(map second %)) (vals))
        y-gp (-> (group-by second robots) (update-vals #(map first %)) (vals))
        cont? (fn [xs]
                (> (->> xs (map continuous?)
                        (filter identity)
                        (count))
                   2))]
    (and (cont? x-gp)
         (cont? y-gp))))

(defn part2 [input width height search-upper-bound]
  (->>
   (robot-formations input width height 0)
   (take search-upper-bound)
   (filter #(likely-christmas-tree? (second %)))
   (first)))

(defn solve []
  (let [input (parse-input)]
    [(part1 input 101 103)
     ;; `part2` spits out [t, f t] and we just want t
     (first (part2 input 101 103 10000))]))
