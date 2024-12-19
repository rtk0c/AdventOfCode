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
       (map #(Integer/parseInt %) nums)))))

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

(defn part1 [input width height]
  (let [x50 (quot width 2) y50 (quot height 2)]
    (->>
     input
     (map #(-> (fly-robot % 100)
               (normalize-robot width height)))
     (reduce (fn [stats [x y]]
               (when-some [q (which-quad x y x50 y50)]
                 (aset stats q
                       (+ 1 (aget stats q))))
               stats)
             (int-array 4))
     (reduce * 1))))

(defn solve []
  (let [input (parse-input)]
    [(part1 input 101 103)]))
