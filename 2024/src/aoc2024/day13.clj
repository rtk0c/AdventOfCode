(ns aoc2024.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn split-at-n [n coll]
  (let [[curr rst] (split-at n coll)]
    (if (empty? rst)
      (list curr)
      (cons curr (lazy-seq (split-at-n n rst))))))

(defn- extract-nums [s]
  (map parse-long (re-seq #"\d+" s)))

(defn parse-input
  ([] (parse-input "inputs/day13.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (->>
      (doall (line-seq rdr))
      (split-at-n 4)
      (map (fn [[btn-a btn-b prize _]]
             (concat (extract-nums btn-a)
                     (extract-nums btn-b)
                     (extract-nums prize))))))))

;; { Ax a + Bx b = Px
;; { Ay a + By b = Py
;;
;; i.e. the linear system [A B]v = P
;; where A, B, P are 2d vectors

(defn- tokens [[ax ay bx by px py]]
  (let [det (- (* ax by) (* bx ay))]
    (if (= det 0)
      ;; non-invertible matrix, no solution
      nil
      ;; v = [A B]^-1 * P
      ;; v = 1/det[A B] * adj[A B] * P
      (let [inv00 (/ by det)
            inv01 (/ (- bx) det)
            inv10 (/ (- ay) det)
            inv11 (/ ax det)
            x (+ (* px inv00) (* py inv01))
            y (+ (* px inv10) (* py inv11))]
        (if (and (integer? x) (integer? y))
          (+ (* x 3) y)
          nil)))))

(defn all-tokens [input]
  (->> input
       (map tokens)
       (filter some?)
       (reduce +)))

(defn solve []
  (let [input (parse-input)]
    [(all-tokens input)
     (all-tokens (map (fn [[ax ay bx by px py]]
                        [ax ay bx by (+ 10000000000000 px) (+ 10000000000000 py)])
                      input))]))
