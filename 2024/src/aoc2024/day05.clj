(ns aoc2024.day05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn- split-on
  "Same as split-with, but discards the element which made 'pred' true"
  [pred coll]
  (let [[front back] (split-with pred coll)]
    [front (rest back)]))

(defn parse-input
  ([]
   (parse-input "inputs/day05.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (->> (line-seq rdr)
          (doall)
          (split-on (complement empty?))))))

(defn- update!
  "Same as update but for transient data structures"
  [m k f]
  (assoc! m k (f (get m k))))

(defn- update-vals!
  "Same as update-vals but for transient maps"
  [m f]
  ())

(defn- conj-set!
  "Same as conj! but automatically creates a new transient set with x if 'coll' is nil."
  [coll x]
  (if coll
    (conj! coll x)
    (transient #{x})))

(defn- construct-graph [f edges]
  (-> (reduce f (transient {}) edges)
      (persistent!)             ;; outer {vert -> #{edges}} map
      (update-vals persistent!) ;; each inner #{edges}
      ))

(defn- construct-dep-graph [edges]
  (construct-graph (fn [g od]
                     (let [[dep of] (map #(Integer/parseInt %)
                                         (str/split od #"\|"))]
                       (update! g of #(conj-set! % dep))))
                   edges))

(defn- valid-topo-order? [g topo]
  (let [present-verts (set topo)]
    ;; returns false on false, and the 'seen'-set on true; too lazy to coerce
    ;; to a bool (but not lazy enough to not write this comment)
    (reduce (fn [seen vert]
              (if (every? #(or (not (contains? present-verts %))
                               (contains? seen %))
                          (get g vert))
                (conj! seen vert)
                (reduced false)))
            (transient #{})
            topo)))

(defn part1
  [[edges topos]] ;; treat 11|22 orderings as edges in a dependency graph
  (let [g (construct-dep-graph edges)
        topos (map #(map (fn [s] (Integer/parseInt s)) (str/split % #",")) topos)]
    (->>
     (filter #(valid-topo-order? g %) topos)
     (map #(nth % (/ (count %) 2)))
     (reduce +))))
