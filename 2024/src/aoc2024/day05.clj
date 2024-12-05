(ns aoc2024.day05
  (:require [aoc2024.utils :refer [split-on map2 update! conj-set! construct-graph valid-topo-order? topological-sort]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;; Treat 11|22 orderings as edges in a dependency graph
(defn- construct-dep-graph [edges]
  (construct-graph (fn [g od]
                     (let [[dep of] (map #(Integer/parseInt %)
                                         (str/split od #"\|"))]
                       (update! g of #(conj-set! % dep))))
                   edges))

(defn parse-input
  ([]
   (parse-input "inputs/day05.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (let [[edges topos] (split-on empty? (doall (line-seq rdr)))]
       [(->> edges (construct-dep-graph))
        (->> topos
             (map #(str/split % #","))
             (map2 #(Integer/parseInt %)))]))))

(defn solve []
  (let [[g topos] (parse-input)
        [valids invalids] ((juxt filter remove)
                           #(valid-topo-order? g %) topos)
        sum-middle (fn [lst]
                     (->> lst
                          (map #(nth % (/ (count %) 2)))
                          (reduce +)))]
    [(->> valids
          (sum-middle))
     (->> invalids
          (map #(topological-sort g %))
          (sum-middle))]))
