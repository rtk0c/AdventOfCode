(ns aoc2024.day05
  (:require [aoc2024.utils :refer [split-on map2 construct-graph valid-topo-order? topo-sort]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input
  ([]
   (parse-input "inputs/day05.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (let [[edges topos] (split-on empty? (doall (line-seq rdr)))]
       [(->> edges
             (map #(str/split % #"\|"))
             (map reverse) ;; problem gives '11|22' as edge (to,from), `construct-graph` expects (from,to)
             (map2 parse-long)
             (construct-graph))
        (->> topos
             (map #(str/split % #","))
             (map2 parse-long))]))))

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
