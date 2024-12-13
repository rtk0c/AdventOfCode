(ns aoc2024.utils)

(defn map2 [f coll]
  (map #(map f %) coll))

(defn reduce-i
  "Same as `reduce`, but additionally pass index to f. Differs from `reduce-kv`
  that this works on any seq."
  [f init coll]
  (loop [i 0, val init, coll coll]
    (if-let [c (first coll)]
      (recur (+ i 1) (f val i c) (rest coll))
      val)))

(defn reduce-2d
  "Reduces a 2d dataset (seq of seq of items)."
  [f init lines]
  (reduce-i
   (fn [val y line]
     (reduce-i
      (fn [val x item]
        (f val x y item))
      val
      line))
   init
   lines))

(defn split-on
  "Split seq into two partitions, separated by the element that made 'pred'
  true (which is discarded)."
  [pred coll]
  (let [[front back] (split-with (complement pred) coll)]
    [front (rest back)]))

(defn update!
  "Same as update but for transient data structures"
  [m k f]
  (assoc! m k (f (get m k))))


(defn conj-set!
  "Same as conj! but automatically creates a new transient set with x if 'coll' is nil."
  [coll x]
  (if coll
    (conj! coll x)
    (transient #{x})))

(defn construct-graph
  "Construct a graph as a hashmap of vertices to hashset of edges. Take f which
  inserts edge into the hashmap."
  [f edges]
  (-> (reduce f (transient {}) edges)
      (persistent!)             ;; outer {vert -> #{edges}} map
      (update-vals persistent!) ;; each inner #{edges}
      ))

(defn valid-topo-order? [g topo]
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

(defn topological-sort
  "Topologically sort a subgraph of 'g' containing only the vertices 'verts'."
  [g verts]
  (letfn [(collect [[order unseen :as state] v]
            (if (contains? unseen v)
              (let [[order' unseen']
                    (reduce #(collect %1 %2)
                            [order (disj unseen v)]
                            (get g v))]
                [(conj order' v) unseen'])
              state))]
    (loop [order []
           unseen (set verts)]
      (if-let [start (first unseen)]
        (let [[order' unseen'] (collect [order unseen] start)]
          (recur order' unseen'))
        order))))
