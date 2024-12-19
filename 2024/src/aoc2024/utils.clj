(ns aoc2024.utils
  (:require [clojure.string :as str]))

(defmacro whilex
  [condition & body]
  `(loop []
     (if (not ~condition)
       nil
       (let [res# (do ~@body)]
         (if (reduced? res#)
           (deref res#)
           (recur))))))

(defn remove-prefix [s prefix]
  (if (str/starts-with? s prefix)
    (subs s (count prefix))
    s))

(defn remove-suffix [s suffix]
  (if (str/ends-with? s suffix)
    (subs s 0 (- (count s) (count suffix)))
    s))

(defn map2 [f coll]
  (map #(map f %) coll))

(defn iterate-til-reduced
  [f x]
  (loop [v x]
    (if (reduced? v)
      (deref v)
      (recur (f v)))))

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

(defn- a*-reconstruct-path [predcessors goal]
  (loop [path (transient [])
         pos goal]
    (if-let [prev-pos (.get predcessors pos)]
      (recur (conj! path pos)
             prev-pos)
      (persistent! (conj! path pos)))))

(defn a* [fneigh fcost fheuristic pos0 posf]
  (let [pq (new java.util.PriorityQueue)
        costs (new java.util.HashMap)
        predcessor (new java.util.HashMap)]
    (.offer pq [0 pos0])
    (.put costs pos0 0)
    (whilex (not (.isEmpty pq))
      (let [[_ pos] (.remove pq)
            cost (.get costs pos)]
        (if (= pos posf)
          (reduced (a*-reconstruct-path predcessor pos))
          (doseq [pos' (fneigh pos)
                  :let [cost' (+ cost (fcost pos pos'))]]
            (when (< cost' (.getOrDefault costs pos' Integer/MAX_VALUE))
              (.put costs pos' cost')
              (.put predcessor pos' pos)
              (.offer pq [(+ cost' (fheuristic pos')) pos']))))))))
