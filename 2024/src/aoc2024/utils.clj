(ns aoc2024.utils
  (:require [clojure.string :as str]))

(def pow10table
  "At index i contains value 10^(i+1)"
  (mapv #(long (Math/pow 10 %)) (range 1 18)))

(defmacro whilex
  [condition & body]
  `(loop []
     (if (not ~condition)
       nil
       (let [res# (do ~@body)]
         (if (reduced? res#)
           (deref res#)
           (recur))))))

(defmacro while-some
  [[var expr] & body]
  `(loop []
     (if-some [~var ~expr]
       (do ~@body
           (recur))
       nil)))

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

(defn conj-list!
  "Same as conj! but automatically creates a new transient list with x if 'coll' is nil."
  [coll x]
  (if coll
    (conj! coll x)
    (transient [x])))

(defn construct-graph
  "Construct a graph as a hashmap of vertices to hashset of edges. Take f which
  inserts edge into the hashmap. If 'f' is not provided, assume 'edges' is a seq
  of 2-element seqs containing from-vertex and to-vertex."
  ([f edges]
   (-> (reduce f (transient {}) edges)
       (persistent!)             ;; outer {vert -> #{edges}} map
       (update-vals persistent!) ;; each inner #{edges}
       ))
  ([edges]
   (construct-graph
    (fn [g [from to]] (update! g from #(conj-set! % to)))
    edges)))

;; 'g' here can be either maps or functions returning seqs. As long as it's a
;; callable that produces a seq of vertices leading from the current.

(defn valid-topo-order?
  "Check if 'topo' is a valid topological ordering for the graph 'g'."
  [g topo]
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

(defn topo-sort
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

(defn cardinal-neighbors
  "Return the (x,y) positions left, top, down, right of the given position. Same
  as `manhattan-neighbors` with distance 1."
  [x y]
  [[(+ x 1) y]
   [x (+ y 1)]
   [x (- y 1)]
   [(- x 1) y]])

(defn rangeii
  "Inclusive-inclusive version of `range`."
  ([end] (range (+ end 1)))
  ([start end] (range start (+ end 1)))
  ([start end step] (range start (+ end 1) step)))

(defn rangeii-symmetric [d]
  (rangeii (- d) d))

(defn manhattan-neighbors [x y d]
  (for [dx (rangeii-symmetric d)
        dy (rangeii-symmetric (- d (abs dx)))
        :when (or (not= dx 0) (not= dy 0))]
    [(+ x dx) (+ y dy)]))

(defn- a*-reconstruct-path [preds vgoal]
  (loop [path (transient [])
         v vgoal]
    (if-let [prev-v (.get preds v)]
      (recur (conj! path v)
             prev-v)
      (persistent! (conj! path v)))))

(defn a* [g fcost fheuristic v0 vf]
  (let [frontier (new java.util.PriorityQueue)
        costs (new java.util.HashMap)
        preds (new java.util.HashMap)]
    (.offer frontier [0 v0])
    (.put costs v0 0)
    (whilex (not (.isEmpty frontier))
      (let [[_ v] (.remove frontier)
            cost (.get costs v)]
        (if (= v vf)
          (reduced (a*-reconstruct-path preds v))
          (doseq [v' (g v)
                  :let [cost' (+ cost (fcost v v'))]]
            (when (< cost' (.getOrDefault costs v' Integer/MAX_VALUE))
              (.put costs v' cost')
              (.put preds v' v)
              (.offer frontier [(+ cost' (fheuristic v')) v']))))))))

(defn print-array [arr]
  (println (seq arr)))

(defn print-2d-array [arr w h]
  (doseq [y (range h)]
    (doseq [x (range w)]
      (print (aget arr (+ x (* y w))) \space))
    (println)))

(defrecord Grid [grid ^long width ^long height])

(defn grid-in-bound? [^Grid g x y]
  (and (< -1 x (.width g))
       (< -1 y (.height g))))

(defn grid-at
  ([^Grid g x y]
   (aget (:grid g)
         (+ x (* (:width g) y))))
  ([^Grid g x y default]
   (if (grid-in-bound? g x y)
     (grid-at g x y)
     default)))

(defn grid-set [^Grid g x y v]
  (aset (:grid g)
        (+ x (* (:width g) y))
        v))

(defn grid-points [^Grid g]
  (let [grid (:grid g) w (:width g) h (:height g)]
    (for [y (range h)
          x (range w)]
      [x y (aget grid (+ x (* w y)))])))

(defn print-grid [^Grid g]
  (print-2d-array (:grid g) (:width g) (:height g)))
