(ns aoc2024.day09)

;; Disk Map data structure:
;; [[<id> <length>] <length> ...]
;; where elements are alternating file and void
;;
;; Voids may have length 0, files never have length 0

(defn- take-files
  "Extract only even numbered elements."
  [dm]
  (if-some [f (first dm)]
    (lazy-seq (cons f (take-files (rest (rest dm)))))))

(defn- take-voids
  "Extract only odd numbered elements."
  [dm]
  (take-files (rest dm)))

(defn- replace-voids [dm])

;; This is dumb
(defn- fill-void-with-files [res void-length files]
  ;; returns [<result> <#files consumed>]
  (loop [res res
         vlen void-length
         files' files
         num-consumed 0]
    (let [file (first files')
          fid (get file 0)
          flen (get file 1)]
      (cond
        (nil? file) (reduced [res num-consumed])
        (= vlen 0) [res files']
        (>= flen vlen) [(conj res [fid vlen])
                        (if (= flen vlen)
                          (+ num-consumed 1)
                          (num-consumed))]
        :else (recur (conj res [fid flen])
                     (- vlen flen)
                     (rest files')
                     (+ num-consumed 1))))))

(defn- replace-voids [dm]
  ;; TODO generate `files` by cutting off dm at a length
  (reduce-kv (fn [[res num-consumed] i elm]
               (if (vector? elm)
                 (conj res elm)
                 (fill-void-with-files res elm
                                       (drop num-consumed (reverse (take-files dm))))))
             [[] 0]
             dm))
