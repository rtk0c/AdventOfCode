(ns aoc2024.day17
  (:require [aoc2024.utils :refer [remove-prefix]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(import aoc2024.day17.Processor)

(defn parse-input
  ([] (parse-input "inputs/day17.txt"))
  ([file]
   (with-open [rdr (io/reader file)]
     (let [[reg-a reg-b reg-c _ prog] (line-seq rdr)
           pi (fn [s] (Integer/parseInt (re-find #"\d+" s)))]
       {:reg-a (pi reg-a)
        :reg-b (pi reg-b)
        :reg-c (pi reg-c)
        :prog (->> (str/split (remove-prefix prog "Program: ") #",")
                   (map parse-long))}))))

(defn part1 [{a :reg-a b :reg-b c :reg-c prog :prog}]
  (let [pc (new Processor)]
    (set! (.-a pc) a)
    (set! (.-b pc) b)
    (set! (.-c pc) c)
    (str/join "," (.run pc (int-array prog)))))

(defn part2 [{prog :prog}]
  ;; Program: 2,4,1,2,7,5,4,5,1,3,5,5,0,3,3,0
  ;;
  ;; An idea: the program needs to be 16 long, and the program performs `adv 3`
  ;; every iteration producing an output. So A needs to be between 8^16 to 8^17
  ;;
  ;; Uh, that's just frankly ridiculously big of a search range, so no
  ;; TODO find clever way to reverse the (what's effectively) a hash function
  (let [prog-arr (int-array prog)]
    (->> (range 0 0) ;; ???? ????
         (map (fn [a]
                (let [pc (new Processor)]
                  (.reset pc a 0 0)
                  (every? identity (map = prog-arr (.run pc prog-arr))))))
         (filter identity)
         (first))))

(defn solve []
  (let [input (parse-input)]
    [(part1 input)
     nil]))
