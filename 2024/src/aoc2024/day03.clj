(ns aoc2024.day03)

(defn parse-with-regex [file-content]
  ;; It seems like a mul(x,y) never spans across a line, so we don't have to
  ;; filter out the \n characters
  (map (fn [[_ x y]]
         [(Integer/parseInt x), (Integer/parseInt y)])
       (re-seq #"mul\((\d+),(\d+)\)" file-content)))

(defn part1 [input]
  (->> input
       (map #(apply * %))
       (reduce +)))

;; Part2 can be done with regex again, something like:
;;   `(mul\((\d+),(\d)+\)|do\(\)|don'\(\))`
;; But why not something more fun?

;; A very crude parser combinator system. No tokens, or rather a "token" is any
;; individual character in the input stream.
;;
;; Combinators return [success skipahead result] of bool, int, any respectively.
;; Indeed this is really wack, normally parser combinators does a full paser
;; over the input, but let's see what happens.

(defn- pc-word [word]
  (fn [offset s]
    (loop [i 0]
      (if (= i (count word))
        [true (+ offset i) word]
        (if (= (nth s (+ offset i))
               (nth word i))
          (recur (+ i 1))
          ;; HERE BE DRAGONS: We can skip ahead everything up to the mismatched
          ;; character, because our words of interest "mul" "do" "don't" all
          ;; don't have any repeated characters. If something like "ssxsss" is
          ;; wanted and we match across "ssxssx...", smart things needs to be
          ;; done to skip to not miss a potential solution starting at the
          ;; send "ssx...". e.g.. techniques found in KMP (or Boyer-Moore).
          (if (= (nth s (+ offset i))
                 (first word))
            [false (+ offset i) nil]
            [false (+ offset i 1) nil]))))))

(defn- pc-int [offset s]
  (loop [i offset]
    (if (and (< i (count s)) ;; needs bounds check, `Character/isDigit` doesn't take nil
            (Character/isDigit (nth s i)))
      (recur (+ i 1))
      (if (= i offset)
        [false i nil]
        [true i (Integer/parseInt (subs s offset i))]))))

(defn- pc-any [& pcs]
  (fn [offset s]
    (let [pc-res (map #(% offset s) pcs)]
      (if-let [res (->> pc-res
                        (filter (fn [[success _ _]] success))
                        (first))]
        res
        (apply min-key
               (fn [[_ skipahead _]] skipahead)
               pc-res)))))

(defn- pc-expand-dos [forms offset-in-s src-in-s]
  (let [form (first forms)
        offset-s (gensym "offset")
        invocation (list (if (vector? form) (second form) form)
                         offset-in-s
                         src-in-s)
        val-s (if (vector? form) (first form) '_)]
    (if (empty? (rest forms))
      invocation
      `(let [[suc?# ~offset-s ~val-s :as res#] ~invocation]
         (if suc?#
           ~(pc-expand-dos (rest forms) offset-s src-in-s)
           res#)))))

(defmacro ^:private pc-do
  [& forms] ;; no empty forms
  (let [offset-s (gensym "offset")
        src-s (gensym "src")]
    `(fn [~offset-s ~src-s]
       ~(pc-expand-dos forms offset-s src-s))))

(defn- pc-pure [v]
  (fn [offset _] [true offset v]))

(def ^:private pc-cmd-mul
  (pc-do
    (pc-word "mul(")
    [x pc-int]
    (pc-word ",")
    [y pc-int]
    (pc-word ")")
    (pc-pure [:mul x y])))

(def ^:private pc-cmd-do
  (pc-do
    (pc-word "do()")
    (pc-pure :do)))

(def ^:private pc-cmd-donot
  (pc-do
    (pc-word "don't()")
    (pc-pure :donot)))

(def ^:private pc-some-command
  (pc-any pc-cmd-mul pc-cmd-do pc-cmd-donot))

(defn parse-wohoo [file-content]
  (loop [offset 0
         commands (transient [])]
    (if  (>= offset (count file-content))
      (persistent! commands)
      (let [[suc? offset res] (pc-some-command offset file-content)]
        (if suc?
          (recur offset (conj! commands res))
          (recur offset commands))))))

(defn part2 [input]
  (loop [acc 0
         commands input
         enabled true]
    (if (empty? commands)
      acc
      (case (first commands)
        :do (recur acc (rest commands) true)
        :donot (recur acc (rest commands) false)
        (if enabled
          (let [[_ x y] (first commands)]
            (recur (+ acc (* x y)) (rest commands) enabled))
          (recur acc (rest commands) enabled))))))

(defn solve []
  (let [file-content (slurp "inputs/day03.txt")]
    [(part1 (parse-with-regex file-content))
     (part2 (parse-wohoo file-content))]))
