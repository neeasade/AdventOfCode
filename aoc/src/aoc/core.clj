;; primarily touching this in a repl-buffer sort of way.


;; A little much, I'm trying to practice clojure some more
;; to run: put this in a file, have clojure, run clojure ./<file>.clj

(def valmap {3 "Crackle" 5 "Pop"})

(defn can-divide? [top bottom]
  (= 0 (mod top bottom)))

(doseq [i (range 0 (inc 100))]
  (if (some true? (map #(can-divide? i %)
           (keys valmap)))

    ;; something is divisible, printem'
    (doseq [[divide-check message] valmap]
      (when (can-divide? i divide-check)
        (print message)))

    ;; nothing was divisible
    (print i))
  (println))


(ns aoc.core
  (:gen-class))

(require '[clojure.core :as core])

(require '[clojure.java.shell :as shell])
(require '[clojure.string :as string])

;; (require '[clojure.math.numeric-tower :as math])

(require '[clojure.set :as set])
(require '[clojure.edn :as edn])
(require '[clojure.java.io :as io])

(defn get-res [name]
  ;; drop trailing newline, keep string type vs collection of chars
  (apply str (drop-last (slurp (format "./res/%s" name)))))


;; 2015
(defn solve-2015-1-1 []
  (apply + (map #(if (= % \() 1 -1) (get-res "2015/1.txt"))))

(defn solve-2015-1-2 []
  (def floor 0)
  (def position 0)
  (first
   (filter (fn [x] x)
           (for [c (get-res "2015/1.txt")]
             (do
               (def floor (+ floor (if (= c \() 1 -1)))
               (def position (+ position 1))
               (when (= floor -1) position))))))

(defn solve-2015-2-1 []
  (defn get-area [l w h]
    (+ (* 2 w l)
       (* 2 w h)
       (* 2 l h)
       (first (sort
               (list
                (* l w)
                (* h w)
                (* l h))))))

  (apply + (map
            (fn [x]
              (apply get-area
                     (map #(Integer/parseInt %)
                          (string/split x #"x"))))
            (string/split (get-res "2015/2.txt") #"\n"))))

(defn solve-2015-2-2 []
  (defn get-length [l w h]
    (apply +
           (concat
            (map #(* 2 %)
                 (drop-last
                  (sort [l w h])))
            [(apply * [l w h])])))

  (apply + (map
            (fn [x]
              (apply get-length
                     (map #(Integer/parseInt %)
                          (string/split x #"x"))))
            (string/split (get-res "2015/2.txt") #"\n"))))

(defn solve-2015-3-1 []
  (def x 0)
  (def y 0)
  (def seen [[0 0]])

  (doseq [dir (get-res "2015/3.txt")]
    (case dir
      \v (def y (- y 1))
      \^ (def y (+ y 1))
      \> (def x (+ x 1))
      \< (def x (- x 1)))
    (def seen (conj seen [x y])))

  (count (distinct seen)))

(defn solve-2015-3-2 []
  (defn move [seen dir]
    (let
        [x (first (last seen))
         y (second (last seen))]
      (case dir
        \v (conj seen [x (- y 1)])
        \^ (conj seen [x (+ y 1)])
        \> (conj seen [(+ x 1) y])
        \< (conj seen [(- x 1) y])
        \= seen
        )))

  (defn do-moves [dirs-in]
    (loop [seen [[0 0]]
           dirs dirs-in]

      (if (empty? dirs)
        seen
        (recur
         (move seen (first dirs))
         (drop 1 dirs)
         ))))

  (def pairs
    (partition 2 2 (repeat \= )
               (get-res "2015/3.txt")))

  (->>
   (concat
    (do-moves (map first pairs))
    (do-moves (map second pairs)))
   (distinct)
   (count)))

(defn solve-2015-4-1-2 []
  (import 'java.security.MessageDigest)
  (defn md5 [^String s]
    (let [algorithm (MessageDigest/getInstance "MD5")
          raw (.digest algorithm (.getBytes s))]
      (format "%032x" (BigInteger. 1 raw))))

  (def key "yzbqklnj")

  (loop [i 0]
    (let [current-hash (md5 (apply str (concat key (str i))))]
      ;; part 1: 00000
      ;; part 2: 000000
      (if (string/starts-with? current-hash "00000")
        ;; current-hash
        i
        (recur (+' i 1)))))
  )

(defn solve-2015-5-1 []
  (defn contains-double? [word]
    (some true?
          (map #(string/includes?
                 word
                 (apply str [(char %) (char %)]))
               (range (int \a) (inc (int \z))))))

  (defn is-nice [word]
    (and
     (> (count (filter #(string/includes? "aeiou" (str %)) word)) 2)
     (contains-double? word)
     (not (some
           #(string/includes? word %)
           '("ab" "cd" "pq" "xy")
           ))))

  (->>
   (get-res "2015/5.txt")
   (#(string/split % #"\n"))
   (filter is-nice)
   (count)))

(defn solve-2015-5-2 []
  (defn repeats-one-inbetween [word]
    (some
     #(= (first %) (nth % 2))
     (partition 3 1 word)))

  (defn has-buddy-pairs [word]
    ;; I'm an IDIOT
    ;; re-seq is cool
    ;; self group referencing regexs, the future is 30 years ago
    (re-seq #"(\w{2})\w*\1" word))

  (defn is-nice [word]
    (and
     (has-buddy-pairs word)
     (repeats-one-inbetween word)))

  (->>
   (get-res "2015/5.txt")
   (#(string/split % #"\n"))
   (filter is-nice)
   (count)))

(defn solve-2015-6-1 []
  (defn get-square [corner corner2]
    (let [x1 (first corner)
          y1 (second corner)
          x2 (first corner2)
          y2 (second corner2)]
      (for [x (range x1 (inc x2))
            y (range y1 (inc y2))]
        [x y])))

  ;; (set ())
  (set (concat '(1 2 3) #{1}))

  (loop [board board]
    (if (empty? board)
      "yes"
      (recur (drop 0 board))))

  ;; change board
  ;; [[true false true false]
  ;;  [true false true false]]


  (defn update-board [board x y val]
    (assoc board x
           (assoc (nth board x) y true)))

  (defn do-move [action board corner corner2]
    (case action
      ;; "on" (set (concat board (get-square corner corner2)))
      "on"
      (loop [board board
             moves (get-square corner corner2)]
        (if (empty?)
          )
        )

      "off" (loop [board board
                   remove (get-square corner corner2)]
              (if (empty? remove)
                board
                (recur
                 (disj board (first remove))
                 (drop 1 remove))))

      "toggle" (loop [board board
                      toggle (get-square corner corner2)]
                 (if (empty? toggle)
                   board
                   (recur
                    (let [ftoggle (first toggle)]
                      (if (contains? board ftoggle)
                        (disj board ftoggle)
                        (conj board ftoggle)))
                    (drop 1 toggle)
                    )))
      ))

  (time (do-move "toggle" #{} [0 0] [500 500]))

  (time do-move)

  (loop
      [board #{}
       moves (string/split (get-res "2015/6.txt") #"\n")
       ]
    (let [move (first moves)
          action (first (re-seq #"toggle|off|on" move))
          corner (take 2 (map read-string (re-seq #"\d+" move)))
          corner2 (take-last 2 (map read-string (re-seq #"\d+" move)))]
      (if (empty? moves)
        board
        ;; (recur
        ;;  (do-move action board corner corner2)
        ;;  (drop 1 moves))
        (do-move action board corner corner2)
        ;; board
        )))

  ))

;; unmap in userspace
;; (ns-unmap (find-ns nil) 'count)

(defn -main [& args] (println "nope"))
