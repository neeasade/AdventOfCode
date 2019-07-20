;; primarily touching this in a repl-buffer sort of way.

(ns aoc.core
  (:gen-class))

(require '[clojure.core :as core])
(require '[clojure.java.shell :as shell])
(require '[clojure.string :as string])

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
            (string/split (get-res "2015/2.txt") #"\n")))

  )


(defn -main [& args] (println "nope"))
