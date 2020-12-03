;; primarily touching this in a repl-buffer sort of way.
(ns aoc.core
  (:gen-class))

;; get the goodies
(do
  (require '[clojure.core :as core])
  (require '[clojure.math.combinatorics :as comb])
  (require '[clojure.java.shell :as shell])
  (require '[clojure.string :as string])
  (require '[clojure.set :as set])
  (require '[clojure.stacktrace :as st])
  (require '[clojure.java.io :as io])

  (defn get-res [name]
    ;; drop trailing newline, keep string type vs collection of chars
    (apply str (drop-last (slurp (format "./resources/%s" name)))))

  (defn get-input [name]
    "get resources"
    (->> name
         (format "./resources/%s")
         (slurp)
         (string/split-lines)))

  )

;; unmap in userspace (for when you override somethin on accident and )
;; (ns-unmap (find-ns nil) list)

(defn -main [& args]
  (println "nop")
  ;; todo here: run tests
  )

(try
  (-main)
  (catch Exception e
    (st/print-stack-trace e)
    )
  )
