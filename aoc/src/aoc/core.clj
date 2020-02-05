;; primarily touching this in a repl-buffer sort of way.
(ns aoc.core
  (:gen-class))

;; (require '[joker.core :as core])
;; (require '[joker.string :as string])
;; (require '[joker.set :as set])

;; (require '[joker.java.shell :as shell])
;; (require '[joker.java.io :as io])
;; (require '[joker.edn :as edn])

;; eval this to setup
(do
  (require '[clojure.core :as core])
  (require '[clojure.java.shell :as shell])
  (require '[clojure.string :as string])
  (require '[clojure.set :as set])
  (require '[clojure.stacktrace :as st])
  (require '[clojure.java.io :as io])

  (defn get-res [name]
    ;; drop trailing newline, keep string type vs collection of chars
    (apply str (drop-last (slurp (format "./res/%s" name)))))
  )


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
  (defn md5 [^String s]
    (let [algorithm (java.security.MessageDigest/getInstance "MD5")
          raw (.digest algorithm (.getBytes s))]
      (format "%032x" (BigInteger. 1 raw))))

  (loop [i 0]
    (let [current-hash (md5 (apply str (concat "yzbqklnj" (str i))))]
      ;; part 1: 00000
      ;; part 2: 000000
      (if (string/starts-with? current-hash "00000")
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
    ;; Im an IDIOT
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
    (vec
     (let [x1 (first corner)
           y1 (second corner)
           x2 (first corner2)
           y2 (second corner2)]
       (for [x (range x1 (inc x2))
             y (range y1 (inc y2))]
         [x y]))))

  (defn update-board [board coord change-func]
    (let [y (first coord)
          x (second coord)
          current-val (-> board (nth y) (nth x))]
      (assoc board y
             (assoc (nth board y)
                    x
                    (change-func current-val)))))

  (defn process-moves
    [board moves change-func]
    (loop [board board
           moves moves]
      (if (empty? moves)
        board
        (recur (update-board board (first moves) change-func)
               (drop 1 moves)))))

  (defn do-instruction [action board coords]
    (case action
      "on" (process-moves board coords (constantly true))
      "off" (process-moves board coords (constantly false))
      "toggle" (process-moves board coords (fn [old-val] (not old-val)))
      ))

  (count
   (filter
    true?
    (flatten
     (loop [board (mapv vec (repeat 1000 (repeat 1000 false)))
            moves (string/split (get-res "2015/6.txt") #"\n")]

       (if (empty? moves)
         board
         (let [move (first moves)
               action (first (re-seq #"toggle|off|on" move))
               corner (take 2 (map read-string (re-seq #"\d+" move)))
               corner2 (take-last 2 (map read-string (re-seq #"\d+" move)))]
           (recur (do-instruction action board (get-square corner corner2))
                  (drop 1 moves))
           )))))))

(defn solve-2015-6-2 []
  (defn get-square [corner corner2]
    (vec
     (let [x1 (first corner)
           y1 (second corner)
           x2 (first corner2)
           y2 (second corner2)]
       (for [x (range x1 (inc x2))
             y (range y1 (inc y2))]
         [x y]))))

  (defn update-board [board coord change-func]
    (let [y (first coord)
          x (second coord)
          current-val (-> board (nth y) (nth x))]
      (assoc board y
             (assoc (nth board y)
                    x
                    (change-func current-val)))))

  (defn process-moves
    [board moves change-func]
    (loop [board board
           moves moves]
      (if (empty? moves)
        board
        (recur (update-board board (first moves) change-func)
               (drop 1 moves)))))

  (defn do-instruction [action board coords]
    (case action
      "on" (process-moves board coords (fn [old-val] (+ 1 old-val)))
      "off" (process-moves board coords (fn [old-val]
                                          (if (> old-val 0)
                                            (- old-val 1)
                                            0
                                            )
                                          ))
      "toggle" (process-moves board coords (fn [old-val] (+ 2 old-val)))
      ))

  (apply +
         (flatten
          (loop [board (mapv vec (repeat 1000 (repeat 1000 0)))
                 moves (string/split (get-res "2015/6.txt") #"\n")]

            (if (empty? moves)
              board
              (let [move (first moves)
                    action (first (re-seq #"toggle|off|on" move))
                    corner (take 2 (map read-string (re-seq #"\d+" move)))
                    corner2 (take-last 2 (map read-string (re-seq #"\d+" move)))]
                (recur (do-instruction action board (get-square corner corner2))
                       (drop 1 moves))
                ))))))

;; todo: you never finished this one
#_(defn solve-2015-7-1 []
    ;; 123 -> x means that the signal 123 is provided to wire x.
    ;; x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
    ;; x OR y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
    ;; p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q.
    ;; p RSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q.
    ;; NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f.

    ;; always val -> dest
    ;; get val, then put in dest

    (def state {})

    (defn do-move [state line]
      (println line)
      (let
          [move (re-find #"LSHIFT|OR|RSHIFT|AND|NOT|\-\>" line)

           left_ident
           (last (re-find (re-pattern (apply str (concat "(.*) " move))) line))

           right_ident
           (last (re-find (re-pattern (apply str (concat move " (.*) ->"))) line))

           left_value_fns
           (when left_ident
             (if (number? (read-string left_ident))
               (read-string left_ident)
               ((keyword left_ident) state)
               )
             )

           right_value_fns
           (when right_ident
             (if (number? (read-string right_ident))
               (read-string right_ident)
               ((keyword right_ident) state))
             )

           destination
           (last (re-find (re-pattern #" -> (.*)") line))
           ]

        (case move
          "LSHIFT" (assoc state (keyword destination) (bit-shift-left left_value right_value))
          "RSHIFT" (assoc state (keyword destination) (bit-shift-right left_value right_value))
          "AND" (assoc state (keyword destination) (bit-and left_value right_value))
          "OR" (assoc state (keyword destination) (bit-or left_value right_value))
          "->" (assoc state (keyword destination) left_value)
          "NOT" (assoc state (keyword destination) (bit-not right_value))
          )
        ))

    (:a
     (loop
         [state {}
          moves (string/split (get-res "2015/7.txt") #"\n")
          ]
       (if (empty? moves)
         state
         (recur
          (do-move state (first moves))
          (drop 1 moves)
          ))))

    (assoc state (keyword "a") 1)

    (:a (assoc state (keyword "a") 1))

    ;; forms
    )


(defn solve-2019-1-1 []
  (defn mass-to-fuel [mass]
    (-> mass
        (/ 3)
        (Math/floor)
        (int)
        (- 2)))

  (apply + (mapv #(mass-to-fuel (Integer/parseInt %))
                 (string/split (get-res "2019/1.txt") #"\n"))))

(defn solve-2019-1-2 []
  (solve-2019-1-1)

  (defn mass-to-full-fuel [mass]
    (loop [total 0
           current mass]
      (let [new-fuel (mass-to-fuel current)]
        (println (format "%s" new-fuel))
        (if (<= new-fuel 0)
          total
          (recur
           (+ total new-fuel)
           new-fuel
           )))))

  (apply + (mapv #(mass-to-full-fuel (Integer/parseInt %))
                 (string/split (get-res "2019/1.txt") #"\n"))))

(defn do-fix [noun verb]
  (loop [state
         (-> (as-> (get-res "2019/2.txt") v
               (string/split v #",")
               (mapv #(Integer/parseInt %) v))
             (assoc 1 noun)
             (assoc 2 verb))
         position 0]
    (let [current-op (nth state position)
          ;; delay evaluation because we might be peeking past state length wrt position
          arg1 #(nth state (nth state (+ position 1)))
          arg2 #(nth state (nth state (+ position 2)))
          dest #(nth state (+ position 3))

          new-state (case current-op
                      1 (assoc state (dest) (+ (arg1) (arg2)))
                      2 (assoc state (dest) (* (arg1) (arg2)))
                      state
                      )]
      (if (= current-op 99)
        (nth state 0)
        (recur new-state (+ position 4))))
    ))

(defn solve-2019-2-2 []
  ;; (solve-2019-2-1)

  (loop [noun 0
         verb 0]
    (if (= (do-fix noun verb) 19690720)
      (+ (* 100 noun) verb)
      (recur
       (if (= verb 99) (+ 1 noun) noun)
       (if (= verb 99) 0 (+ 1 verb)))))
  )

(defn solve-2019-3-1 []
  ;; maintain 2 collections of points representing lines
  ;; then find all intersections
  ;; then find all intersection distances and sort
  (defn line-input-to-points [moves]
    (loop [moves moves
           points []
           x 0
           y 0]
      (prn moves)
      (if (empty? moves)
        points
        (let [move (first moves)
              move-action (first move)
              move-distance (Integer/parseInt (apply str (drop 1 move)))
              new-points (case move-action
                           \U (mapv #(vec [x (+ y %)]) (range move-distance))
                           \D (mapv #(vec [x (- y %)]) (range move-distance))
                           \R (mapv #(vec [(+ x %) y]) (range move-distance))
                           \L (mapv #(vec [(- x %) y]) (range move-distance)))
              new-y (case move-action
                      \U (+ y move-distance)
                      \D (- y move-distance)
                      y)

              new-x (case move-action
                      \R (+ x move-distance)
                      \L (- x move-distance)
                      x)
              ]
          (recur
           (drop 1 moves)
           (concat points new-points)
           new-x
           new-y
           )))))


  (let [
        line1-points (line-input-to-points (string/split (nth (string/split (get-res "2019/3.txt") #"\n") 0) #","))
        line2-points (line-input-to-points (string/split (nth (string/split (get-res "2019/3.txt") #"\n") 1) #","))

        ;; line1-points (line-input-to-points ["R8" "U5" "L5" "D3"])
        ;; line2-points (line-input-to-points ["U7" "R6" "D4" "L4"])
        intersections (set/intersection (set line2-points) (set line1-points))
        ]
    ;; the first is the [0 0] erroneously placed by our point aggregation
    (second
     (sort <
           (mapv
            #(+ (Math/abs (first %)) (Math/abs (second %)))
            intersections
            )))
    ))

(defn solve-2019-4-1 []
  (defn is-match [n]
    (and
     ;; (and (> n start) (< n end))
     (and (> n 99999) (< n 1000000))
     (some #(= (first %) (second %))
           (partition 2 1 (str n)))
     (= (sort < (mapv #(Integer/parseInt (str %)) (str n)))
        (mapv #(Integer/parseInt (str %)) (str n)))))

  (count (filter is-match (range 206938 679128))))

(defn solve-2019-4-2 []
  (defn is-match [n]
    (and
     (->> n
          str
          (re-seq #"(.)\1{1,}") ;; one or more
          (map first)
          (some #(= (count %) 2)))

     (= (sort < (mapv #(Integer/parseInt (str %)) (str n)))
        (mapv #(Integer/parseInt (str %)) (str n)))))

  (count (filter is-match (range 206938 679128))))

(defn solve-2019-5-1 []
  (defn get-param-mode [n input]
    (->>
     (string/replace (format "%5s" input)
                     " " "0")
     (reverse)
     (drop 2)
     (#(nth % n))
     (list)
     (apply str)
     (Integer/parseInt)
     ))

  (loop [state
         (-> (as-> (get-res "2019/5.txt") v
               (string/split v #",")
               (mapv #(Integer/parseInt %) v)))

         position 0
         input [1]
         ]

    (let [full-op (nth state position)
          current-op
          (Integer/parseInt
           (apply str
                  (take-last 2 (format "%s" full-op)))
           )

          ;; (nth state position)

          ;; (#(do (list % %)) 1)

          lookup-arg-val #(if (= (get-param-mode % full-op) 0)
                            ;; position mode
                            (nth state (nth state (+ % position)))
                            ;; immediate mode (assume param mode is 1)
                            (nth state (+ % position))
                            ;; (+ 1 position)
                            )

          arg1 #(lookup-arg-val 1)
          arg2 #(lookup-arg-val 2)

          arg3 #(if (= (get-param-mode % current-op) 0)
                  (nth state (+ 1 % position))
                  (+ 1 % position)
                  )

          op-length
          (case current-op
            1 4
            2 4
            3 2
            4 2)

          new-state (case current-op
                      1 (assoc state (arg3 2) (+ (arg1) (arg2)))
                      2 (assoc state (arg3 2) (* (arg1) (arg2)))
                      3 (assoc state (arg3 0)
                               (take 1 input))
                      4 ;;(assoc state
                      ;; output
                      (println (arg1))
                      ;; )
                      state
                      )]
      (if (= current-op 99)
        (nth state 0)
        (recur
         new-state
         (+ position op-length)
         (if (= current-op 3)
           (drop 1 input)
           input)
         )))
    )

  ;; [1 2 3]
  ;; (get-param-mode)
  ;; (string/split (get-res "2019/5.txt") #",")

  )

;; unmap in userspace (for when you override somethin on accident and )
;; (ns-unmap (find-ns nil) list)

(defn -main [& args]
  (println "arst")
  (solve-2019-5-1)
  )

(try
  (-main)
  (catch Exception e
    (st/print-stack-trace e)
    )
  )
