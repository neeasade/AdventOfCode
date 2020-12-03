
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
