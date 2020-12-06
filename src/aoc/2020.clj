
(require '[clojure.string :as string])
(require '[clojure.math.combinatorics :as comb])
(require '[clojure.data :as data])
(require '[clojure.set :as set])

(defn solve-2020-1 [input-file]
  (defn solve-comb [div]
    (let [numbers (read-string (format "[%s]" (slurp "1.txt")))]
      (->> (comb/combinations numbers div)
           (filter #(= 2020 (apply + %)))
           first
           (apply *))))
  [(solve-comb 2) (solve-comb 3)])

(defn solve-2020-2 [input-file]
  [;; 1
   (->> (get-input input-file)
        (filter
         (fn [line]
           (let [[_ lower upper letter pass]
                 (first (re-seq #"([0-9]+)-([0-9]+) ([a-zA-Z]): (.+)" line))
                 lower (Integer/parseInt lower)
                 upper (Integer/parseInt upper)
                 result (count (filter #(= % (first letter)) pass))]
             (<= lower result upper))))
        count)

   ;; 2
   (->> (get-input input-file)
        (filter
         (fn [line]
           (let [[_ lower upper letter pass]
                 (first (re-seq #"([0-9]+)-([0-9]+) ([a-zA-Z]): (.+)" line))
                 lower (dec (Integer/parseInt lower))
                 upper (dec (Integer/parseInt upper))
                 result (count (filter #(= (nth pass %) (first letter)) [lower upper]))]
             (= result 1))))
        count)
   ])

(defn solve-2020-3 [input-file]
  (defn sled [right down]
    (:trees
     (reduce
      (fn [{:keys [w h trees]} line]
        (if (zero? (mod h down))
          {:w (+ right w)
           :h (+ 1 h)
           :trees (if (= \# (nth line (mod w (count line))))
                    (inc trees) trees)}
          {:w w :h (+ h 1) :trees trees}))
      {:w 0 :h 0 :trees 0}
      (get-input input-file))))

  [;; 1
   (sled 3 1)

   ;; 2
   (apply *
          (map #(apply sled %)
               [[1 1] [3 1] [5 1] [7 1] [1 2]]))
   ]
  )

(defn solve-2020-4 [input-file]
  (defn input-to-map []
    ;; the dirtiest part of this one
    (->>
     (string/split (get-res input-file) #"\n\n")

     ;; form:
     (map #(string/replace % #"\n" " "))
     (map #(string/split % #" "))

     ;; now, we have a collection of strings "key:val", so turn them into proper maps (still string-> string):
     (map
      (fn [coll]
        (reduce
         #(let [[key val] (string/split %2 #":")]
            (assoc %1 key val))
         {} coll)))))

  (defn validate [key val]
    (condp = key
      "byr" (<= 1920 (Integer/parseInt val) 2002)
      "iyr" (<= 2010 (Integer/parseInt val) 2020)
      "eyr" (<= 2020 (Integer/parseInt val) 2030)
      "hgt" (when-let [[_ val kind] (first (re-seq #"([0-9]+)(cm|in)" val))]
              (if (= kind "cm")
                (<= 150 (Integer/parseInt val) 193)
                (<= 59 (Integer/parseInt val) 76)))
      "hcl" (re-seq #"\#[0-9a-f]{6}" val)
      "ecl" (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} val)
      "pid" (re-seq #"^[0-9]{9}$" val)
      "cid" true
      nil))

  (let [part-1-coll
        (->> (input-to-map)
             (filter
              (fn [passport-coll]
                (every?
                 #((set (keys passport-coll)) %)
                 ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]
                 ))))]
    [ ;; 1
     (count part-1-coll)

     ;; 2
     (count
      (filter
       (fn [passport-coll]
         (prn passport-coll)
         (every? #(apply validate %) passport-coll))
       part-1-coll))
     ]
    )
  )

(map first



     )

(re-seq #"([BF]+)([RL]+)" (first

                           (get-input "2020/4.txt"))
        )

(->>
 (get-input "2020/4.txt")

 (map
  (fn [line]
    (let [[_ v h] (first (re-seq #"([BF]+)([RL]+)" line))]
      [
       (first
        (reduce
         (fn [acc new]
           (if (= new \F)
             (first (split-at (/ (count acc) 2) acc))
             (second (split-at (/ (count acc) 2) acc))))
         (range 128)
         v))


       (or
        (first
         (reduce
          (fn [acc new]
            (if (= new \L)
              (first (split-at (/ (count acc) 2) acc))
              (second (split-at (/ (count acc) 2) acc))))
          (range 7)
          h))
        0
        )
       ]
      ))
  )

 (map
  (fn [[row col]]
    (+ col (* 8 row))
    )
  )

 (sort >)
 ((fn [all]
    ;; (range (last all) (first all))
    (prn (take 10 (range (last all) (first all))))
    (prn (take 10 (reverse all)))
    (data/diff
     (range (last all) (first all))
     (reverse all)
     )
    )
  )

 ;; (filter #(<= 740 % 750))
 ;; (group-by first)
 ;; (sort-by #(count (second %)) <)
 ;; (data/diff (reverse (range 994)))

 (first)
 ;; (second)

 (filter #(not (nil? %)))
 (filter #(<= 740 % 750))

 )

(->>
 (get-input "2020/4.txt")

 (map
  (fn [line]
    (let [[_ rows cols] (first (re-seq #"([BF]+)([RL]+)" line))
          splitter
          (fn [lower-letter]
            (fn [[acc new]] ((if (= new lower-letter) first second)
                             (split-at (/ (count acc) 2) acc))))
          ]
      [(reduce (splitter \F) (range 128) rows)
       (reduce (splitter \L) (range 7) rows)
       ])))

 (map
  (fn [[row col]]
    (+ col (* 8 row))
    )
  )

 (sort >)
 ((fn [all]
    ;; (range (last all) (first all))
    (prn (take 10 (range (last all) (first all))))
    (prn (take 10 (reverse all)))
    (data/diff
     (range (last all) (first all))
     (reverse all)
     )
    )
  )

 ;; (filter #(<= 740 % 750))
 ;; (group-by first)
 ;; (sort-by #(count (second %)) <)
 ;; (data/diff (reverse (range 994)))

 (first)
 ;; (second)

 (filter #(not (nil? %)))
 (filter #(<= 740 % 750))

 )



(defn solve-2020-6 [input-file]
  [(->>
    (string/split (get-res input-file) #"\n\n")
    (map #(string/replace % "\n" ""))
    (map #(reduce conj #{} %))
    (map count)
    (apply +))

   (->>
    (string/split (get-res input-file) #"\n\n")
    (map #(string/split % #"\n"))
    (map
     (fn [letters]
       (map #(reduce conj #{} %) letters)))
    (map #(apply set/intersection %))
    (map count)
    (apply +))])
