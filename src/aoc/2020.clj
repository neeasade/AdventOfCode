

(defn solve-2020-1 [input-file]
  (defn solve-comb [div]
    (let [numbers (read-string (format "[%s]" (slurp "1.txt")))]
      (->> (comb/combinations numbers div)
           (filter #(= 2020 (apply + %)))
           first
           (apply *))))
  [
   (solve-comb 2)
   (solve-comb 3)
   ])

(defn solve-2020-2 [input-file]
  [
   ;; 1
   (-> (get-input input-file)
       (filter
        (fn [line]
          (let [[_ lower upper letter pass]
                (first (re-seq #"([0-9]+)-([0-9]+) ([a-zA-Z]): (.+)" line))
                lower (dec (Integer/parseInt lower))
                upper (dec (Integer/parseInt upper))
                ;; result (count (filter #(= (nth pass %) (first letter)) [lower upper]))
                result (count (filter #(= % (first letter)) pass))]
            (<= lower result upper))))
       count)

   ;; 2
   (count
    (filter
     (fn [line]
       (let [[_ lower upper letter pass]
             (first (re-seq #"([0-9]+)-([0-9]+) ([a-zA-Z]): (.+)" line))
             lower (dec (Integer/parseInt lower))
             upper (dec (Integer/parseInt upper))
             result (count (filter #(= (nth pass %) (first letter)) [lower upper]))]
         (= result 1)))
     (get-input input-file)))
   ]
  )

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

