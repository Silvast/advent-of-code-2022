(ns advent-of-code.dec-1-2)

(defn make-integer [a]
  (map read-string a))

(->> (slurp "resources/elves.txt")
     (clojure.string/split-lines)
     (partition-by #(= "" %))
     (remove #(empty? (first %)))
     (map #(make-integer %))
     (map #(reduce + %))
     (apply max))


(->> (slurp "resources/elves.txt")
     (clojure.string/split-lines)
     (partition-by #(= "" %))
     (remove #(empty? (first %)))
     (map #(make-integer %))
     (map #(reduce + %))
     (sort)
     (reverse)
     (take 3)
     (reduce +))

;; december 2nd

(def guide (->> (slurp "resources/rock.txt")
                (clojure.string/split-lines)
                (map #(clojure.string/replace % #"\s" ""))))



(defn get-shape [i]
  (let [shape-score [1 2 3]]
    (nth shape-score i)))

(defn get-score [i j]
  (let [score-matrix  [[3 6 0] [0 3 6] [6 0 3]]]
    (-> score-matrix
        (nth i)
        (nth j))))

(defn get-outcome [i j]
  (+ (get-shape j) (get-score i j)))

(defn find-outcome []
  (let [guide (->> (slurp "resources/rock.txt")
                   (clojure.string/split-lines)
                   (map #(clojure.string/replace % #"\s" "")))
        letters {:A 0 :B 1 :C 2 :X 0 :Y 1 :Z 2}]
    (apply + (map #(get-outcome
                    (letters (keyword (str (first %))))
                    (letters (keyword (str (second %))))) guide))))

;;Answer
(find-outcome)

;;Part 2

(defn get-new-score [i j]
  (let [score-matrix  [[3 4 8] [1 5 9] [2 6 7]]]
    (-> score-matrix
        (nth i)
        (nth j))))


(defn find-new-outcome []
  (let [guide (->> (slurp "resources/rock.txt")
                   (clojure.string/split-lines)
                   (map #(clojure.string/replace % #"\s" "")))
        letters {:A 0 :B 1 :C 2 :X 0 :Y 1 :Z 2}]
    (apply + (map #(get-new-score
                    (letters (keyword (str (first %))))
                    (letters (keyword (str (second %))))) guide))))

;;Answer
(find-new-outcome)