(ns advent-of-code.dec-6)

;;Part 1
(defn is-unique? [b i j]
  (= (count (subvec b i j)) (count (distinct (subvec b i j)))))


(defn get-outcome [d]
  (let [a (atom 0)
        s (vec (seq d))]
    (while (and (< @a (count d)) (not (is-unique? s @a (+ @a 4))))
      (swap! a inc))
    (+ @a 4)))


;;Answer
(->> (slurp "resources/markers.txt")
     (get-outcome))


;; Part 2

(defn get-new-outcome [d]
  (let [a (atom 0)
        s (vec (seq d))]
    (while (and (< @a (count d)) (not (is-unique? s @a (+ @a 14))))
      (swap! a inc))
    (+ @a 14)))

;;Answer
(->> (slurp "resources/markers.txt")
     (get-new-outcome))