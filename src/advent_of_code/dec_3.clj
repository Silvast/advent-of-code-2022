(ns advent-of-code.dec-3)

(def rucksack (clojure.string/split-lines
               (slurp "resources/rucksack.txt")))

(def first-rucksack (first rucksack))

(defn split-to-two [s]
  (let [half (/ (.length s) 2)
        first-half (subs s 0 half)
        second-half (subs s half (.length s))]
    (list first-half second-half)))


(def pairs (map #(split-to-two %) rucksack))

;; {:a 1 ... :z 26}
;; {:A 27 ... :Z 52}

(def lowercase-prio
  (let [keywords
        (map #(keyword (str %))  (map char (range 97 123)))]
    (zipmap keywords (range 1 27))))

(def uppercase-prio
  (let [keywords
        (map #(keyword (clojure.string/upper-case (str %)))
             (map char (range 97 123)))
        prios {}]
    (zipmap keywords (range 27 53))))

(defn is-uppercase? [s]
  (= 0 (compare s (clojure.string/upper-case s))))

(defn get-priority [s]
  (if (is-uppercase? s)
    (uppercase-prio (keyword s))
    (lowercase-prio (keyword s))))


;;Find the item type that appears in both compartments of each rucksack.

(defn find-matching-chars [s1 s2]
  (distinct (filter (into #{} s1) s2)))

(def comp1 (first (first pairs)))
(def comp2 (second (first pairs)))

(def matching-chars (map #(find-matching-chars (first %) (second %)) pairs))

(first (first matching-chars))

(apply + (map #(get-priority (str (first %))) matching-chars))


;;;;;;;; part 2

(def groups-of-three (partition 3 rucksack))

;; find char in all three
(defn find-matching-chars2 [s1 s2 s3]
  (-> (find-matching-chars s1 s2)
      (find-matching-chars s3)))


(def new-matching-chars
  (map #(find-matching-chars2 (first %) (second %) (nth % 2))
       groups-of-three))

new-matching-chars

(first new-matching-chars)
(apply + (map #(get-priority (str (first %))) new-matching-chars))