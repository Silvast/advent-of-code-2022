(ns advent-of-code.dec-5
  (:require [clojure.core.matrix :as m]))


(defn make-integer [c]
  (map #(Integer/parseInt %) c))

(def a (->> (slurp "resources/directions.txt")
            (clojure.string/split-lines)
            (map #(clojure.string/replace % #"move " ""))
            (map #(clojure.string/replace % #"from " ""))
            (map #(clojure.string/replace % #"to " ""))
            (map #(clojure.string/split % #" "))))

(def directions (map #(make-integer (nth a %))
                     (range 0 (count a) 1)))

(defn stringyfy [s]
  (->> s
       (map #(str %))
       (remove #(= "0" %))))

(def d
  (->> (slurp "resources/crates.txt")
       (clojure.string/split-lines)
       (map #(clojure.string/replace % #"\[" ""))
       (map #(clojure.string/replace % #"\]" ""))
       (map #(clojure.string/replace % #"\s\s\s\s" " 0 "))
       (map #(clojure.string/replace % #"\s" ""))
       (vec)
       (map #(vec (seq %)))
       (vec)
       (m/transpose)
       (map #(stringyfy %))))

;; this is a copy paste from stack overflow :D
(defn update-list [l i x]
  (loop [new-data [] old-list l]
    (if (seq old-list)
      (if (= (count new-data) i)
        (recur (conj new-data x) (rest old-list))
        (recur (conj new-data (first old-list)) (rest old-list)))
      (apply list new-data))))

(defn move [crates amount to from i j]
  (let [a (atom amount)
        eka (atom to)
        toka (atom from)
        crates (atom crates)]
    (while (> @a 0)
      (reset! eka (conj @eka (first @toka)))
      (reset! toka (rest @toka))
      (swap! a dec))
    (reset! crates (update-list @crates i @eka))
    (reset! crates (update-list @crates j @toka))
    @crates))


(defn find-outcome [directions]
  (let [a (atom 0)
        crates (atom d)
        amount (atom (first (nth directions @a)))
        from (atom (dec (second (nth directions @a))))
        to (atom (dec (last (nth directions @a))))]
    (while  (< @a (count directions))
      (reset! amount (first (nth directions @a)))
      (reset! from (dec (second (nth directions @a))))
      (reset! to (dec (last (nth directions @a))))
      (reset! crates (move @crates @amount (nth @crates @to)
                           (nth @crates @from) @to @from))
      (swap! a inc))
    @crates))

;;Answer
(clojure.string/join (map #(first %) (find-outcome directions)))

;;Part 2

(defn move-new [crates amount to from i j]
  (let [a (atom amount)
        eka (atom to)
        toka (atom from)
        crates (atom crates)]
    (do
      (reset! eka (flatten (conj @eka (take amount @toka))))
      (reset! toka (drop amount @toka))
      (swap! a dec))
    (reset! crates (update-list @crates i @eka))
    (reset! crates (update-list @crates j @toka))
    (println @crates)
    @crates))

(defn find-outcome-new [directions]
  (let [a (atom 0)
        crates (atom d)
        amount (atom (first (nth directions @a)))
        from (atom (dec (second (nth directions @a))))
        to (atom (dec (last (nth directions @a))))]
    (while  (< @a (count directions))
      (reset! amount (first (nth directions @a)))
      (reset! from (dec (second (nth directions @a))))
      (reset! to (dec (last (nth directions @a))))
      (reset! crates (move-new @crates @amount (nth @crates @to)
                               (nth @crates @from) @to @from))
      (swap! a inc))
    @crates))

;;Answer
(clojure.string/join (map #(first %) (find-outcome-new directions)))
