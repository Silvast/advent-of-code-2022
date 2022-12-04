(ns advent-of-code.dec-4)

(def cleanup (clojure.string/split-lines
              (slurp "resources/cleanup.txt")))

(defn get-range [pair] (vec (map #(Integer/parseInt %)
                                 (clojure.string/split pair #"-"))))


(defn fully-contains? [pair]
  (let [splitted-pair (clojure.string/split pair #",")
        first-range (get-range (first splitted-pair))
        second-range (get-range (second splitted-pair))]
    (cond
      (= (first first-range) (first second-range))
      true
      (> (first first-range) (first second-range))
      (<= (second first-range) (second second-range))
      (< (first first-range) (first second-range))
      (<= (second second-range) (second first-range)))))


(defn get-contains [data]
  (map #(fully-contains? %) data))

;;Answer
(count (filter true? (get-contains cleanup)))

;;part 2

(defn partly-overlap? [pair]
  (let [splitted-pair (clojure.string/split pair #",")
        first-range (get-range (first splitted-pair))
        second-range (get-range (second splitted-pair))
        a (first first-range)
        b (second first-range)
        c (first second-range)
        d (second second-range)]

    (some true? (map #(.contains (range a (inc b)) %) (range c (inc d))))))

(defn partly-contains [data]
  (map #(partly-overlap? %) data))

;;Answer
(count (filter true? (partly-contains cleanup)))
