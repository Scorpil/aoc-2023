(ns aoc-2023.day2
  (:require [clojure.string :as str]))

(def round (create-struct :red :green :blue))
(def game (create-struct :id :rounds))

(defn parse-round
  "Parse a round strings into a round struct
   e.g. ['3 blue' '4 red'] -> {:red 4, :green 0, :blue 3}"
  [round-str]
  (let [colors-pairs (map #(str/split % #" ") round-str)]
    (reduce
     (fn [round color-pair]
       (let [
             color (second color-pair)
             count (Integer/parseInt (first color-pair))
             ]
         (case color
           "red" (assoc round :red count)
           "green" (assoc round :green count)
           "blue" (assoc round :blue count)))
       )
     (struct round 0 0 0)
     colors-pairs)))

(defn parse-game
  [input-str]
  (let [game-str (str/split input-str #": ")
        game-id (Integer/parseInt (second (str/split (first game-str) #" ")))
        round-strs (map #(str/split % #", ") (str/split (second game-str) #"; "))
        ] 
    (struct game game-id (map parse-round round-strs))))

(defn parse-input
  [input]
  (map parse-game (str/split input #"\n")))

;; == Part 1 ==

(defn within-limits [game limits] 
  (every? 
   (fn [round]
     (and (<= (:red round) (:red limits))
          (<= (:green round) (:green limits))
          (<= (:blue round) (:blue limits))))
   (:rounds game)))

(defn part1
  [input]
  (let [
        games (parse-input input)
        limits (struct round 12 13 14)
        ]
    (reduce
     (fn [total game]
         (if (within-limits game limits)
            (+ total (:id game))
            total))
     0
     games)))

;; == Part 2 ==

(defn minimal-set
  [game]
  (reduce 
   (fn [minimal round]
     (assoc minimal
            :red (max (:red round) (:red minimal))
            :green (max (:green round) (:green minimal))
            :blue (max (:blue round) (:blue minimal))))
    (struct round 0 0 0)
    (:rounds game)
))

(defn calculate-power
  [game]
  (* (:red game) (:green game) (:blue game)))

(defn part2
  [input]
  (let [games (parse-input input)]
    (reduce + (map #(-> % minimal-set calculate-power) games))))
