(ns aoc-2023.day1 
  (:require [clojure.string :as str]
            [aoc-2023.utils :refer [str-reverse]]))

;; == Part 1 ==
(defn find-first-digit
  [line]
  (some #(if (> (Character/digit % 10) 0) % false) line))

(defn find-calibration-value
  [line]
  (Integer/parseInt (apply str [(find-first-digit line) (find-first-digit (reverse line))])))

(defn part1
  [input]
  (reduce + (map find-calibration-value (str/split input #"\n")))
  )

;; == Part 2 ==
(defn find-calibration-value-incl-words
  [line]
    (let [digit-words {"one" "1"
                       "two" "2"
                       "three" "3"
                       "four" "4"
                       "five" "5"
                       "six" "6"
                       "seven" "7"
                       "eight" "8"
                       "nine" "9"}
          match-as-str-digit (fn [match] (if (= 1 (count match)) match (get digit-words match)))
          pattern-str (str/join "|" (concat (keys digit-words) (vals digit-words)))
          pattern (re-pattern pattern-str)
          left-match (re-find pattern line) 

          reversed-pattern (re-pattern (str-reverse pattern-str))
          right-match (str-reverse (re-find reversed-pattern (str-reverse line))) 
          ]
      (Integer/parseInt (apply str [(match-as-str-digit left-match) (match-as-str-digit right-match)])))
  )

(defn part2
  [input]
  (reduce + (map find-calibration-value-incl-words (str/split input #"\n"))))