(ns aoc-2023.utils 
  (:require [clojure.string :as str]))

(defn list-contains? [list value]
  (some #(= % value) list))

(defn str-reverse [s]
  (apply str (reverse s)))

(defn is-digit? [c]
  (> (Character/digit c 10) -1))

(defn sum [list]
  (reduce + list))

(defn parse-digit-line-base
  "Parse a line of digits into a list of digits, e.g \"1 2 3\" -> [1 2 3]"
  [type line]
  (map #(type %) (str/split line #"\s+")))

(def parse-digit-line (partial parse-digit-line-base #(Integer/parseInt % 10)))
(def parse-digit-line-long (partial parse-digit-line-base #(Long/valueOf %)))