(ns aoc-2023.utils)

(defn list-contains? [list value]
  (some #(= % value) list))

(defn str-reverse [s]
  (apply str (reverse s)))