(ns aoc-2023.day5
  (:require [clojure.string :as str]
            [aoc-2023.utils :refer [parse-digit-line-long]]))

(defn parse-seeds [line]
  (-> (str/split line #": ") second str/trim parse-digit-line-long))

(defn parse-map [input-lines] 
  (let [
        lines (rest input-lines) ; skip header
        parse-route (fn [line] (-> line str/trim parse-digit-line-long))
        parse-map-inner
        (fn [[line & rest-lines] routes]
            (if (empty? line)
              [routes rest-lines]
              (recur rest-lines (conj routes (parse-route line)))))]
    (parse-map-inner lines [])))

(defn parse-all-maps [lines maps]
  (if (empty? lines)
    maps
    (let [[new-map remaining-lines] (parse-map lines)]
      (parse-all-maps remaining-lines (conj maps new-map)))))

(defn transform [seed map]
  (let [
        eval-route
        (fn [[[dest-range src-range dist] & rest-routes]]
          (if (and (>= seed src-range) (< seed (+ src-range dist)))
            (+ dest-range (- seed src-range))
            (if (empty? rest-routes)
              seed
              (recur rest-routes))
            ))]
    (eval-route map)))

(defn transform-chain [maps seed]
  (reduce transform seed maps))

(defn part1 [input]
  (let [
        lines (str/split input #"\n") 
        seeds (parse-seeds (first lines))
        map-lines (-> lines rest rest)
        maps (parse-all-maps map-lines [])]
    (apply min (map (partial transform-chain maps) seeds))))

(defn part2 [input] 
  (let [lines (str/split input #"\n")]
    ))