(ns aoc-2023.day4
  (:require [clojure.string :as str]
            [aoc-2023.utils :refer [sum]]))

(defn parse-space-separated-numbers [line]
  (map #(Integer/parseInt % 10) (str/split line #"[ ]+")))

(defn parse-line [line]
  (let [
        lotto-str (second (str/split line #":[ ]+"))
        [winning-str selected-str] (str/split lotto-str #" \|[ ]+")]
    {:winning (parse-space-separated-numbers winning-str)
     :selected (parse-space-separated-numbers selected-str)}))

(defn parse-input [input]
  (map parse-line (str/split input #"\n")))

(defn is-winning? [winning number]
  (some #(= % number) winning))

(defn do-score [lotto]
  (let [{:keys [winning selected]} lotto]
    (reduce
     (fn [total number]
       (if (is-winning? winning number)
         (inc total)
         total))
     0
     selected)))

(defn part1 [input]
  (let [lottos (parse-input input) 
        pow2x (fn [x] (int (Math/pow 2 x)))
        ]
    (sum (map #(-> % do-score dec pow2x) lottos))))

(defn part2 [input] 
  (let [lottos (parse-input input)
        reducer 
        (fn [{:keys [modifiers total]} lotto]
          (let [
                filter-expired (partial filter #(> (:longevity %) 0))
                dec-modifiers (partial map #(assoc % :longevity (dec (:longevity %))))
                step (comp filter-expired dec-modifiers)
                instances (-> (map #(:modifier %) modifiers) sum inc)
                score (do-score lotto)
                new-modifiers (if (zero? score)
                                (step modifiers)
                                (conj (step modifiers) {:modifier instances :longevity score}))]
            { :modifiers new-modifiers
                :total (+ total instances)
             }))]
    (:total (reduce reducer {:modifiers [] :total 0} lottos))))