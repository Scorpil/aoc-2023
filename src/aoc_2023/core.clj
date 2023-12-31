(ns aoc-2023.core
  (:require
   [aoc-2023.day1 :as day1]
   [aoc-2023.day2 :as day2]
   [aoc-2023.day3 :as day3]
   [aoc-2023.day4 :as day4]
   [aoc-2023.day5 :as day5]
   [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def cli-options
  [["-d" "--day DAY" "Day to run"
    :parse-fn #(Integer/parseInt %)]
   ["-p" "--part PART" "Part to run"
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help" "Show help"]])

(defn solution-wrapper
  [f]
  (let [input (slurp *in*)]
    (println (format "Result: %s" (f input)))))

(def solution-fns
  {[1 1] day1/part1
   [1 2] day1/part2
   [2 1] day2/part1
   [2 2] day2/part2
   [3 1] day3/part1
   [3 2] day3/part2
   [4 1] day4/part1
   [4 2] day4/part2
   [5 1] day5/part1
   [5 2] day5/part2})

(defn -main
  [& args]
  (let [opts (:options (parse-opts args cli-options))
        day (:day opts)
        part (:part opts)
        _ (println (format "Day: %d; Part: %d" day part))
        solution-fn (get solution-fns [day part])]
    (if solution-fn
      (solution-wrapper solution-fn)
      (println (format "No solution for day %d part %d" (:day opts) (:part opts))))))
