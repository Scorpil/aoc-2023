(ns aoc-2023.core
  (:require
   [aoc-2023.day1 :as day1]
   [aoc-2023.day2 :as day2]
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
   [2 2] day2/part2})

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
