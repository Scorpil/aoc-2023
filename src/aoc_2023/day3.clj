(ns aoc-2023.day3
  (:require [aoc-2023.2d :as td]
            [clojure.string :as str]
            [aoc-2023.utils :refer [is-digit?]]))

(def parse-state (create-struct :part-numbers-map :symbols :current-location :current-part))
(def empty-parse-state
  (struct parse-state [] [] td/origin nil))

(defn state-store-symbol [state location]
  (assoc state :symbols (conj (:symbols state) location)))

(defrecord Part [span number])

(defn part-to-number [part]
  (Integer/parseInt (apply str (:number part))))

(defn is-part-open? [state]
  (not (nil? (:current-part state))))

(defn open-part [state char]
  (let [current-location (:current-location state)
        new-span (td/new-span (-> current-location td/left td/up) (-> current-location td/right td/down))
        ]
    (assoc state :current-part (Part. new-span [char]))))

(defn add-to-part [state char]
  (let [current-location (:current-location state)
        current-part (:current-part state)
        current-span (:span current-part)]
    (assoc state :current-part
           (Part.
                  (td/new-span (:top-left current-span) (-> current-location td/right td/down))
                  (conj (:number current-part) char)))))

(defn close-part [state]
  (assoc state
         :current-part nil
         :part-numbers-map (conj (:part-numbers-map state) (:current-part state))))

(defn iter-digit-char [state char]
  (if (is-part-open? state)
    (add-to-part state char)
    (open-part state char)))

(defn iter-non-digit-char [is-symbol? state char]
  (if (is-symbol? char)
    (state-store-symbol state (:current-location state))
    state))

(defn iter-chars
  [is-symbol? state char]
  (let [current-location (:current-location state)
        processed-state
        (if (is-digit? char)
          (iter-digit-char state char)
          (iter-non-digit-char is-symbol? (if (is-part-open? state) (close-part state) state) char))]
    (assoc processed-state :current-location (td/right current-location))))

(defn aggregate-good-part-numbers [aggregator parts symbol-locations]
  (let [initial-state {
                       :unevaluated_parts parts 
                       :good-parts 0
                       }
        reducer (fn [state symbol-location] 
                  (let [local-initial-state {:unevaluated_parts (:unevaluated_parts state)
                                             :parts []
                                             :match []}
                        find-match (fn [state]
                                     (if (empty? (:unevaluated_parts state))
                                       state
                                       (recur (if (td/in-span? symbol-location (:span (first (:unevaluated_parts state))))
                                                (assoc state
                                                       :match (conj (:match state) (first (:unevaluated_parts state)))
                                                       :unevaluated_parts (rest (:unevaluated_parts state)))
                                                (assoc state
                                                       :parts (conj (:parts state) (first (:unevaluated_parts state)))
                                                       :unevaluated_parts (rest (:unevaluated_parts state)))))))
                        new-state (find-match local-initial-state)]
                    {
                     :unevaluated_parts (:parts new-state)
                     :good-parts (+ (:good-parts state) (aggregator (:match new-state)))
                     }))
        good-parts (:good-parts (reduce reducer initial-state symbol-locations))]
    good-parts))

(defn solve [is-symbol? aggregator input]
  (let [rows (str/split input #"\n")
        iter-rows (fn [state, row]
                    (let [new-state (reduce (partial iter-chars is-symbol?) state row)]
                      (assoc new-state :current-location (td/next-line (:current-location new-state)))
                      )
                    )
        state (reduce iter-rows empty-parse-state rows)
        ]
       (aggregate-good-part-numbers aggregator (:part-numbers-map state) (:symbols state))))

(defn part1 [input]
  (let [is-symbol? (fn [char] (not (= char \.)))
        aggregator (fn [matches] (reduce + (map part-to-number matches)))
        ]
    (solve is-symbol? aggregator input)))

(defn part2 [input]
  (let [is-symbol? (fn [char] (= char \*))
        aggregator (fn [matches] (if (= 2 (count matches)) (reduce * (map part-to-number matches)) 0))
        ]
    (solve is-symbol? aggregator input)))