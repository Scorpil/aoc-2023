(ns aoc-2023.2d)

(defrecord Point [x y])
(defn new-point [x y] (Point. x y))
(def origin (Point. 0 0))

(defrecord Span [top-left bottom-right])
(defn new-span [top-left bottom-right] (Span. top-left bottom-right))

(defn right [p] (assoc p :x (inc (:x p))))
(defn left [p] (assoc p :x (dec (:x p))))
(defn up [p] (assoc p :y (dec (:y p))))
(defn down [p] (assoc p :y (inc (:y p))))
(defn next-line [p] (Point. 0 (inc (:y p))))
(defn in-span? [p span]
  (let [tl (:top-left span)
        br (:bottom-right span)]
    (and (<= (:x tl) (:x p) (:x br))
         (<= (:y tl) (:y p) (:y br)))))