(ns advent-2020.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn match [func pattern]
  (fn [input]
    (->> input
         (re-find pattern)
         (map str)
         (map func))))

(def match-row (match #(case %
                         "F" :left
                         "B" :right
                         (throw (Exception. "bad input")))
                      #"[FB]+"))

(def match-col (match #(case %
                         "L" :left
                         "R" :right
                         (throw (Exception. "bad input")))
                      #"[LR]+"))

(defn midpoint [left right]
  (+ left (int (/ (- right left) 2))))

(comment
  (midpoint 0 127) ;; 63
  )

(defn binary-search [left right sequence]
  (let [[x & xs] sequence]
    ;; (println left " " right " " x)
    (case x
      nil right
      :left (recur left (midpoint left right) xs)
      :right (recur (+ 1 (midpoint left right)) right xs))))

(comment
  (binary-search 0 7 [:right :right :right])
  (binary-search 0 127 [:left :left :left :left :left :left :right])
  (binary-search 0 127 [:right :right :right :right :right :right :right])
  (binary-search 0 127 [:left :right :left :right :right :left :left])
  )

(defn parse-line [line]
  {:row (match-row line)
   :col (match-col line)})

(defn run-line [parsed-line rows cols]
  (let [row (binary-search 0 rows (:row parsed-line))
        col (binary-search 0 cols (:col parsed-line))]
    {:row row
     :col col
     :id (+ (* row 8) col)}))

(def input (->> (io/resource "day5.txt")
                (slurp)
                (str/split-lines)
                (map parse-line)))

(def seats (map #(run-line % 127 7) input))

(def answer-1 (apply max-key :id seats)) ;; hooray

;; part 2
(def max-id (+ (* 127 8) 7))

(defn filled-seats [seats]
  (reduce conj #{} (map :id seats)))

(defn missing-seats [filled-seats]
  (set/difference (set (for [id (range max-id)]
                         id))
                  filled-seats))

(defn filter-front-back [missing-seats]
  (filter (fn [seat]
            (not (or (contains? missing-seats (+ seat 1))
                     (contains? missing-seats (- seat 1)))))
          missing-seats))

(def answer-2 (-> seats
                  filled-seats
                  missing-seats
                  filter-front-back))
