(ns advent-2020.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input (->> (io/resource "day10.txt")
                (slurp)
                (string/split-lines)
                (map read-string)
                ((fn [in]
                   (concat in [(+ 3 (apply max in))])))))

(def adapters (sort input))

(defn count-differences [input]
  (loop [differences {}
         [j & js] input
         last-j 0]
    (if j
      (recur (update differences (- j last-j) (fn [x] (if x (inc x) 1)))
             js
             j)
      differences)))

(def answer-1 (let [diffs (count-differences adapters)]
                (* (get diffs 1)
                   (get diffs 3))))

;; part 2
(def table
  (atom {0 1
         1 1
         2 2
         3 4}))

(defn compatible [n adapters]
  (filter (fn [x]
            (and (< x n)
                 (<= (- n x) 3)))
          adapters))

(defn calc [n adapters]
  (if-let [ans (get @table n)]
    ans
    (let [compat (compatible n adapters)
          ans (apply + (map #(calc % adapters) compat))]
      (swap! table assoc n ans)
      ans)))

(def answer-2 (calc 175 adapters))

(comment
  ;; base case:
  1 element, 1 combo

  ;; recursive case:
  combo for n:

  )
