(ns advent-2020.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (io/resource "day9.txt")
                (slurp)
                (str/split-lines)
                (map read-string)))

(defn check-number [prev-25 number]
  (some (fn [[a b]]
          (= number (+ a b)))
        (for [x prev-25
              y prev-25]
          [x y])))

(defn run-loop [input]
  (loop [prev-25 (take 25 input)
         [next-num & rest-nums] (drop 25 input)]
    (if (check-number prev-25 next-num)
      (recur (concat (rest prev-25) [next-num])
             rest-nums)
      next-num)))


(def answer-1 (run-loop input))

;; part 2

;; did NOT enjoy this thank you very much
(defn run-counts [input ans]
  (loop [totals []
         [v & vs] input
         n 0]
    (let [new-totals (map (partial + v) totals)]
      (if-let [answer (first (filter #(= ans (nth new-totals %)) (range n)))]
        (let [lower answer
              upper n
              nums (map (partial nth input) (range lower upper))
              smallest (apply min nums)
              largest (apply max nums)]
          (+ smallest largest))
        (recur (concat new-totals [v]) vs (+ 1 n))))))


