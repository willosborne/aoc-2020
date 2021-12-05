(ns advent-2020.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def input (->> (io/resource "day13.txt")
                (slurp)
                (string/split-lines)))

(def start-time (read-string (first input)))

(def bus-times
  (map read-string (filter #(not (= "x" %)) (string/split (second input) #","))))

(def offsets (map vector bus-times (map (fn [id]
                                          (Math/abs (- (mod start-time id) id)))
                                        bus-times)))

(def answer-1
  (let [[id wait] (apply min-key second offsets)]
    (* id wait)))
