(ns advent-2020.day-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def input (->> (io/resource "day1.txt")
                (slurp)
                (str/split-lines)
                (map edn/read-string)))

(def combos (for [x input
                  y input]
              [x y]))

(defn valid? [arg]
  (let [[x y] arg]
    (= (+ x y) 2020)))

(def valid (filter valid? combos))

(def answer ((first valid)))