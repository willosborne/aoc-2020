(ns advent-2020.day-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def input (->> (io/resource "day1.txt")
                (slurp)
                (str/split-lines)
                (map edn/read-string)))

(def combos (for [x input
                  y input
                  z input]
              [x y z]))

(defn valid? [[x y z]]
  (= (+ x y z) 2020))

(def valid (filter valid? combos))

(def answer (let [[x y z] (first valid)]
              (* x y z)))

(defn func [[a b]]
  (print a b))
