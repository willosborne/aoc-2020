(ns advent-2020.day-3
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def parser
  (insta/parser
   "<S> = (TREE|EMPTY)+
TREE = <'#'>
EMPTY = <'.'>"))

(def column (->> (io/resource "day3.txt")
                 (slurp)
                 (str/split-lines)
                 (map parser)
                 (map #(map first %)))) ;; splice lists out into top level

(defn print-grid [grid]
  (doall
   (for [line grid]
     (println (apply str (map #(case %
                                 :TREE \#
                                 :EMPTY \.) line)))))
  nil)

(defn get-grid-wrapped [grid x y]
  (let [w (count (first grid))
        h (count grid)
        x-index (mod x w)]
    (if (>= y h)
      :out-of-bounds
      (nth (nth grid y) x-index))))

(defn run
  [grid dx dy]
  (defn run-inner [x y num-trees]
    (case (get-grid-wrapped grid x y)
      :out-of-bounds num-trees
      :TREE (recur (+ x dx)
                   (+ y dy)
                   (+ num-trees 1))
      :EMPTY (recur (+ x dx)
                    (+ y dy)
                    num-trees)
      (throw "error")))
  (run-inner 0 0 0))

(def answer-1 (run column 3 1))

(def answer-2 (* (run column 1 1)
                 (run column 3 1)
                 (run column 5 1)
                 (run column 7 1)
                 (run column 1 2)))
