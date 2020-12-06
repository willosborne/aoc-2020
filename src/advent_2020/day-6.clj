(ns advent-2020.day-6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def input (-> (io/resource "day6.txt")
               (slurp)
               (str/split #"\n\n")))

(defn get-all-questions [string]
  (->> string
       (filter #(not (= \newline %)))
       (set)))


(def answer-1 (apply + (map (comp count get-all-questions)
                            input)))


(defn get-common-questions [string]
  (->> string
       (str/split-lines)
       (map set)
       (apply set/intersection)))


(def answer-2 (apply  + (map (comp count get-common-questions)
                             input)))
