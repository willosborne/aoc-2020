(ns advent-2020.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn zip [& args]
  (apply map vector args))

(defn parse-line [line]
  (apply vector (map #(case %
                        \# :filled
                        \. :floor
                        \L :empty)
                     line)))

(def grid (->> (io/resource "day11.txt")
          (slurp)
          (string/split-lines)
          (map parse-line)
          (apply vector)))

;; nifty
(defn print-grid [grid]
  (doall
   (for [line grid]
     (println (apply str (map #(case %
                                 :filled \#
                                 :empty \L
                                 :floor \.) line)))))
  nil)

(defn get-grid [grid x y]
  (get (get grid y) x))


(defn count-adjacent-filled-seats [grid x y]
  (->> [[1 0] [0 1] [-1 0] [0 -1]
        [1 1] [1 -1] [-1 1] [-1 -1]]
       (map (fn [[dx dy]]
              (get-grid grid (+ x dx) (+ y dy))))
       (map #(case %
               :filled 1
               :floor 0
               :empty 0
               nil 0))
       (apply +)))

(defn advance-grid [grid]
  (apply vector
         (map (fn [[y row]]
                (apply vector
                       (map (fn [[x val]]
                              (case val
                                :floor :floor
                                :empty (if (= (count-adjacent-filled-seats grid x y) 0)
                                         :filled
                                         :empty)
                                :filled (if (>= (count-adjacent-filled-seats grid x y) 4)
                                          :empty
                                          :filled))
                              ;; (count-adjacent-filled-seats grid x y)
                              )
                            (zip (range (count row)) row))))
              (zip (range (count grid)) grid))))


(defn run-loop [grid]
  (let [next (advance-grid grid)]
    (if (= grid next)
      (count (filter #(= :filled %) (flatten grid)))
      (recur next))))

(def answer-1 (run-loop grid))


;; part 2
(defn cast-ray [grid x y dx dy]
  (if-let [val (get-grid grid x y)]
    (case val
      :filled 1
      :empty 0
      (recur grid (+ x dx) (+ y dy) dx dy))
    0))

(defn count-visible-filled-seats [grid x y]
  (->> [[1 0] [0 1] [-1 0] [0 -1]
        [1 1] [1 -1] [-1 1] [-1 -1]]
       (map (fn [[dx dy]]
              (cast-ray grid (+ x dx) (+ y dy) dx dy)))
       (apply +)))

(defn advance-grid-2 [grid]
  (apply vector
         (map (fn [[y row]]
                (apply vector
                       (map (fn [[x val]]
                              (case val
                                :floor :floor
                                :empty (if (= (count-visible-filled-seats grid x y) 0)
                                         :filled
                                         :empty)
                                :filled (if (>= (count-visible-filled-seats grid x y) 5)
                                          :empty
                                          :filled)))
                            (zip (range (count row)) row))))
              (zip (range (count grid)) grid))))

(defn run-loop-2 [grid]
  (let [next (advance-grid-2 grid)]
    (if (= grid next)
      (count (filter #(= :filled %) (flatten grid)))
      (recur next))))

(def answer-2 (run-loop-2 grid))



(def test-grid
  [[:filled :floor :empty :floor :filled]
   [:floor :floor :floor :floor :floor]
   [:filled :floor :filled :floor :filled]
   ])

;; visual inspection
(def state (atom grid))

(defn run []
  (swap! state advance-grid-2)
  (print-grid @state))
