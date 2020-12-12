(ns advent-2020.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(defn parse-line [line]
  (let [[dir & value] line]
    {:op (keyword (str dir))
     :value (read-string (apply str value))}))

(def input (->> (io/resource "day12.txt")
                (slurp)
                (string/split-lines)
                (map parse-line)))

(def directions [:N :E :S :W])

(defn turn [delta value old-dir]
  (let [offset (case delta
                 :L -1
                 :R 1)]
    (loop [steps (mod (/ value 90) 4)
           dir old-dir]
      (if (= 0 steps)
        dir ;; out of steps, return
        (recur (- steps 1)
               (get directions
                    (mod (+ (.indexOf directions dir) offset)
                         4)))))))

(defn run-instruction [instruction pos direction]
  (let [{op :op
         value :value} instruction
        [x y] pos]
    (case op
      :N [[x (+ y value)] direction]
      :E [[(+ x value) y] direction]
      :S [[x (- y value)] direction]
      :W [[(- x value) y] direction]
      :F (recur {:op direction :value value} pos direction)
      :L [pos (turn :L value direction)]
      :R [pos (turn :R value direction)])))

(defn run-instructions [instructions]
  (loop [position [0 0]
         direction :E
         [i & is] instructions]
    (println i ", " position ", " direction)
    (if i
      (let [[new-pos new-dir] (run-instruction i position direction)]
        (recur new-pos new-dir is))
      (let [[x y] position]
        (+ (Math/abs x) (Math/abs y))))))

(def answer-1 (run-instructions input))


;; part 2

(defn turn-2 [delta value old-waypoint-offset]
  (loop [steps (mod (/ value 90) 4)
         [wx wy :as offset] old-waypoint-offset]
    (if (= 0 steps)
      offset ;; out of steps, return
      (recur (- steps 1)
             (case delta
               :L [(- wy) wx] ;; swap x and y, negating as appropriate
               :R [wy (- wx)])))))

(defn run-instruction-2 [instruction pos waypoint-offset]
  (let [{op :op
         value :value} instruction
        [x y] pos
        [wx wy] waypoint-offset]
    (case op
      :N [pos [wx (+ wy value)]]
      :E [pos [(+ wx value) wy]]
      :S [pos [wx (- wy value)]]
      :W [pos [(- wx value) wy]]
      :F [[(+ x (* wx value)) (+ y (* wy value))] waypoint-offset]
      :L [pos (turn-2 :L value waypoint-offset)]
      :R [pos (turn-2 :R value waypoint-offset)])))

(defn run-instructions-2 [instructions]
  (loop [position [0 0]
         waypoint-offset [10 1]
         [i & is] instructions]
    (println i ", " position ", " waypoint-offset)
    (if i
      (let [[new-pos new-waypoint] (run-instruction-2 i position waypoint-offset)]
        (recur new-pos new-waypoint is))
      (let [[x y] position]
        (+ (Math/abs x) (Math/abs y))))))

(def answer-2 (run-instructions-2 input))
