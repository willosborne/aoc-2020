(ns advent-2020.day-8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-instruction [line]
  (let [[opcode operand] (str/split line #" ")]
    {:opcode opcode
     :operand (read-string operand)}))

(def input (->> (io/resource "day8.txt")
                (slurp)
                (str/split-lines)
                (map parse-instruction)
                (apply vector)))

(defn run-instruction [instructions line-no acc]
  (let [instruction (get instructions line-no)
        {opcode :opcode
         operand :operand} instruction]
    (case opcode
      "nop" {:line (+ line-no 1) :acc acc}
      "acc" {:line (+ line-no 1) :acc (+ acc operand)}
      "jmp" {:line (+ line-no operand) :acc acc})))


(defn run-loop [instructions]
  (loop [line-no 0
         acc 0
         visited-lines #{0}]
    (let [result (run-instruction instructions line-no acc)
          {new-line-no :line
           new-acc :acc} result]
      (if (or (contains? visited-lines new-line-no)
              (>= new-line-no (count instructions)))
        new-acc
        (recur new-line-no new-acc (conj visited-lines new-line-no))))))
