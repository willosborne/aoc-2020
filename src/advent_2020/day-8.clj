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
              (>= new-line-no (count instructions))) ;; terminate successfully
        new-acc
        (recur new-line-no new-acc (conj visited-lines new-line-no))))))

;; part 2

;; modified - returns :loop rather than the accumulator if we detect a loop
(defn run-loop-fail [instructions]
  (loop [line-no 0
         acc 0
         visited-lines #{0}]
    (let [result (run-instruction instructions line-no acc)
          {new-line-no :line
           new-acc :acc} result]
      (if (contains? visited-lines new-line-no)
        :loop
        (if (>= new-line-no (count instructions))
          new-acc ;; terminate
         (recur new-line-no new-acc (conj visited-lines new-line-no)))))))


;; flip nop and jmp at given position
(defn flip [instructions n]
  (let [instruction (get instructions n)
        {operand :operand
         opcode :opcode} instruction]
    (case opcode
      "jmp" (assoc instructions n {:opcode "nop" :operand operand})
      "nop" (assoc instructions n {:opcode "jmp" :operand operand})
      "acc" instructions)))

;; loop through and try every one flipped until we get a result out
(defn find-corruption [instructions]
  (loop [[n & ns] (range (count instructions))]
    (let [result (run-loop-fail (flip instructions n))]
      (if (= result :loop)
        (recur ns)
        result))))
