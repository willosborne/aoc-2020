(ns advent-2020.day-7
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as sets]))

(def parser
  (insta/parser
   "
file = (rule <'\\n'>)+
rule = colour <' bags contain '> (itemlist | none) <'.'>
none = <'no other bags'>
itemlist = item (<', '> item)*
item = count <' '> colour <' bag'> <#'s?'>
colour = words
<words> = word | word ' ' words
<word> = #'[a-z]+'
count = #'[0-9]+'
"))

(def transforms
  {:count read-string
   :colour str
   :item (fn [count colour]
           {:count count
            :colour colour})
   :itemlist vector
   :rule (fn [colour itemlist]
           {:colour colour
            :items itemlist})
   :file vector
   :none (fn [] [])
   })

(defn parse-bags [input]
  (insta/transform transforms (parser input)))

(def input-bags (->> (io/resource "day7.txt")
                     (slurp)
                     (parse-bags)))

(defn bags-containing-colour [bags target-colour]
  (filter (fn [{items :items}]
            (some #(= target-colour (:colour %))
                  items))
          bags))

(defn colours-containing-colour [bags target-colour]
  (set (map :colour (bags-containing-colour bags target-colour))))

(defn expand-colour-set [bags colour-set]
  (apply sets/union colour-set (map #(colours-containing-colour bags %) colour-set)))

(defn run-loop [bags start-colour]
  (loop [colour-set (colours-containing-colour bags start-colour)
         n 0]
    (let [new-set (expand-colour-set bags colour-set)]
      (if (= (count new-set)
             (count colour-set))
        (do
          (println n)
          new-set)
        (recur new-set (+ 1 n))))))

(def answer-1 (count (run-loop input-bags "shiny gold")))


(def test-case
  (parse-bags "red bags contain 10 blue bag.
blue bags contain 10 green bag.
green bags contain no other bags.
"))
;; 1 + 10 (1 + 10 (1))

;; part 2
(defn get-bag [bags colour]
  (first (filter #(= colour (:colour %)) bags)))

;; this is unimaginably dreadful but it works
(defn count-bags [bags start-colour]
  (loop [bags-so-far 0
         [next-bag & remaining-bags] [start-colour]]
    (if next-bag
      (let [{items :items} (get-bag bags next-bag)
            new-bags (for [{n :count
                            colour :colour} items
                           x (range n)]
                       colour)]
        (println bags-so-far "," (into remaining-bags new-bags))
        (recur (+ bags-so-far 1)
               (into remaining-bags new-bags)))
      bags-so-far)))


(defn build-bag-tree [bags colour]
  (let [{items :items} (get-bag bags colour)]
    (for [{n :count col :colour} items]
      [n (build-bag-tree bags col)])))

(defn count-bag-tree [tree]
  (let [list (map (fn [[n subtree]]
                    (* n (count-bag-tree subtree)))
                  tree)]
    (+ 1 (apply + list))))

(def input-bag-tree (build-bag-tree input-bags "shiny gold"))
(def answer-2 (- (count-bag-tree input-bag-tree) 1))
