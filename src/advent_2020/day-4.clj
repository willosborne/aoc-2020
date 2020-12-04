(ns advent-2020.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]))

(def parser (insta/parser
              "
file = passport (<'\\n\\n'> passport)* <'\\n'>
passport = field (<whitespace> field)*
whitespace = #'[ \\n]'
field = key <':'> value
key = #'[a-z]+'
value = #'[a-z0-9#]+'"
              ))

(def transform-options
  {:key identity
   :value identity
   :field (fn [key val]
            {:key key :value val})
   :passport (comp vec list)
   :file (comp vec list)
   })

(def data (->> (io/resource "day4.txt")
               (slurp)
               (parser)
               (insta/transform transform-options)))

(def required-fields ["byr"
                      "iyr"
                      "eyr"
                      "hgt"
                      "hcl"
                      "ecl"
                      "pid"])

(defn passport-contains-key [pass key]
  (some #(= key %)(map :key pass)))

(defn passport-valid-1
  [passport]
  (every? #(passport-contains-key passport %) required-fields))

(def answer-1 (count (filter passport-valid-1 data)))

(defn check-date [min max]
  (fn [value]
    (and (= (count value) 4)
         (let [num (read-string value)]
           (and (>= num min)
                (<= num max))))))

(comment
  (def sample (check-date 1920 2002))
  (sample "12")
  (sample "1234")
  (sample "1920")
  (sample "2002")
  (sample "2003"))

(def height-parser
  (insta/parser "height = val ('cm'|'in')
val = #'[0-9]+'"))

(def height-transform {:height (comp vec list)
                       :val read-string})

(defn check-height [input]
  (try
    (let [[height unit] (->> input
                             (height-parser)
                             (insta/transform height-transform))]
      (or (and (= unit "cm")
               (>= height 150)
               (<= height 193))
          (and (= unit "in")
               (>= height 59)
               (<= height 76))))
    (catch Exception e false)))

(comment
  (check-height "40in")
  (check-height "asdfa") ;; error
  (check-height "59in")
  (check-height "76in")
  (check-height "88in")
  (check-height "20cm")
  (check-height "150cm")
  (check-height "193cm")
  (check-height "200cm")
  )

(defn check-hair [hair]
  (re-matches #"#[0-9a-f]{6}" hair))

(comment
  (check-hair "#ffffff")
  (check-hair "ffffff")
  (check-hair "#gggggg")
  (check-hair "#aaa0000")
  )

(def valid-eyes ["amb"
                 "blu"
                 "brn"
                 "gry"
                 "grn"
                 "hzl"
                 "oth"])
(defn check-eye [eye]
  (some #(= eye %) valid-eyes))

(defn check-pid [pid]
  (re-matches #"[0-9]{9}" pid))

(def key-preds
  {"byr" (check-date 1920 2002)
   "iyr" (check-date 2010 2020)
   "eyr" (check-date 2020 2030)
   "hgt" check-height
   "hcl" check-hair
   "ecl" check-eye
   "pid" check-pid})

(defn passport-valid-2 [passport]
  (and (every? (fn [{key :key
                     value :value}]
                 (let [pred (get key-preds key)]
                   (if pred
                     (pred value)
                     true)))
               passport)
       (passport-valid-1 passport)))

(def answer-2 (count (filter passport-valid-2 data)))
