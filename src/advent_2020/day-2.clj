(ns advent-2020.day-2
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def password-policy-parser
  (insta/parser
   "
FILE = (LINE<'\\n'>)*
LINE = REQS<': '>PASS
<REQS> = RANGE<' '>CHAR
<RANGE> = LOWER<'-'>HIGHER
<LOWER> = #'[0-9]+'
<HIGHER> = #'[0-9]+'
<CHAR> = #'[a-z]'
<PASS> = #'[a-z]+'
"))

(defn parse-line [line]
  (let [[_ lower upper target password] line]
    {:lower (edn/read-string lower)
     :upper (edn/read-string upper)
     :target (first target)
     :password password}))

(def data (->> (io/resource "day2.txt")
              (slurp)
              (password-policy-parser)
              (rest)
              (map parse-line)))

(defn count-occurrences [x xs]
  (or (get (frequencies xs) x)
      0))

(defn password-valid-1? [password]
  (let [count (count-occurrences (:target password) (:password password))]
    (and (>= count (:lower password))
         (< count (:upper password)))))

(def answer-1 (count (filter password-valid-1? data)))

(defn password-valid-2? [password]
  (let [target (:target password)
        lower (get (:password password) (- (:lower password) 1))
        upper (get (:password password) (- (:upper password) 1))
        match-lower (= lower target)
        match-upper (= upper target)]
    (or (and match-lower (not match-upper))
        (and (not match-lower) match-upper))))

(def answer-2 (count (filter password-valid-2? data)))
