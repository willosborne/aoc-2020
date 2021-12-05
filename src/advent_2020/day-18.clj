(ns advent-2020.day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as insta]))


(def parser-1 (insta/parser
               "
line = expr op expr
expr = num | binop
binop = <'('> expr op expr <')'>
<op> = <#' +'> '+' <#' +'> | <#' +'> '*' <#' +'>
num = lit
lit = #'[0-9]+'
"))

(def transforms
  {:lit read-string
   :expr identity
   :line vector})

(def input (->> (io/resource "day18.txt")
                (slurp)
                (string/split-lines)
                (map parser-1)
                (map insta/transform transforms)))
