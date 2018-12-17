(ns inventory-management-system
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [difference]]
            [clojure.math.combinatorics :refer [combinations]]))


(defn checksum
  []
  (let [freqs  (as-> (slurp "./2-inventory-management-system/input.txt") s
                     (split s #"\n")
                     (map frequencies s))
        twos   (->> freqs
                    (filter #(some #{2} (vals %)))
                    count)
        threes (->> freqs
                    (filter #(some #{3} (vals %)))
                    count)]
    (* twos threes)))


(defn similar []
  (as-> (slurp "./2-inventory-management-system/input.txt") s
        (split s #"\n")
        (combinations s 2)
        (drop-while
           (fn [[s1 s2]]
             (->> (map #(if (= %1 %2) 0 1) s1 s2)
                  (apply +)
                  (not= 1)))
           s)
        (first s)
        (reduce (fn [val [c1 c2]]
                  (if (= c1 c2)
                    (conj val c1)
                    val))
                []
                (map vector
                     (first s)
                     (second s)))
        (apply str s)))

(comment
  (checksum)
  (similar))
