(ns chronal-abberation
  (:require [clojure.string :refer [split]]))


(defn sum-of-frequencies
  []
  (as-> (slurp "./1-chronal-abberation/input.txt") s
        (split s #"\n")
        (map read-string s)
        (apply + s)))


(defn find-repeated
  []
  (as-> (slurp "./1-chronal-abberation/input.txt") s
        (split s #"\n")
        (map read-string s)
        (repeat s)
        (flatten s)
        (reduce (fn [acc freq]
                  (if ((:seen acc) (:freq acc))
                    (reduced (assoc acc :res (:freq acc)))
                    (-> acc
                        (update :seen #(conj % (:freq acc)))
                        (update :freq #(+ % freq)))))
                {:freq 0 :seen #{} :res nil}
                s)
        (:res s)))


(comment
  (sum-of-frequencies)
  (find-repeated))