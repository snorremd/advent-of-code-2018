(ns no-matter-how-you-slice-it
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [intersection]]
            [clojure.math.combinatorics :refer [cartesian-product combinations]]))


(defn str->claim-vectors
  "Given claims string returns sequence of claims on form:
   [id, offset-left, offset-top, width, height], e.g.
   [101, 5, 10, 20, 17]"
  [claim-str]
  (->> (re-find #"#(\d*) @ (\d*),(\d*): (\d*)x(\d*)"
                claim-str)
       rest
       (map read-string)
       (zipmap [:id :l :t :w :h])))


(defn input->claims!
  "Reads inputs and returns a lacy sequence of claim vectors"
  []
  (as-> (slurp "./3-no-matter-how-you-slice-it/input.txt") s
        (split s #"\n")
        (map str->claim-vectors s)))


(defn coordinates
  [{:keys [l t w h]}]
  (cartesian-product
    (range l (+ l w))
    (range t  (+ t h))))


(defn two-or-more
  "Reads input and calculates number of overlapping square inches"
  []
  (->> (input->claims!)
       (map coordinates)
       (map #(zipmap % (repeat 1)))
       (apply merge-with +)
       (filter #(>= (second %) 2))
       count))


(defn no-overlap?
  [overlapping claim]
  (= 0 (count (intersection overlapping
                            (:coordinates claim)))))


(defn non-overlapping-claim
  "Reads input and finds non-overlapping claim"
  []
  (as-> {} s
        (assoc s :claims  (->> (input->claims!)
                               (map #(assoc % :coordinates (set (coordinates %))))))
        (assoc s :overlap (->> (:claims s)
                               (map :coordinates)
                               (map #(zipmap % (repeat 1)))
                               (apply merge-with +)
                               (filter #(>= (second %) 2))
                               (map first)
                               set))
        (filter (partial no-overlap? (:overlap s))
                (:claims s))
        (map :id s)))


(comment
  (two-or-more)
  (non-overlapping-claim))
