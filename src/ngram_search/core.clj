(ns ngram-search.core
  (:require [clojure.string :as string]
            [clojure.data :as data])
  (:gen-class))

(def random-names ["Lauretta Staten" "Tova Bjorklund" "Bill Vandam" "Frederick Grogan" "Verla Yeung" "Amberly Ratcliffe" "Eloy Dennard" "Deandre Power" "Barabara Clanton" "Shana Brumm" "Buddy Halloway" "Vivian Pendelton" "Livia Ashworth" "Harrison Hassett" "Adolfo Dearmond" "Shaunda Inouye" "Cayla Ducharme" "Barbera Stjames" "Ima Jordahl" "Mandie Guerrier"])

(defn str->ngrams
  "Breaks up a string into words, and then breaks those words into n-grams"
  [n string]
  (let [words (string/split string #" ")
        ngrams (mapcat (partial partition n) words)]
    (map (partial apply str) ngrams)))

(defn str->attributes
  "Creates an attribute map for a string containing the string and a list of all digrams and trigrams"
  [string]
  {:string   string
   :digrams  (str->ngrams 2 string)
   :trigrams (str->ngrams 3 string)})

(defn similarity
  "Sums the amount of shared n-grams for `a` and `b`"
  [a b]
  (let [[_ _ shared] (data/diff (str->attributes a) (str->attributes b))]
    (apply + (map count (vals shared)))))

(defn search
  "Ranks strings in `coll` based on their similarity to `input`"
  [input coll]
  (let [rank (fn [s] {:string s :matches (similarity input s)})
        matches (->> coll
                     (map rank)
                     (filter #(> (:matches %) 0)))]
    (reverse (sort-by :matches matches))))

;; example (search "zloy bnnard" random-names)     => ({:string "Eloy Dennard", :matches 2})
;;         (search "zdolfo beermond" random-names) => ({:string "Adolfo Dearmond", :matches 11} {:string "Mandie Guerrier", :matches 5})
