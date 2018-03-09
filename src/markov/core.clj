(ns markov.core
  (:require [clojure.string :as str]))

;; Generating a markov dictionary from a string of text
;; e.g. { :first-words ["The" "An"] :last-words ["dog"] :dict { "The" ["quick" "large"] "fox" ["jumped"] ... } }

(defn build-dict [words]
  (reduce (fn [dict, [current-word next-word]]
            (update-in dict [current-word] conj next-word))
          {} (partition 2 1 words)))

(defn get-bookend-words [sentences which-end]
  (map (fn [sentence]
         (which-end (str/split sentence #" "))) sentences))

(defn markov-generate-dict [text]
  (let [sentences (str/split text #"\. ") ; TODO: handle '?'
        words (mapcat #(str/split %1 #" ") sentences)]
    {:first-words (get-bookend-words sentences first)
     :last-words (get-bookend-words sentences last)
     :dict (build-dict words)}))

(defn recursively-construct-sentence [dict words]
  (let [current-word (last words)]
    (if (.contains (:last-words dict) current-word)
      words 
      (->> (get (:dict dict) current-word)
           rand-nth
           (conj words)
           (recursively-construct-sentence dict)))))

(defn generate-sentence [dict]
  (let [first-word (rand-nth (:first-words dict))]
    (str/join " " (recursively-construct-sentence dict [first-word]))))

(defn generate-from-sample
  [sample-text num-sentences]
  (->> (str/replace sample-text #"\s+" " ")
       markov-generate-dict
       (partial generate-sentence)
       repeatedly
       (take num-sentences)
       (str/join ". ")))

