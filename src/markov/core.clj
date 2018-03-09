(ns markov.core
  (:require [clojure.string :as str]))

;; Generating a markov dictionary from a string of text
;; e.g. { :first-words ["The" "An"] :last-words ["dog"] :dict { "The" ["quick" "large"] "fox" ["jumped"] ... } }

(defn build-dict [words]
  (reduce (fn [dict, [current-word next-word]]
            (update-in dict [current-word] conj next-word))
          {} (partition 2 1 words)))

(defn get-first-words [sentences]
  (map (fn [sentence]
         (first (split-into-words sentence))) sentences))

(defn get-last-words [sentences]
  (map (fn [sentence]
         (last (split-into-words sentence))) sentences))

(defn markov-generate-dict [text]
  (let [sentences (str/split text #"\. ") ; TODO: handle '?'
        words (mapcat #(str/split %1 #" ") sentences)]
    {:first-words (get-first-words sentences)
     :last-words (get-last-words sentences)
     :dict (build-dict words)}))

;; Generating text from a markov dictionary

(defn next-word [word dict]
  (rand-nth (get (:dict dict) word)))

(defn recursively-construct-sentence [dict words]
  (let [current-word (last words)]
    (if (.contains (:last-words dict) current-word) ; `contains?` is a well-named method and clojure is a good language
      words 
      (->> (next-word current-word dict)
           (conj words)
           (recursively-construct-sentence dict)))))

(defn generate-sentence [dict]
  (let [first-word (rand-nth (:first-words dict))]
    (str/join " " (recursively-construct-sentence dict [first-word]))))


(defn lazy-generate-text [dict]
  (repeatedly (fn [] (generate-sentence dict))))

(defn sanitize-text
  [text]
  (-> (str/replace text #"\\n" " ")
      (str/replace #"\s+" " ")))

(defn generate-from-sample
  [sample-text num-sentences]
  (->> (sanitize-text sample-text)
       markov-generate-dict
       lazy-generate-text
       (take num-sentences)
       (str/join ". ")))

