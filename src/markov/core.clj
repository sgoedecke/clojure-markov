(ns markov.core
  (:gen-class))

;; Generating a markov dictionary from a string of text
;; e.g. { :first-words ["The" "An"] :last-words ["dog"] :dict { "The" ["quick" "large"] "fox" ["jumped"] ... } }

(defn zip-pairs
  "From a list, generates a list of sequential pairs ([1 2 3] becomes [[1 2] [2 3]])"
  [list]
  (map vector list (next list)))

(defn build-dict
  "Builds the graph of words in a markov dictionary, given an ordered list of words"
  [words]
  (reduce (fn [dict, current-pair]
            (let [current-word (current-pair 0)
                  next-word (current-pair 1)
                  words-after-current (get dict current-word)]
              (if words-after-current
                (update dict current-word conj next-word) 
                (merge dict {current-word [next-word]}))))
          {} (zip-pairs words)))

(defn split-into-words
  [text]
  (clojure.string/split text #" "))

(defn get-first-words
  "From a list of sentences, returns a list of the first words"
  [sentences]
  (map (fn [sentence]
         (first (split-into-words sentence))) sentences))

(defn get-last-words
  "From a list of sentences, returns a list of the last words"
  [sentences]
  (map (fn [sentence]
         (last (split-into-words sentence))) sentences))

(defn markov-generate-dict
  "Builds a markov dict for some text, including a list of first and last words."
  [text]
  (let [sentences (clojure.string/split text #"\. ") ; TODO: handle '?'
        words (mapcat split-into-words sentences)
        first-words (get-first-words sentences)
        last-words (get-last-words sentences)]
    (hash-map :first-words first-words :last-words last-words :dict (build-dict words))))

;; Generating text from a markov dictionary

(defn next-word
  "Gets the next word, given a word and a markov dict."
  [word dict]
  (rand-nth (get (:dict dict) word)))

(defn recursively-construct-sentence
  "Adds a word to an array of words from the markov dict, if possible."
  [sentence-array dict]
  (let [current-word (last sentence-array)]
    (if (.contains (:last-words dict) current-word) ; `contains?` is a well-named method and clojure is a good language
      sentence-array
      (recursively-construct-sentence (conj sentence-array (next-word current-word dict)) dict))))

(defn generate-sentence
  "Generates a sentence from a markov dictionary."
  [dict]
  (let [first-word (rand-nth (:first-words dict))]
    (str 
      (clojure.string/join " " (recursively-construct-sentence [first-word] dict))
      ".")))

(defn generate-paragraph
  "Generates a paragraph from a markov dictionary."
  [num-sentences dict]
  (clojure.string/join " " (map 
                             (fn [n] (generate-sentence dict)) 
                             (range num-sentences))))

;; Sanitizing the text used to generate the markov dictionary

(defn shrink-whitespace
  [text]
  (clojure.string/replace text #"\s" " "))

(defn remove-newlines
  [text]
  (clojure.string/replace text #"\\n" " "))

(defn sanitize-text
  [text]
  (shrink-whitespace
    (remove-newlines text)))

;; Main

(defn -main
  [sample-text num-sentences]
  (generate-paragraph num-sentences (markov-generate-dict (sanitize-text sample-text))))

