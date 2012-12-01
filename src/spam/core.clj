;; This is a text classifier written for the stanford AI-class

(ns spam.core
  (:use [clojure.string :only [split]]
        clojure.tools.trace))



(comment
  ;; Some alternative training sets
  (def training {:spam ["offer is secret", "click secret link", "secret sports link"],
                 :ham  ["play sports today", "went play sports", "secret sports event", "sports is today", "sports costs money"]})

  (def training {:movie ["a perfect world", "my perfect woman", "pretty woman"]
                 :song ["a perfect day", "electric storm", "another rainy day"]}))

;; The very small training set
(def training {:old ["top gun" "shy people" "top hat"]
               :new ["top gear" "gun shy"]})

(defn words-in-sentence
  "Split out the words in a sentence and count them"
  [sentence]
  (count (split sentence #" +")))

(defn words-in-set
  "Total words in the given sentence"
  [sentences]
  (reduce + (map words-in-sentence sentences)))

(defn word-frequency
  "Build up a frequency map for each word"
  [sentences]
  (reduce (fn [dict word] (assoc dict word
                                (inc (get dict word 0))))
          {}          
          (reduce concat (for [sentence sentences] (split sentence #" +")))))

(defn total-unique-words
  "Total number of unique words in 'training'"
  [training]
  (count (distinct (apply concat (for [c (keys training)]
                                   (map key (word-frequency (c training))))))))

(defn total-messages
  "Total number of sentences"
  [training]
  (reduce + (for [[_ sentences] training] (count sentences))))

(defn priors [training type k]
  (/
   (+ (count (training type)) k)
   (+ (total-messages training) (* k (count training)))))

(defn probability
  ([word classification k]
     (let [word-count            (get (word-frequency (training classification)) word 0)
           total-words-for-class (words-in-set (training classification))
           total-words           (total-unique-words training)]
       (println "============================================")
       (println "word frequency " (word-frequency (training classification)))
       (println "word count" word-count)
       (println "total words for " classification " is " total-words-for-class)
       (println "total words" total-words) 
       (println "============================================")
                        
       (/ (+ word-count k)
          (+ total-words-for-class (* k total-words)))))
  ([classification k]
     (let [messages        (count (training classification))
           total-messages  (total-messages training)
           classifications (count training)]
       (/ (+ messages k)
          (+ total-messages (* k classifications))))))

(defn classify [message k]
  (letfn [(prob [classification word]
            (/ (+ (get (word-frequency (training classification)) word 0) k)
               (+ (words-in-set (training classification)) (* k (total-unique-words training)))))]
    (let [words (split message #" +")]
      (for [classification (keys training)]
        (let [numerator (* (reduce * 1 (map (partial prob classification) words))
                           (priors training classification k))
              denom     (reduce + 0 (for [classification (keys training)]
                                      (* (reduce * 1 (map (partial prob classification) words))
                                         (priors training classification k))))]

          [classification (/ numerator denom)])))))




(comment
  (probability :spam 1)
  (probability :ham 1)
  (probability "today" :spam 1)
  (probability "today" :ham 1)

  (probability :movie 1)
  (probability :song 1)
  (probability "perfect" :movie 1)
  (probability "perfect" :song 1)
  (probability "storm" :movie 1)
  (probability "storm" :song 1)
  (classify "perfect storm" 1)
  (classify "today is secret" 1)

  (probability :new 1)
  (probability :old 1)
  (probability "top" :old 1)
  (classify "top" 1)

  ;; simple classification for P(SPAM | "perfect storm")

  (let [prob-movie            1/2
        prob-song             1/2
        prob-perfect-in-movie 2/8
        prob-storm-in-movie   0/8
        prob-perfect-in-song  1/8
        prob-storm-in-song    1/8]
    
    (/ (* prob-movie prob-perfect-in-movie prob-storm-in-movie)
       (+ (* prob-movie prob-perfect-in-movie prob-storm-in-movie)
          (* prob-song prob-perfect-in-song prob-storm-in-song))))
  
  )


