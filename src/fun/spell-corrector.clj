(ns fun.spell-corrector
  "Based on http://norvig.com/spell-correct.html . See also the
implementation by the master himself at http://goo.gl/9xD8r"
  (:use
   clojure.set
   [clojure.java.io :only (as-url)])
  (:import
   javax.swing.JOptionPane))

(defonce wordsdb
  (frequencies
   (re-seq #"[a-z]+"
           (.toLowerCase
            (slurp
             (as-url "http://www.gutenberg.org/files/1661/1661.txt"))))))

(let [alphabet (map char (range 97 123))]
  (defn edits [word]
    (set (map (partial apply str)
              (mapcat (fn [[fst rst]]
                        (concat
                         [(concat fst (drop 1 rst))
                          (concat (drop-last fst) [(first rst) (last fst)] (rest rst))]
                         (mapcat #(vector (concat fst [%] rst)
                                          (concat fst [%] (rest rst))) alphabet)))
                      (map #(split-at % word) (range 1 (inc (count word)))))))))

(defn known [words]  (seq (set (filter wordsdb words))))

(defn edits-2 [word]  (known (mapcat edits (edits word))))

(defn correct [word]
  (first (sort-by (comp - wordsdb)
                  (or (known [word]) (known (edits word))
                      (edits-2 word) [word]))))

(defn query-user []
  (JOptionPane/showInputDialog nil "Enter a word:"))

(defn -main []
  (loop [word (query-user)]
    (if-not (seq word)
      nil
      (do (JOptionPane/showMessageDialog nil (correct word))
          (recur (query-user))))))

