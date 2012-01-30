(ns fun.sudoku
  "Based on http://norvig.com/sudoku.html"
  (:import
   [java.io BufferedReader FileReader])
  (:use
   clojure.set
   [clojure.java.io :only (reader as-url)]))

(defonce puzzles
  (map #(apply str (drop-last %))
       (partition 10
                  (line-seq
                   (reader
                    (as-url "http://norvig.com/easy50.txt"))))))

(defonce hard-puzzles
  (with-open [rdr (reader (as-url "http://magictour.free.fr/top95"))]
    (doall
     (map #(.replaceAll % "\\." "0")
          (line-seq rdr)))))

(defn join-nth [n sep coll]
  (apply str
         (flatten
          (interpose sep
                     (partition n coll)))))

(defn display [p]
  (println
   (join-nth 3 (apply str (concat (repeat 21 \-) "\n"))
             (map #(str (join-nth 1 " "
                                  (join-nth 3 "|" %)) "\n")
                  (partition 9 p)))))


(def rows "ABCDEFGHI")
(def cols "123456789")

(defn cross [X Y]
  (set
   (for [x X y Y]
     (keyword (str x y)))))

(def squares
  (sort (cross rows cols)))

(def unitlist
  (concat
   (map #(cross rows [%]) cols)
   (map #(cross [%] cols) rows)
   (for [r ["ABC" "DEF" "GHI"]
         c ["123" "456" "789"]]
     (cross r c))))

(def units
  (apply merge-with concat
         (for [u unitlist, cell u]
           {cell [u]})))

(def peers
  (into {}
   (for [[k vs] units]
     [k (difference (apply union vs) #{k})])))

(declare eliminate)

(defn assign [grid [s d]]
  (reduce eliminate grid
          (map #(vector s %)
               (difference (grid s) #{d}))))

(defn eliminate-1 [grid [s d]]
  (when (seq (grid s))
    (let [vs (grid s)
          grid
          (if-not (=  1 (count vs))
            grid
            (reduce eliminate grid
                    (map #(vector % (first vs)) (peers s))))]
      (reduce (fn [g u]
                (when g
                  (if-let [dplaces (for [su u :when ((grid su) d)] su)]
                    (if (= (count dplaces) 1)
                      (assign g [(first dplaces) d])
                      g))))
              grid (units s)))))

(defn eliminate [grid [s d]]
  (when grid
    (if-not ((grid s) d)
      grid
      (eliminate-1 (assoc grid s (difference (grid s) #{d}))
                   [s d]))))

(defn parse-grid [p]
  (reduce assign
          (zipmap squares (repeat (set cols)))
          (filter #(not= (second %) \0)
                  (map vector squares p))))

(defn complete? [grid]
  (not (some #(> % 1)
             (map count (vals grid)))))

(defn search [grid]
  (when grid
    (if (complete? grid)
      grid
      (first
       (for [[s ds] (sort-by (comp count second)
                             (filter #(> (count (second %)) 1)
                                     grid))
             d ds
             :let [result (search (assign grid [s d]))]
             :when result]
         result)))))

(defn solve [puzzle]
  (apply str
         (mapcat (search (parse-grid puzzle))
                 squares)))

;;try for example: (display (solve (first puzzles)))