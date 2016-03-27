(ns connect-four.minimax
  (:use connect-four.board))

(def MAX_DEPTH 6)

(defn ^:private get-children [board player]
  (filter (comp not nil?)
          (map 
            (fn [column] 
              (if (column-full? board column)
                nil
                {:column column
                 :board (insert-into-board board player column)}))
            (range NUM_COLS))))

(defn ^:private score-board [board depth]
  (let [winner (get-winner-of-board board)]
    (cond (= winner 0) 
          (if (>= depth MAX_DEPTH)
            0
            (let [players-turn (if (even? depth) 2 1)]
              (apply (if (= players-turn 1) max min) 
                     (map (fn [board] (score-board board (inc depth)))
                          (map (fn [child] (child :board)) 
                               (get-children board players-turn))))))
          (= winner 1) 1
          (= winner 2) -1)))

(defn ^:private index-of-max [s]
  (first (reduce (fn [a i]
                   (if (> (i 1) (a 1)) i a))
                 (map-indexed vector s))))

(defn best-col [board]
  ((reduce  
     (fn [best-child child]
       (let [score (score-board (child :board) 0)]
         (if (> score (best-child :score))
           {:score score
            :column (child :column)}
           best-child)))
     {:score -2 :column -1}
     (get-children board 1)) :column))
