(ns connect-four.core
  (:use connect-four.board)
  (:use connect-four.minimax))

(defn play-game [board players-turn]
  (draw-board board)
  (println)
  (println "Player" players-turn "turn")
  (println)
  (let [col (if (= players-turn 1)
              (best-col board)
              (bigint (read-line)))
        next-players-turn (if (= players-turn 2) 1 2)]
    (play-game 
      (insert-into-board board players-turn col) 
      next-players-turn)))

(defn -main
  [& args]
  (play-game (initial-board) 1))
