(ns connect-four.board)

(def NUM_ROWS 6) 
(def NUM_COLS 7) 

(defn ^:private new-board [player-1-bit-board player-2-bit-board column-bit-board-position]
  {:player-1-bit-board player-1-bit-board 
   :player-2-bit-board player-2-bit-board
   :bit-board-position-for-column column-bit-board-position})

(defn ^:private get-bit-position [row col]
  (+ (* NUM_COLS col) row))

(defn ^:private bit-board-winner? [bit-board]
  (let [major-diagonal (bit-and bit-board (bit-shift-right bit-board 6))   
        horizontal (bit-and bit-board (bit-shift-right bit-board 7)) 
        minor-diagonal (bit-and bit-board (bit-shift-right bit-board 8)) 
        vertical (bit-and bit-board (bit-shift-right bit-board 1))] 
    (not (zero? 
           (bit-or (bit-and major-diagonal (bit-shift-right major-diagonal 12))
                   (bit-and horizontal (bit-shift-right horizontal 14))
                   (bit-and minor-diagonal (bit-shift-right minor-diagonal 16))
                   (bit-and vertical (bit-shift-right vertical 2))))))) 

(defn initial-board []
  (new-board 0 0 [0 7 14 21 28 35 42]))

(defn insert-into-board [board player column]
  (let [new-column-bit-board-position 
        (update-in (board :bit-board-position-for-column) [column] inc)]
    (cond 
      (= player 1)
      (new-board 
        (bit-set 
          (board :player-1-bit-board)
          ((board :bit-board-position-for-column) column))
        (board :player-2-bit-board)
        new-column-bit-board-position)
      (= player 2)
      (new-board 
        (board :player-1-bit-board)
        (bit-set 
          (board :player-2-bit-board)
          ((board :bit-board-position-for-column) column))
        new-column-bit-board-position)
      :else 
      (throw 
        (Exception. (str "Invalid player number " player ". Must be either 1 or 2."))))))

(defn get-winner-of-board [board]
  (cond (bit-board-winner? (board :player-1-bit-board)) 1
        (bit-board-winner? (board :player-2-bit-board)) 2
        :else 0)) 

(defn get-player [board row col]
  (let [bit-position (get-bit-position row col)]
    (cond 
      (bit-test (board :player-1-bit-board) bit-position) 1
      (bit-test (board :player-2-bit-board) bit-position) 2
      :else 0)))

(defn column-full? [board col]
  (>= 
    (mod ((board :bit-board-position-for-column) col) 7) 
    NUM_ROWS)) 

(defn ^:private print-line [] 
  (doall
    (for [col (range NUM_COLS)]
      (print "|---")))
  (println "|"))

(defn ^:private print-columns []
  (doall
    (for [col (range NUM_COLS)]
      (print "|" col "")))
  (println "|"))

(defn draw-board [board]
  (print-line)
  (print-columns)
  (print-line)
  (doall
    (for [row (reverse (range NUM_ROWS))]
      (do (doall 
            (for [col (range NUM_COLS)]
              (print "|" (get-player board row col) "")))
          (println "|"))))
  (print-line))
