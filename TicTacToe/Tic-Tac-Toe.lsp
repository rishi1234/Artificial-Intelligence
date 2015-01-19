;; Start 3D game
(defun make-board_3d ()
  (list 'board_3d 0 0 0 0 0 0 0 0 0 0
			   0 0 0 0 0 0 0 0 0 0
			   0 0 0 0 0 0 0))

(defun convert-to-letter_3d (v)
  (cond ((equal v 1) "0")
        ((equal v 10) "X")
        (t " ")))

(defun print-row_3d (x y z)
  (format t "~&  ~A | ~A | ~A"
    (convert-to-letter_3d x)
    (convert-to-letter_3d y)
    (convert-to-letter_3d z)))

(defun print-board_3d (board_3d)
  (format t "~%")
  (print-row_3d
   (nth 1 board_3d) (nth 2 board_3d) (nth 3 board_3d))
  (format t "~& ___________")
  (print-row_3d
    (nth 4 board_3d) (nth 5 board_3d) (nth 6 board_3d))
  (format t "~& ___________")
  (print-row_3d
   (nth 7 board_3d) (nth 8 board_3d) (nth 9 board_3d))
  (format t "~%~%"))
  
(defun print-board_3d-1 (board_3d)
  (format t "~%")
  (print-row_3d
   (nth 10 board_3d) (nth 11 board_3d) (nth 12 board_3d))
  (format t "~& ___________")
  (print-row_3d
    (nth 13 board_3d) (nth 14 board_3d) (nth 15 board_3d))
  (format t "~& ___________")
  (print-row_3d
   (nth 16 board_3d) (nth 17 board_3d) (nth 18 board_3d))
  (format t "~%~%"))
  
(defun print-board_3d-2 (board_3d)
  (format t "~%")
  (print-row_3d
   (nth 19 board_3d) (nth 20 board_3d) (nth 21 board_3d))
  (format t "~& ___________")
  (print-row_3d
    (nth 22 board_3d) (nth 23 board_3d) (nth 24 board_3d))
  (format t "~& ___________")
  (print-row_3d
   (nth 25 board_3d) (nth 26 board_3d) (nth 27 board_3d))
  (format t "~%~%"))


(defun make-move_3d (player pos board_3d)
  (setf (nth pos board_3d) player)
  board_3d)

(setf *computer* 10)
(setf *opponent* 1)
(setf *opponent_1* 10)
(setf *opponent_2* 1)

(setf *triplets_3d*
  '((1 2 3) (4 5 6) (7 8 9)
    (1 4 7) (2 5 8) (3 6 9)
    (1 5 9) (3 5 7)
	(10 11 12)(13 14 15)(16 17 18)
	(10 13 16) (11 14 17) (12 15 18)
	(10 14 18) (12 14 16)
	(19 20 21)(22 23 24)(25 26 27)
	(19 22 25) (20 23 26) (21 24 27)
	(19 23 27) (21 23 25)
	(1 10 19) (4 13 22) (7 16 25)
	(2 11 20) (5 14 23) (8 17 26)
	(3 12 21) (6 15 24)(9 18 27) 
	(3 14 25) (1 14 27) (21 14 7)(19 14 9)
	(1 13 25) (19 13 7) (2 14 26)
	(20 14 8) (3 15 27) (21 15 9)
	(7 17 27) (9 17 25) (4 14 24)(6 14 22)(1 11 21)(3 11 19)
	))

(defun sum-triplet (board_3d triplet)
  (+ (nth (first triplet) board_3d)
     (nth (second triplet) board_3d)
     (nth (third triplet) board_3d)))

(defun compute-sums_3d (board_3d)
  (mapcar #'(lambda (triplet)
              (sum-triplet board_3d triplet))
    *triplets_3d*))

(defun winner-p_3d (board_3d)
  (let ((sums (compute-sums_3d board_3d)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))


(defun opponent-move_3d (board_3d)
  (let* ((pos (read-a-legal-move_3d board_3d))
         (new-board_3d (make-move_3d
                     *opponent*
                     pos
                     board_3d)))
    (print-board_3d new-board_3d)(print-board_3d-1 new-board_3d)(print-board_3d-2 new-board_3d)
    (cond ((winner-p_3d new-board_3d)
           (format t "~&You win!"))
          ((board_3d-full-p new-board_3d)
           (format t "~&Tie game."))
          (t (computer-move_3d new-board_3d)))))

(defun read-a-legal-move_3d (board_3d)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 27)))
           (format t "~&Invalid input.")
           (read-a-legal-move_3d board_3d))
          ((not (zerop (nth pos board_3d)))
           (format t
               "~&That space is already occupied.")
           (read-a-legal-move_3d board_3d))
          (t pos))))

(defun board_3d-full-p (board_3d)
  (not (member 0 board_3d)))

(defun computer-move_3d (board_3d)
  (let* ((best-move_3d (choose-best-move_3d board_3d))
         (pos (first best-move_3d))
         (strategy (second best-move_3d))
         (new-board_3d (make-move_3d
                     *computer* pos board_3d)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board_3d new-board_3d)(print-board_3d-1 new-board_3d)(print-board_3d-2 new-board_3d)
    (cond ((winner-p_3d new-board_3d)
           (format t "~&I win!"))
          ((board_3d-full-p new-board_3d)
           (format t "~&Tie game."))
          (t (opponent-move_3d new-board_3d)))))

(defun choose-best-move_3d (board_3d)
  (random-move_3d-strategy board_3d))

(defun random-move_3d-strategy (board_3d)
  (list (pick-random-empty-position_3d board_3d)
        "random move"))

(defun pick-random-empty-position_3d (board_3d)
  (let ((pos (+ 1 (random 27))))
    (if (zerop (nth pos board_3d))
        pos
      (pick-random-empty-position_3d board_3d))))
          
 (defun play-one-game_3d()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move_3d (make-board_3d))
    (computer-move_3d (make-board_3d))))
;; End 3D game
	
(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  (cond ((equal v 1) "0")
        ((equal v 10) "X")
        (t " ")))

(defun print-row (x y z)
  (format t "~&  ~A | ~A | ~A"
    (convert-to-letter x)
    (convert-to-letter y)
    (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& ___________")
  (print-row
    (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& ___________")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(setf *triplets*
  '((1 2 3) (4 5 6) (7 8 9)
    (1 4 7) (2 5 8) (3 6 9)
    (1 5 9) (3 5 7)))

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
    *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))


(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&You win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (computer-move new-board)))))
		  
;;Expert		  
(defun opponent-move_3 (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&You win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (computer-move_1 new-board)))))		  

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t
               "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))
 
(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move
                     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&I win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move new-board)))))

;;expert		  
(defun computer-move_1 (board)
  (let* ((best-move (choose-best-move_1 board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move
                     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&I win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move_3 new-board)))))		  

(defun choose-best-move (board)
  (random-move-strategy board))
 
;;expert 
(defun choose-best-move_1 (board) 
(or (try-to-win board)
(block-opponent-win board)
(random-move-strategy_1 board)
(random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "random move"))

(defun random-move-strategy_1 (board)
  (list (pick-random-empty-position_1 board)
        "Smart move"))		

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
      (pick-random-empty-position board))))	  

(defun pick-random-empty-position_1 (board)
  (let ((pos (+ 1 4)))
	(if (zerop (nth pos board)) pos
	(pick-random-empty-position board))))	  

;;user vs user 
(defun winner-p_usr (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *opponent_1*) sums)
        (member (* 3 *opponent_2*) sums))))

;;user vs user		
(defun opponent-move_1 (board)
  (let* ((pos (read-a-legal-move_1 board))
         (new-board (make-move
                     *opponent_1*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p_usr new-board)
           (format t "~&Player-1 won the game!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move_2 new-board)))))

;;user vs user		  
(defun opponent-move_2 (board)
  (let* ((pos (read-a-legal-move_2 board))
         (new-board (make-move
                     *opponent_2*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p_usr new-board)
           (format t "~&Player-2 won the game!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move_1 new-board)))))

;;user vs user		  
(defun read-a-legal-move_1 (board)
  (format t "~&Player-1 move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move_1 board))
          ((not (zerop (nth pos board)))
           (format t
               "~&That space is already occupied.")
           (read-a-legal-move_1 board))
          (t pos))))

;;user vs user		  
(defun read-a-legal-move_2 (board)
  (format t "~&Player-2 move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move_2 board))
          ((not (zerop (nth pos board)))
           (format t
               "~&That space is already occupied.")
           (read-a-legal-move_2 board))
          (t pos))))

;; Start Expert 3*3		   
(defun try-to-win (board)
(let ((pos (win-the-game board
(* 2 *computer*))))
(and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
(let ((pos (win-the-game_1 board
(* 2 *opponent*))))
(and pos (list pos "block opponent"))))

(defun win-the-game (board 	value-total)
(let ((triplet (find-if
#'(lambda (x)
(equal (sum-triplet board
x)
value-total))
*triplets*)))
(when triplet
(empty-block board triplet))))

(defun win-the-game_1 (board 	value-total)
(let ((triplet (find-if
#'(lambda (x)
(equal (sum-triplet board
x)
value-total))
*triplets*)))
(when triplet
(empty-block board triplet))))

(defun empty-block (board squares)
(find-if #'(lambda (pos)
(zerop (nth pos board)))
squares))			  
;; End Expert 3*3		  		
 
;;start 4*4 
(defun make-board_4-4 ()
  (list 'board 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter_4-4 (v)
  (cond ((equal v 1) "0")
        ((equal v 10) "X")
        (t " ")))

(defun print-row_4-4 (l m n o)
  (format t "~&  ~A | ~A | ~A | ~A"
    (convert-to-letter_4-4 l)
    (convert-to-letter_4-4 m)
    (convert-to-letter_4-4 n)
    (convert-to-letter_4-4 o)))

(defun print-board_4-4 (board)
  (format t "~%")
  (print-row_4-4
   (nth 1 board) (nth 2 board) (nth 3 board) (nth 4 board))
  (format t "~& ________________")
  (print-row_4-4
   (nth 5 board) (nth 6 board) (nth 7 board) (nth 8 board))
  (format t "~& ________________")
  (print-row_4-4
   (nth 9 board) (nth 10 board) (nth 11 board) (nth 12 board))
  (format t "~& ________________")
  (print-row_4-4
   (nth 13 board) (nth 14 board) (nth 15 board) (nth 16 board))
  (format t "~%~%"))

(defun make-move_4-4 (player pos board)
  (setf (nth pos board) player)
  board)

(setf *computer* 10)
(setf *opponent* 1)

(setf *triplets_4-4*
  '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)
    (1 5 9 13) (2 6 10 14) (3 7 11 15) (4 8 12 16)
    (1 6 11 16) (4 7 10 13)))

(defun sum-triplet_4-4 (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)
     (nth (fourth triplet) board)))

(defun compute-sums_4-4 (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet_4-4 board triplet))
    *triplets_4-4*))

(defun winner-p_4-4 (board)
  (let ((sums (compute-sums_4-4 board)))
    (or (member (* 4 *computer*) sums)
        (member (* 4 *opponent*) sums))))

(defun opponent-move_4-4 (board)
  (let* ((pos (read-a-legal-move_4-4 board))
         (new-board (make-move_4-4
                     *opponent*
                     pos
                     board)))
    (print-board_4-4 new-board)
    (cond ((winner-p_4-4 new-board)
           (format t "~&You win!"))
          ((board-full-p_4-4 new-board)
           (format t "~&Tie game."))
          (t (computer-move_4-4 new-board)))))

;;expert		  
(defun opponent-move_4-4_1 (board)
  (let* ((pos (read-a-legal-move_4-4 board))
         (new-board (make-move_4-4
                     *opponent*
                     pos
                     board)))
    (print-board_4-4 new-board)
    (cond ((winner-p_4-4 new-board)
           (format t "~&You win!"))
          ((board-full-p_4-4 new-board)
           (format t "~&Tie game."))
          (t (computer-move_4-4_1 new-board)))))		  

(defun read-a-legal-move_4-4 (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 16)))
           (format t "~&Invalid input.")
           (read-a-legal-move_4-4 board))
          ((not (zerop (nth pos board)))
           (format t
               "~&That space is already occupied.")
           (read-a-legal-move_4-4 board))
          (t pos))))

(defun board-full-p_4-4 (board)
  (not (member 0 board)))

(defun computer-move_4-4 (board)
  (let* ((best-move (choose-best-move_4-4 board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move_4-4
                     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board_4-4 new-board)
    (cond ((winner-p_4-4 new-board)
           (format t "~&I win!"))
          ((board-full-p_4-4 new-board)
           (format t "~&Tie game."))
          (t (opponent-move_4-4 new-board)))))
		  
;;expert		  
(defun computer-move_4-4_1 (board)
  (let* ((best-move (choose-best-move_4-4_1 board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move_4-4
                     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board_4-4 new-board)
    (cond ((winner-p_4-4 new-board)
           (format t "~&I win!"))
          ((board-full-p_4-4 new-board)
           (format t "~&Tie game."))
          (t (opponent-move_4-4_1 new-board)))))		  

(defun choose-best-move_4-4 (board)
	(random-move-strategy_4-4 board))

(defun random-move-strategy_4-4 (board)
  (list (pick-random-empty-position_4-4 board)
        "random move"))

(defun pick-random-empty-position_4-4 (board)
  (let ((pos (+ 1 (random 16))))
    (if (zerop (nth pos board))
        pos
      (pick-random-empty-position_4-4 board))))

;;expert
(defun choose-best-move_4-4_1 (board)
	(or (try-to-win_4-4 board)
(block-opponent-win_4-4 board)
  (random-move-strategy_4-4 board)))
	
(defun try-to-win_4-4 (board)
(let ((pos (win-the-game_4-4 board
(* 3 *computer*))))
(and pos (list pos "make four in a row"))))

(defun block-opponent-win_4-4 (board)
(let ((pos (win-the-game_4-4 board
(* 3 *opponent*))))
(and pos (list pos "block opponent"))))

(defun win-the-game_4-4 (board 	value-total)
(let ((triplet (find-if
#'(lambda (x)
(equal (sum-triplet_4-4 board
x)
value-total))
*triplets_4-4*)))
(when triplet
(empty-block_4-4 board triplet))))

(defun empty-block_4-4 (board squares)
(find-if #'(lambda (pos)
(zerop (nth pos board)))
squares))		
	
;;end 4*4 
(defun select-choice()
  (if (y-or-n-p "Would you like to go first? ")
      (select-user)
    (select-computer)))		   
	
(defun select-user()
(format t "~&Choose the difficulty level:")
(format t "~&1.) Beginner")
(format t "~&2.) Expert")
(format t "~&I/p:")
(let ((pos (read)))
(cond ((not (and (integerp pos)
                     (<= 1 pos 2)))
           (format t "~&Invalid input.")
		   (select-user))
		   ((equal pos 1) (opponent-move (make-board)))
		   ((equal pos 2) (opponent-move_3 (make-board)))
		   )))
		   
(defun select-computer()
(format t "~&Choose the difficulty level:")
(format t "~&1.) Beginner")
(format t "~&2.) Expert")
(format t "~&I/p:")
(let ((pos (read)))
(cond ((not (and (integerp pos)
                     (<= 1 pos 2)))
           (format t "~&Invalid input.")
		   (select-computer))
		   ((equal pos 1) (computer-move (make-board)))
		   ((equal pos 2) (computer-move_1 (make-board)))
		   )))		   
		   
(defun select-user_1()
(format t "~&Who would like to go first:")
(format t "~&1.) Player-1")
(format t "~&2.) Player-2")
(format t "~&I/p:")
(let ((pos (read)))
(cond ((not (and (integerp pos)
                     (<= 1 pos 2)))
           (format t "~&Invalid input.")
		   (select-user_1))
		   ((equal pos 1) (opponent-move_1 (make-board)))
		   ((equal pos 2) (opponent-move_2 (make-board)))
		   )))			   
         
(defun play-game_4-4()
  (if (y-or-n-p "Would you like to go first? ")
      (play-user)
    (play-computer)))		   
	
(defun play-user()
(format t "~&Choose the difficulty level:")
(format t "~&1.) Beginner")
(format t "~&2.) Expert")
(format t "~&I/p:")
(let ((pos (read)))
(cond ((not (and (integerp pos)
                     (<= 1 pos 2)))
           (format t "~&Invalid input.")
		   (select-user))
		   ((equal pos 1) (opponent-move_4-4 (make-board_4-4)))
		   ((equal pos 2) (opponent-move_4-4_1 (make-board_4-4)))
		   )))
		   
(defun play-computer()
(format t "~&Choose the difficulty level:")
(format t "~&1.) Beginner")
(format t "~&2.) Expert")
(format t "~&I/p:")
(let ((pos (read)))
(cond ((not (and (integerp pos)
                     (<= 1 pos 2)))
           (format t "~&Invalid input.")
		   (select-computer))
		   ((equal pos 1) (computer-move_4-4 (make-board_4-4)))
		   ((equal pos 2) (computer-move_4-4_1 (make-board_4-4)))
		   )))
		   
 (defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
    (computer-move (make-board))))
	(defun start-game()
	(format t "~&MENU:")
	(format t "~&1.) User vs. Computer")
	(format t "~&2.) User vs. User")
	(format t "~&3.) 4*4 Tic-Tac-Toe")
	(format t "~&4.) 3D Tic-Tac-Toe")
	(format t "~&5.) Quit")
	(format t "~&Select the item from the menu:")	
	(let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 5)))
           (format t "~&Invalid input.")
           (start-game))
		   ((equal pos 1) (select-choice))
		   ((equal pos 2) (select-user_1))
		   ((equal pos 3) (play-game_4-4))
		   ((equal pos 4) (play-one-game_3d))
		   ((equal pos 5) "quiting the program")
		   )))
			   		   