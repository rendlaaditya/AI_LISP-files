;; YOUR NAME HERE

;; This program provides the functions necessary to solve Sudoku, however it
;; does not contain coded search functions: DFS-Solve and BFS-Solve. You are 
;; to write the code for those functions. 

;; When running your final program to gather the data for HW questions 3, you 
;; must use the Solve-from-file function provided at the bottom of the file. 
;; You must also use the five provided input files to collect the necessary
;; data. 

; the size of the board
(defparameter board-size 9)

; the number of rows
(defparameter rows 9)

;the number of columns
(defparameter cols 9)

;the number of states expanded
(defvar num-states-expanded)

;the number of states on stack
(defvar num-states-onstack)

; returns the initial board
(defun get-board () 
  '((0 1 0 0 0 9 3 8 0)
    (4 7 2 0 5 0 1 0 9)
    (3 9 0 1 7 6 2 4 5)
    (7 4 6 0 0 1 5 9 8)
    (5 2 1 9 8 0 6 7 3)
    (9 8 0 7 6 5 4 1 2)
    (8 6 7 5 4 0 9 3 1)
    (1 5 4 3 9 7 8 2 6)
    (2 3 9 6 1 8 7 5 4)))


;loads a board from the given file , it expects the board to be in the format given in get-board
(defun get-board-from-file (file)
  (let ((in (open file :if-does-not-exist nil)))
    (when in  (return-from get-board-from-file (read in)))
    (when (not in) (format t "Unable to open file ~A" file))
    )
  )


; print the given board
(defun print-board (board) 
    (dotimes (r board-size)
    (format t "~%+---+---+---+---+---+---+---+---+---+~%|")
    (dotimes (c board-size)
      (format t " ~A |" (get-value board r c))))
  (format t "~%+---+---+---+---+---+---+---+---+---+~%~%"))



;returns the value of the array location row , col does bound checking
(defun get-value (array row col)
  ;check the bounds
  (if (not (and (< row  rows) (< col cols))) NIL (nth col (nth row array)))
  ); end of function

;sets the given position in the array to the given value , is bounds checked
(defun set-value (array row col value)
  ;check the bounds
  (if (not (and (< row  rows) (< col cols))) NIL (setf (nth col (nth row array)) value ))
  ); end of function

;returns a list containing the first empty cell in the format row, column
(defun get-first-empty (board)
  (do
   (
    (i 0 (+ i 1)) ;the outer loop index
    (result NIL) ; the result to be returned
    (cell-found? NIL)) ; control variable to terminate the loops)

    ((or (>= i rows) cell-found?) result) ;return the result if cell-found? is true or we have examined all rows 
    
    ;outer loops main body
    (do
     ((j 0 (+ j 1))) ; the inner loop index
     ((or (>= j cols) cell-found?) ) ; if the cell has  been found or we have examined all columns in the row exit the loop
     
     ; inner loops main body
     (if (= (get-value board i j) 0) (and (setf cell-found? T) (setf result (list i j)))) ; we use the and here so that we can set the cell-found? variable as 
					                                             ; set the result to the proper value, remember if allows only one 
                                                                                     ; S expression in the action part
     ); end of inner do
    ); endof outer do
); end of function


;returns T if the given board is complete else return NIL
(defun is-complete? (board)
  (if (get-first-empty board) NIL T)) ; if there is no cell to fill then the board is complete otherwise not


;remove the numbers which occur in the given row from the array of given numbers
(defun exclude-rownums (board row col possiblenums)
  (do
   ((i 0 (+ i 1))) ; the index
   ((>= i cols)) ; termination test
   (let 
       ((number (get-value board row i)))
     (if (> number 0) (setf (aref possiblenums (- number 1)) 0))
     ); end of let
   ); end of do
  ); end of function


 ;remove the numbers which occur in the given column from the array of given numbers
(defun exclude-colnums (board row col possiblenums)
  (do
   ((i 0 (+ i 1))) ; the index
   ((>= i rows)) ; termination test
   (let 
       ((number (get-value board i col)))
     (if (> number 0) (setf (aref possiblenums (- number 1)) 0))
     ); end of let
   ); end of do
  ); end of function


; remove the numbers which occur in the region from the array of given numbers
(defun exclude-regions (board row col possiblenums)
  (do
   (
    (rowbase (truncate row 3))
    (colbase (truncate col 3))
    (i 0 (1+ i)))
    ((>= i 3))
    (do
     ((j 0 (1+ j)))
     ((>= j 3))
     (let 
	 ((number (get-value board (+ (* 3 rowbase) i) (+ (* 3 colbase) j))))
       (if (> number 0) (setf (aref possiblenums (- number 1)) 0))
       ); end of let
     ); end of inner do
    ); end of outer do
  ); end of function



; return the list of possible numbers that can occupy a given cell
(defun get-possible-numbers (board row col)
  (let (
	(possible-numbers (make-array (list board-size) 
				      :initial-contents '(1 2 3 4 5 6 7 8 9)))
	); end of varibale declaration of let
    (exclude-rownums board row col possible-numbers) ; exclude numbers which occur in the row
    (exclude-colnums board row col possible-numbers) ; exclude numbers which occur in the column
    (exclude-regions board row col possible-numbers) ; exclude numbers which occur in the region
    
    ;debug
    ;(format T "~A" possible-numbers)
    
    ; now return the list of possible numbers
    (do 
     (
      (i 0 (+ i 1)) ; the index
      (result NIL)) ; the return value
     ((>= i board-size) result) ; the termination condition
     
     (if (> (aref possible-numbers i) 0) (setf result (append result (list (aref possible-numbers i)))) );if the given posiiton in the array is not 0 then append  
     ); end of do
    ); end of let
  ); end of function




;returns a list of all possible successor states of a given state, computes the successors by filling in all possible values for the first empty cell
(defun get-successors (board)
  (if (is-complete? board) NIL ; if the given board is complete return NIL else proceed 
    (let* ; note the use of let* as we need the value of row and col in the initialization of values
	(
	 (row (first (get-first-empty board)))
	 (col (second (get-first-empty board)))
	 (values (get-possible-numbers board row col))
	 (ret NIL)
	 (temp NIL)
	 )
      
      (dolist (val values ret)
	      (setf temp (copy-tree board)) 
	      (set-value temp row col val) ; create the board resulting from applying the current value to the existing board
	      (setf ret (append ret (list temp)))
	      ); end of do list
      ); end of let
    );end of if
  ); end of function

(defun repeater (wwww)
	(setq uuu (list 0 0 0 0 0 0 0 0 0 0))
	(setq pppp 0)
	(loop while (< pppp 9)
	do  (setq sssss (nth pppp wwww))
		(setq pppp (+ pppp 1))
		(if (not (equal sssss 0)) (if (< (nth sssss uuu) 1) (setf (nth sssss uuu) 1) (return-from repeater T)))
	)
	(return-from repeater nil)
)

(defun issizer (board)
	(setq lkj (length board))
	(if (not (equal lkj 9)) (return-from issizer nil))
	(loop while (> (length board) 0)
	do	(setf ghj (pop board))
		(if (not (equal 9 (length ghj))) (return-from issizer nil)) 
	)
	(return-from issizer T)
) 

(defun islegal (board)
	(if (not (issizer board)) (return-from islegal nil))
	(setf a (list ))
	(setf a (append board a))
	(setf b (apply #'mapcar #'list board))
	(setf a (append b a))
	(setf c (list (nth 0 (nth 0 board)) (nth 1 (nth 0 board)) (nth 2 (nth 0 board)) (nth 0 (nth 1 board)) (nth 1 (nth 1 board)) (nth 2 (nth 1 board)) (nth 0 (nth 2 board)) (nth 1 (nth 2 board)) (nth 2 (nth 2 board))))
	(setf a (cons c a))
	(setf cc (list (nth 3 (nth 0 board)) (nth 4 (nth 0 board)) (nth 5 (nth 0 board)) (nth 3 (nth 1 board)) (nth 4 (nth 1 board)) (nth 5 (nth 1 board)) (nth 3 (nth 2 board)) (nth 4 (nth 2 board)) (nth 5 (nth 2 board))))
	(setf a (cons cc a))
	(setf ccc (list (nth 6 (nth 0 board)) (nth 7 (nth 0 board)) (nth 8 (nth 0 board)) (nth 6 (nth 1 board)) (nth 7 (nth 1 board)) (nth 8 (nth 1 board)) (nth 6 (nth 2 board)) (nth 7 (nth 2 board)) (nth 8 (nth 2 board))))
	(setf a (cons ccc a))
	(setq c (list (nth 0 (nth 3 board)) (nth 1 (nth 3 board)) (nth 2 (nth 3 board)) (nth 0 (nth 4 board)) (nth 1 (nth 4 board)) (nth 2 (nth 4 board)) (nth 0 (nth 5 board)) (nth 1 (nth 5 board)) (nth 2 (nth 5 board))))
	(setf a (cons c a))
	(setq c (list (nth 3 (nth 3 board)) (nth 4 (nth 3 board)) (nth 5 (nth 3 board)) (nth 3 (nth 4 board)) (nth 4 (nth 4 board)) (nth 5 (nth 4 board)) (nth 3 (nth 5 board)) (nth 4 (nth 5 board)) (nth 5 (nth 5 board))))
	(setf a (cons c a))
	(setq c (list (nth 6 (nth 3 board)) (nth 7 (nth 3 board)) (nth 8 (nth 3 board)) (nth 6 (nth 4 board)) (nth 7 (nth 4 board)) (nth 8 (nth 4 board)) (nth 6 (nth 5 board)) (nth 7 (nth 5 board)) (nth 8 (nth 5 board))))	
	(setf a (cons c a))
	(setq c (list (nth 0 (nth 6 board)) (nth 1 (nth 6 board)) (nth 2 (nth 6 board)) (nth 0 (nth 7 board)) (nth 1 (nth 7 board)) (nth 2 (nth 7 board)) (nth 0 (nth 8 board)) (nth 1 (nth 8 board)) (nth 2 (nth 8 board))))
	(setf a (cons c a))
	(setq c (list (nth 3 (nth 6 board)) (nth 4 (nth 6 board)) (nth 5 (nth 6 board)) (nth 3 (nth 7 board)) (nth 4 (nth 7 board)) (nth 5 (nth 7 board)) (nth 3 (nth 8 board)) (nth 4 (nth 8 board)) (nth 5 (nth 8 board))))
	(setf a (cons c a))
	(setq c (list (nth 6 (nth 6 board)) (nth 7 (nth 6 board)) (nth 8 (nth 6 board)) (nth 6 (nth 7 board)) (nth 7 (nth 7 board)) (nth 8 (nth 7 board)) (nth 6 (nth 8 board)) (nth 7 (nth 8 board)) (nth 8 (nth 8 board))))
	(setf a (cons c a))
	(loop while (> (length a) 0)
	do 	(setf b (pop a))
		(if (repeater b) (return-from islegal nil))
	)
	(return-from islegal T)
)
	 
;solves the given board using DFS
(setf num-states-expanded 0)  ; for no.of states explored
(setf num-states-onstack -1)  ; max for no.of states on stack
(setf dfsta nil) ;this is dfs stack
(setf bfsta nil) ; this is bfs stack

(defun maxi (a b)
(if (>= a b) (return-from maxi a))
(return-from maxi b))

(defun DFS-Solve (board)
	(setf num-states-onstack -1)
	(setf num-states-expanded 0)
	(if (not (islegal board)) (return-from DFS-Solve "Invalid board given"))
	(push board dfsta)
	(setf p 1)
	(loop while (> (length dfsta) 0)
	do  (setf a (pop dfsta))
		(setq p (- p 1))
		(if (is-complete? a) (return-from DFS-Solve a))
		(setq num-states-expanded (+ num-states-expanded 1))
		(setq b (get-successors a))
		(setq dfsta (append b dfsta))
		(setq p (+ p (length b)))
		(setq num-states-onstack (maxi p num-states-onstack))
	)	
	(return-from DFS-Solve "No Solution Found")
)

;solves the given board using BFS

(defun BFS-Solve (board)
;; You are to write this function
	(setf bfsta nil)
	(setf level 0)
	(setf num-states-onstack -1)
	(setf num-states-expanded 0)
	(if (not (islegal board)) (return-from BFS-Solve "Invalid board given"))
	(push board bfsta)
	(setf p 1)
	(push 1 bfsta)
	(loop while (> (length bfsta) 0)
	do  (setf a (pop bfsta))
		(if (equal a 1)
			(progn 
				(setq level (+ level 1))
				(setq bfsta (append bfsta '(1)))
				(if (equal (car bfsta) 1) (return-from BFS-Solve "No Solution Found"))
			)
		(progn
			(setq p (- p 1))
			(if (is-complete? a) (progn (format t "depth : ~5f" (- level 1))(return-from BFS-Solve a)))
			(setq num-states-expanded (+ num-states-expanded 1))
			(setq b (get-successors a))
			(setq bfsta (append bfsta b))
			(setq p (+ p (length b)))
			(setf num-states-onstack (maxi p num-states-onstack))
		)
		)
	)
	(return-from BFS-Solve "No Solution Found")	
  ); end of function
  
     
     
;solves the given board using the given solver
; NOTE: DO NOT USE THIS FUCTION TO OBTAIN THE RESULTS REQUIRED FOR 
; QUESTION 3. USE THE SOLVE-FROM-FILE function WITH THE PROVIDED PUZZLES ONLY
(defun solve-board (board solver)
 (let 
     ((answer (funcall solver board)))
	 (if (equal answer "Invalid board given") (return-from solve-board answer))
	 (if (equal answer "No Solution Found") (return-from solve-board answer))
   (cond (answer  (print-board answer) (format t "Number of states expanded ~A, maximum number of states on stack ~A" num-states-expanded num-states-onstack))
 )
)
)



;solves the first puzzle in the given file
; To use this fuction: (solve-from-file "Filepath/filename.lisp" #'Solver) 
; where Solver is either DFS-Solver or BFS-solver.
(defun solve-from-file (file solver)
  (time (solve-board (get-board-from-file file) solver))
  )
  